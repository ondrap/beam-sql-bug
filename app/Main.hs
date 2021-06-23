{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Int (Int32)
import Database.PostgreSQL.Simple
import Data.Foldable (for_)
import Database.Beam
import Database.Beam.Postgres
import Data.Decimal
import           Database.PostgreSQL.Simple.FromField
import qualified Data.Attoparsec.ByteString.Char8 as AT
import qualified Data.ByteString.Char8 as B8

data Price = Price {
    pValue :: Decimal
  , pCurrencyId :: Int32
} deriving (Show, Eq)

instance FromBackendRow Postgres Price
instance FromField Price where
  fromField f bsval =
    case bsval of
      Nothing -> err UnexpectedNull "Null?"
      Just dval -> do
        tn <- typename f
        case tn of
          "price" ->
            case AT.parseOnly parser dval of
              Left msg -> err ConversionFailed msg
              Right val -> return val
          _ -> err Incompatible "Incompatible type"
    where
      parser = do
        _ <- AT.char '('
        amount <- AT.rational
        _ <- AT.char ','
        curid <- AT.decimal
        _ <- AT.char ')'
        return $ Price amount curid

      err errC msg = do
        typnam <- typename f
        conversionError $ errC (B8.unpack typnam)
                    (tableOid f)
                    (maybe "Name" B8.unpack (name f))
                    "price"
                    msg

data InternalPaymentT f = InternalPayment {
    ipPaymid :: C f Int32
  , ipPaymamount :: C f Price
} deriving (Generic, Beamable)
type InternalPayment = InternalPaymentT Identity
deriving instance Show InternalPayment
instance Table InternalPaymentT where
  data PrimaryKey InternalPaymentT f = InternalPaymentId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = InternalPaymentId . ipPaymid

data TrackingDb f = TrackingDb {
    _dbInternalPayments :: f (TableEntity InternalPaymentT)
} deriving (Generic, Database be)

trackingDb :: DatabaseSettings Postgres TrackingDb
trackingDb = defaultDbSettings


main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname=test"
  pmlist <- runBeamPostgres conn $ do
     runSelectReturningList $ select $ do
                all_ (_dbInternalPayments trackingDb)
  for_ pmlist $ \payment -> do
    liftIO $ print payment
