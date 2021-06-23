CREATE TYPE price AS (
    value numeric(16, 2),
    currencyid int
);
CREATE TABLE internal_payments (
  paymid serial primary key,
  paymamount price not null
);
insert into internal_payments VALUES
  (1, '(50000.00,0)'),
  (2, '(26000.00,0)'),
  (3, '(27000.00,0)'),
  (4, '(8800.00,0)'),
  (5, '(10000.00,0)');
