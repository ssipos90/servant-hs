CREATE TABLE IF NOT EXISTS users
( userId INT GENERATED BY DEFAULT AS IDENTITY
, firstName VARCHAR NOT NULL
, lastName VARCHAR NOT NULL
, PRIMARY KEY(userId)
);

CREATE TYPE account AS ENUM ('Liability', 'Expense');

CREATE TABLE IF NOT EXISTS transactions
( transactionId INT GENERATED BY DEFAULT AS IDENTITY
, fromUserId INT NOT NULL
, toUserId INT NOT NULL
, amount INT NOT NULL
, account account NOT NULL
, note VARCHAR(100) NOT NULL
, date TIMESTAMP NOT NULL DEFAULT now()
, PRIMARY KEY(transactionId)
, CONSTRAINT fk_from_user
    FOREIGN KEY(fromUserId)
      REFERENCES users(userId)
, CONSTRAINT fk_to_user
    FOREIGN KEY(toUserId)
      REFERENCES users(userId)
);

CREATE INDEX IF NOT EXISTS transaction_account_idx
ON transactions
(account);
