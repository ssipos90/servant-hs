{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module SmallAccounts
    ( startApp
    ) where
import           Control.Monad.Reader                 (MonadIO (liftIO),
                                                       ReaderT (..), ask,
                                                       runReaderT)
import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON (toJSON),
                                                       Value (String),
                                                       defaultOptions, object,
                                                       withText, (.=))
import           Data.Aeson.TH                        (deriveJSON)
import           Data.ByteString.Char8                (pack)
import           Data.Pool                            (Pool (..), createPool,
                                                       withResource)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import           Database.PostgreSQL.Simple           (ConnectInfo, Connection,
                                                       FromRow, ToRow, In (In),
                                                       Only (Only), close,
                                                       connect, query)
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       ResultError (..),
                                                       returnError)
import           Database.PostgreSQL.Simple.Time      (LocalTimestamp)
import           Database.PostgreSQL.Simple.ToField   (Action (..),
                                                       ToField (..))
import           GHC.Generics                         (Generic (..))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant                              (Capture, Handler,
                                                       HasServer (ServerT),
                                                       Proxy (..), QueryParams,
                                                       ServerT, hoistServer,
                                                       serve, FromHttpApiData (parseQueryParam))
import           Servant.API                          (Get, JSON, Post, ReqBody,
                                                       (:<|>) (..), (:>))

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Show, Generic)

instance Eq User where
  User {userId = userId1} == User {userId = userId2} = userId1 == userId2

data CreateUser = CreateUser
  { firstName :: String
  , lastName  :: String
  } deriving (Show, Generic)

data Account = Liability | Expense deriving (Show, Eq)

instance FromJSON Account where
  parseJSON = withText "Account" $ \case
      "Liability" -> return Liability
      "Expense"   -> return Expense
      _           -> fail "invalid account type"

instance ToJSON Account where
  toJSON = String . T.pack . show

instance FromHttpApiData Account where
  parseQueryParam "Liability" = Right Liability
  parseQueryParam "Expense" = Right Expense
  parseQueryParam a = Left $ "Fuck if I know what " <> a <> " is."

instance FromField Account where
  fromField f mData = case mData of
                        Just d -> case decodeUtf8 d of
                          "Liability" -> return Liability
                          "Expense" -> return Expense
                          s -> returnError ConversionFailed f $ T.unpack s
                        Nothing -> returnError Incompatible f ""

escapeShow :: Show a => a -> Action
escapeShow = Escape . pack . show

instance ToField Account where
  toField a = escapeShow a

data Transaction = Transaction
  { transactionId :: Int
  , fromUserId    :: Int
  , toUserId      :: Int
  , amount        :: Int
  , date          :: String
  , account       :: Account
  , note          :: T.Text
  -- TODO: storno
  } deriving (Show, Generic, FromRow)

data AddTransaction = AddTransaction
  { fromUserId :: Int
  , toUserId   :: Int
  , amount     :: Int
  , account    :: Account
  , note       :: T.Text
  } deriving (Generic)

instance FromJSON CreateUser

instance FromJSON AddTransaction

instance ToRow AddTransaction

instance FromRow User

instance ToJSON User where
  toJSON User { userId, userFirstName, userLastName } =
    object [ "userId" .= userId
           , "firstName" .= userFirstName
           , "lastName"  .= userLastName
           ]

$(deriveJSON defaultOptions ''Transaction)

data AppConfig = AppConfig {
  pool :: Pool Connection
}

initConnectionPool :: ConnectInfo  -> IO (Pool Connection)
initConnectionPool connInfo =
  createPool (connect connInfo)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe


startApp :: ConnectInfo -> IO ()
startApp connInfo = do
  _ <- putStrLn "Initializing database pool"
  pool <- initConnectionPool connInfo
  _ <- putStrLn "Running app"
  run 8080 (app $ AppConfig { pool = pool })

app :: AppConfig -> Application
app db = serve api (hoistServer api (readerToHandler db) server)


type API =
          "users" :> (
              Get '[JSON] [User]
          :<|> ReqBody '[JSON] CreateUser :> Post '[JSON] User
          :<|> Capture "userId" Int :> (
                Get '[JSON] User
            :<|> "transactions" :> QueryParams "accounts" Account :> Get '[JSON] [Transaction]
                )
        )
     :<|> "transactions" :> (
                Get '[JSON] [Transaction]
           :<|> ReqBody '[JSON] AddTransaction :> Post '[JSON] Transaction
          )


api :: Proxy API
api = Proxy


readerToHandler :: AppConfig -> ReaderT AppConfig Handler a -> Handler a
readerToHandler db r = runReaderT r db


server :: ServerT API (ReaderT AppConfig Handler)
server = (listUsers
  :<|> createUser
  :<|> (\userId ->
          showUser userId
     :<|> showUserTransactions userId
  ))
  :<|> (
         listTransactions
    :<|> addTransaction
    )


showUserTransactions :: Int -> [Account] -> ReaderT AppConfig Handler [Transaction]
showUserTransactions userId accounts = do
  _ <- liftIO $ putStrLn "Showing user transactions"
  AppConfig{ pool } <- ask
  res :: [Transaction] <- liftIO $ withResource pool (\conn ->
      if null accounts then
        query conn "SELECT transactionId, fromUserId, toUserId, amount, date \
                   \FROM transactions \
                   \WHERE fromUserId=? OR toUserId=?"
          (userId, userId)
      else
        query conn "SELECT transactionId, fromUserId, toUserId, amount, date \
                  \FROM transactions \
                  \WHERE (fromUserId=? OR toUserId=?) AND account in ?"
          (userId, userId, In accounts)
    )
  return res

addTransaction :: AddTransaction -> ReaderT AppConfig Handler Transaction
addTransaction tr = do
  let AddTransaction{ fromUserId, toUserId, amount, account, note } = tr
  AppConfig{ pool } <- ask
  res :: [(Int, LocalTimestamp)] <- liftIO $ withResource pool (\conn ->
    query conn "INSERT INTO transactions (fromUserId, toUserId, amount, account, note) \
                                 \VALUES (?, ?, ?, ?, ?) \
                                 \RETURNING transactionId, date"
      tr)
  case res of
    [(transactionId, date)] ->
      return Transaction { transactionId = transactionId
                         , fromUserId = fromUserId
                         , toUserId = toUserId
                         , amount = amount
                         , account = account
                         , note = note
                         , date = show date
                         }
    _ -> error "Transaction failed"


listTransactions :: ReaderT AppConfig Handler [Transaction]
listTransactions = do
  AppConfig{ pool } <- ask
  res :: [Transaction] <- liftIO $ withResource pool (\conn ->
    query conn "SELECT * FROM transactions" ())
  return res

showUser :: Int -> ReaderT AppConfig Handler User
showUser userId = do
    AppConfig{ pool } <- ask
    res :: [User] <- liftIO $ withResource pool (\conn -> query conn "SELECT * FROM users WHERE userId=? LIMIT 1" (Only userId))
    case res of
      [user] -> return user
      _ -> error "User not found."


createUser :: CreateUser -> ReaderT AppConfig Handler User
createUser CreateUser{ firstName, lastName } = do
  AppConfig{ pool } <- ask
  res :: [Only Int] <- liftIO $ withResource pool $ \conn ->
    query conn "INSERT INTO users (firstName, lastName) VALUES (?, ?) RETURNING userId"
      (firstName, lastName)
  case res of
    [Only userId] -> return $ User
                  { userId = userId
                  , userLastName = lastName
                  , userFirstName = firstName
                  }
    _ -> error "IDK what happened."


listUsers :: ReaderT AppConfig Handler [User]
listUsers = do
  AppConfig{ pool } <- ask
  res :: [User] <- liftIO $ withResource pool $ \conn ->
    query conn "SELECT * FROM users" ()
  return res
