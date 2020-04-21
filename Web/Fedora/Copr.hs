{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.Fedora.Copr where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving (Show, Generic)

instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)

instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving (Show, Generic)

instance FromJSON Email

type API = "" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> ClientM Position

hello :: Maybe String -- ^ an optional value for "name"
      -> ClientM HelloMessage

marketing :: ClientInfo -- ^ value for the request body
          -> ClientM Email

api :: Proxy API
api = Proxy

position :<|> hello :<|> marketing = client api

type API' = API :<|> EmptyAPI

api' :: Proxy API'
api' = Proxy

(position' :<|> hello' :<|> marketing') :<|> EmptyClient = client api'

queries :: ClientM (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, message, em)

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "copr.fedorainfracloud.org" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em

-- type HoistClientAPI = Get '[JSON] Int :<|> Capture "n" Int :> Post '[JSON] Int

-- hoistClientAPI :: Proxy HoistClientAPI
-- hoistClientAPI = Proxy

-- getIntClientM :: ClientM Int
-- postIntClientM :: Int -> ClientM Int
-- getIntClientM :<|> postIntClientM = client hoistClientAPI

-- -- our conversion function has type: forall a. ClientM a -> IO a
-- -- the result has type:
-- -- Client IO HoistClientAPI = IO Int :<|> (Int -> IO Int)
-- getClients :: ClientEnv -> Client IO HoistClientAPI
-- getClients clientEnv
--   = hoistClient hoistClientAPI
--                 ( fmap (either (error . show) id)
--                 . flip runClientM clientEnv
--                 )
--                 (client hoistClientAPI)
