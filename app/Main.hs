module Main where

import Control.Concurrent (
  MVar,
  forkIO,
  modifyMVar,
  modifyMVar_,
  newMVar,
  readMVar,
 )
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import HtmlBuilder (createErrorMsg, createUserChatMsg, createUserConnectedMsg)
import Network.Wai.Application.Static (
  defaultFileServerSettings,
  staticApp,
 )
import Network.Wai.Handler.Warp (run)
import Network.WebSockets qualified as WS

type Client = (Text, WS.Connection)

type ServerState = [Client]

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

main :: IO ()
main = do
  state <- newMVar []
  _ <- forkIO $ WS.runServer "127.0.0.1" 9160 $ application state
  putStrLn "Running server on http://localhost:8000/ and ws://localhost:9160/ ..."
  run 8000 (staticApp (defaultFileServerSettings "./html"))

data ConnectMessage where
  ConnectMessage :: {userName :: Text} -> ConnectMessage
  deriving (Show, Eq, Generic)

instance FromJSON ConnectMessage

data ChatMessage where
  ChatMessage :: {chatMessage :: Text} -> ChatMessage
  deriving (Show, Eq, Generic)

instance FromJSON ChatMessage

-- | Application that runs the WebSocket server
application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    clients <- readMVar state
    case decodeStrictText msg of
      Nothing -> do
        pure ()
      Just (ConnectMessage userName) -> do
        case userName of
          _
            | clientExists client clients -> do
                WS.sendTextData
                  conn
                  ( createErrorMsg
                      "Client with that name already exists. Please choose another name."
                  )
            | otherwise -> flip finally disconnect $ do
                modifyMVar_ state $ \s -> do
                  let s' = addClient client s
                  WS.sendTextData conn $
                    createUserConnectedMsg userName (numClients s')
                  broadcast (createUserChatMsg "_system_" (fst client <> " joined")) s'
                  return s'
                talk client state
       where
        client = (userName, conn)
        disconnect = do
          s <- modifyMVar state $ \s ->
            let s' = removeClient client s in return (s', s')
          broadcast (createUserChatMsg "_system_" (fst client <> " disconnected")) s

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  case decodeStrictText msg of
    Nothing -> pure ()
    Just (ChatMessage chatMessage) ->
      do
        readMVar state
        >>= broadcast (createUserChatMsg user chatMessage)
