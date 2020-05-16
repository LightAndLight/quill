{-# language LambdaCase #-}
module Database where

import Control.Exception (bracket)
import qualified Data.Text as Text
import qualified Network.Socket as Socket

import Quill.Backend (Backend)
import qualified Quill.Backend as Backend
import qualified Quill.Backend.Postgres as Postgres

setupDb ::
  (Backend -> IO ()) ->
  (Backend -> IO ()) ->
  (Backend -> IO ()) ->
  IO ()
setupDb setup teardown k =
  Backend.withBackend
    (\server_sock config -> do
       port <-
         (\case; Socket.SockAddrInet port _ -> pure $ show port; _ -> error "Got non-IPv4 socket address") =<<
         Socket.getSocketName server_sock
       addr <-
         head <$>
         Socket.getAddrInfo
           (Just $
           Socket.defaultHints
           { Socket.addrFamily = Socket.AF_INET
           , Socket.addrSocketType = Socket.Stream
           }
           )
           (Just "127.0.0.1")
           (Just port)
       bracket
         (Socket.socket
           (Socket.addrFamily addr)
           (Socket.addrSocketType addr)
           (Socket.addrProtocol addr)
         )
         Socket.close
         (\client_sock -> do
            Socket.connect client_sock (Socket.addrAddress addr)
            Postgres.run client_sock $
              Postgres.Config
              { Postgres._cfgPort = port
              , Postgres._cfgDbHost = Text.unpack <$> Backend._cfgDbHost config
              , Postgres._cfgDbPort = show <$> Backend._cfgDbPort config
              , Postgres._cfgDbName = Text.unpack <$> Backend._cfgDbName config
              , Postgres._cfgDbUser = Text.unpack <$> Backend._cfgDbUser config
              , Postgres._cfgDbPassword = Text.unpack <$> Backend._cfgDbPassword config
              }
         )
    )
    Backend.emptyConfig
    (\b -> bracket (pure b) teardown (\backend -> setup backend *> k backend))
