{-# language LambdaCase #-}
module Quill.Backend (Backend, Config(..), withBackend, request) where

import qualified Capnp (defaultLimit, sGetValue, sPutValue)
import Capnp.Gen.Request.Pure (Request(..))
import Capnp.Gen.Response.Pure (Response(..))
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Socket as Socket
import qualified System.Process as Process

newtype Backend = Backend { request :: Request -> IO Response }

data Config
  = Config
  { _cfgDbHost :: Maybe Text
  , _cfgDbPort :: Maybe Int
  , _cfgDbName :: Maybe Text
  , _cfgDbUser :: Maybe Text
  , _cfgDbPassword :: Maybe Text
  }

withBackend :: Text -> Config -> (Backend -> IO ()) -> IO ()
withBackend name config k = do
  addr <-
    head <$>
    Socket.getAddrInfo
      (Just $
       Socket.defaultHints
       { Socket.addrFlags = [Socket.AI_PASSIVE]
       , Socket.addrFamily = Socket.AF_INET
       , Socket.addrSocketType = Socket.Stream
       }
      )
      Nothing
      (Just "0")
  bracket
    (Socket.socket
       (Socket.addrFamily addr)
       (Socket.addrSocketType addr)
       (Socket.addrProtocol addr)
    )
    Socket.close
    (\init_sock -> do
       Socket.bind init_sock (Socket.addrAddress addr)
       port <-
         (\case; Socket.SockAddrInet port _ -> pure port; _ -> error "Got non-IPv4 socket address") =<<
         Socket.getSocketName init_sock
       Socket.listen init_sock 5
       forkBackend port name config
       bracket
         (fst <$> Socket.accept init_sock)
         Socket.close
         (\sock -> do
            let
              backend = Backend $ \req -> do
                Capnp.sPutValue sock req
                Capnp.sGetValue sock Capnp.defaultLimit
            k backend
         )
    )

forkBackend :: Socket.PortNumber -> Text -> Config -> IO ()
forkBackend port name config =
  void . forkIO $ do
    Process.callProcess ("quill-" <> Text.unpack name) $
      ["--port", show port] <>
      withArg "--db-host" _cfgDbHost Text.unpack <>
      withArg "--db-port" _cfgDbPort show <>
      withArg "--db-name" _cfgDbName Text.unpack <>
      withArg "--db-user" _cfgDbUser Text.unpack <>
      withArg "--db-password" _cfgDbUser Text.unpack
  where
    withArg argName get render =
      maybe [] (\arg -> [argName, render arg]) (get config)
