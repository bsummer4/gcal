{-# LANGUAGE DataKinds #-}

module Main where


-- Imports ---------------------------------------------------------------------

import Auth
import Relude
import Servant

import qualified Data.ByteString.Char8    as BS8
import qualified Data.Text                as T
--port qualified Network.HTTP.Types       as W
import qualified Network.Wai              as W
import qualified Network.Wai.Handler.Warp as W
import Control.Monad.IO.Class


-- Types -----------------------------------------------------------------------

type Api = "googauth" :> QueryParam "state" Text
                      :> QueryParam "code" Text
                      :> QueryParam "error" Text
                      :> Get Text
      :<|> Get Text

authCode ∷ MonadIO m ⇒ Maybe Text → Maybe Text → Maybe Text → m Text
authCode (Just _) Nothing  (Just msg) = return $ "auth error: " <> msg
authCode (Just _) (Just _) Nothing  = return "auth success"
authCode _        _        _        = return "invalid auth"

frontPage ∷ MonadIO m ⇒ m Text
frontPage = return "front page"

apiServer ∷ W.Application
apiServer = serve (Proxy∷Proxy Api) $ authCode :<|> frontPage


-- Values ----------------------------------------------------------------------

main ∷ IO ()
main = do
  [clid] <- map (ClientID . T.pack) <$> getArgs
  let params = authExample & clientID .~ clid
  BS8.putStrLn $ authURL params ^. uriBS
  W.run 8080 apiServer
