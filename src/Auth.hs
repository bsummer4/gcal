{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Auth where

import Relude

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Text.Encoding      as T
import qualified Network.HTTP.Types.URI  as W


-- Types -----------------------------------------------------------------------

-- ^ Some arbitrary token that is passed through Google's servers.
-- This is useful for preventing request forgeries.
newtype StateToken = StateToken { _stTokText ∷ Text }

newtype ClientID = ClientID { _clientIDText ∷ Text }
newtype Scope = Scope { _scopeText ∷ Text } -- May not contain spaces
type AuthScope = [Scope]
type EmailAddress = Text
data AccessType = Online | Offline
data PromptWhen = PromptAlways | PromptAuto
newtype URI = URI { _uriBS ∷ ByteString }

data AuthParams = AuthParams {
    _clientID             ∷ ClientID
  , _redirectURI          ∷ URI
  , _stateTok             ∷ StateToken
  , _accessTy             ∷ AccessType
  , _scope                ∷ AuthScope
  , _promptWhen           ∷ PromptWhen
  , _emailAddress         ∷ Maybe EmailAddress
  , _includeGrantedScopes ∷ Bool
  }


-- Instances -----------------------------------------------------------------

deriving instance Eq StateToken
deriving instance IsString StateToken

deriving instance IsString Scope

deriving instance Eq URI
deriving instance Ord URI
deriving instance Monoid URI
deriving instance IsString URI

instance Show URI where
  show (URI u) = show u

makeLenses ''ClientID
makeLenses ''Scope
makeLenses ''AuthParams
makeLenses ''StateToken
makeLenses ''URI


-- Values ----------------------------------------------------------------------

authExample ∷ AuthParams
authExample = AuthParams
  { _clientID = ClientID "000000000000-00000000000000000000000000000000.apps.googleusercontent.com"
  , _stateTok = ""
  , _accessTy = Online
  , _scope = ["https://www.googleapis.com/auth/calendar.readonly"]
  , _promptWhen = PromptAuto
  , _redirectURI = "https://oauth2-login-demo.appspot.com/code"
  , _emailAddress = Nothing
  , _includeGrantedScopes = True
  }

authBase ∷ Text
authBase = "https://accounts.google.com/o/oauth2/auth"

authURL ∷ AuthParams → URI
authURL params = bsbToURI (bsbText authBase <> "?" <> queryParams)
  where bsbToURI    = URI . BSL.toStrict . BSB.toLazyByteString
        bsbText     = BSB.byteString . T.encodeUtf8
        queryParams = W.renderQueryText False . fmtAuthParams $ params

-- https://developers.google.com/identity/protocols/OAuth2WebServer#formingtheurl
fmtAuthParams ∷ AuthParams → [(Text,Maybe Text)]
fmtAuthParams params = second Just <$> catMaybes results
  where
    results ∷ [Maybe(Text,Text)] =
      [ ("response_type",) <$> Just "code"

      , ("client_id",) <$> Just (params ^. clientID . clientIDText)

      , ("redirect_uri",) <$> Just (T.decodeUtf8 $ params ^. redirectURI . uriBS)

      , ("scope",) <$> Just (
          fold . intersperse " " $
            view scopeText <$> params ^. scope)

      , ("state",) <$> Just (params ^. stateTok . stTokText)

      , ("access_type",) <$> Just (
          case params ^. accessTy of Online  → "online"
                                     Offline → "offline")

      , ("approval_prompt",) <$> Just (
          case params ^. promptWhen of PromptAuto   → "auto"
                                       PromptAlways → "force")

      , ("login_hint",) <$> params ^. emailAddress

      , ("include_granted_scopes",) <$> Just (
          if params ^. includeGrantedScopes then "true" else "false")
      ]
