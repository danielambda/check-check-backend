{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AuthServiceClient (getJwtToken) where

import Control.Monad.Identity (Identity(runIdentity, Identity))
import Control.Monad.IO.Class (MonadIO(..))
import Crypto.JOSE (Alg(HS256))
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, secondsToNominalDiffTime)
import GHC.Generics (Generic)
import qualified Crypto.JOSE as Jose
import qualified Crypto.JWT as Jose
import qualified Data.ByteString as BS
import Servant.API (JSON, (:>), Post)
import Servant.Auth (Auth, JWT)
import Servant.Auth.Client (Token (Token))
import Servant.Auth.JWT (ToJWT(encodeJWT))
import Servant.Client (client, ClientM)
import Telegram.Bot.API (UserId, User (..))
import Data.Functor ((<&>))

type AuthServiceAPI = "auth" :> "telegram" :> Auth '[JWT] AuthData :> Post '[JSON] AuthResult

data AuthData = AuthData
  { userId :: UserId
  , username :: Text
  } deriving (Generic, ToJWT, ToJSON)

newtype AuthResult = AuthResult
  { token :: Text }
  deriving (Generic, FromJSON)

authServiceClient :: Token -> ClientM AuthResult
authServiceClient = client $ Proxy @AuthServiceAPI

getJwtToken :: ByteString -> User -> ClientM (Either Jose.Error Token)
getJwtToken secret User{ userId, userUsername } = do
  let username = fromMaybe (error "null username") userUsername
  let jwk = Jose.fromOctets secret
  let jwtAlg = HS256
  expiration <- liftIO $ getCurrentTime <&> addUTCTime (secondsToNominalDiffTime 3600)
  liftIO (makeJWT jwk jwtAlg expiration AuthData{..}) >>= either
    (return . Left)
    (fmap (Right . authResultToToken) . authServiceClient)
  where
    authResultToToken = Token . encodeUtf8 . token

-- partially from servant-auth-server package
makeJWT :: (Jose.MonadRandom m, Jose.AsError e, ToJWT a)
        => Jose.JWK -> Jose.Alg -> UTCTime -> a -> m (Either e Token)
makeJWT jwk alg expiry a = Jose.runJOSE $ do
  ejwt <- Jose.signClaims jwk
                          (Jose.newJWSHeader ((), alg))
                          (addExp $ encodeJWT a)
  return $ Token $ BS.toStrict $ Jose.encodeCompact ejwt
  where
   addExp = Jose.claimExp ?~ Jose.NumericDate expiry
     where setter ?~ value = runIdentity . setter (const $ Identity $ Just value)



