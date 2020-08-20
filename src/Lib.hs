{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Network.HTTP.Media ( (//), (/:) )
import qualified Data.ByteString.Lazy as BS
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status (status200)
import Servant
import Control.Monad.IO.Class (liftIO)


data HTML
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML BS.ByteString where
    mimeRender _ bs = bs

data FileType = HTMLFile | CSSFile | JSFile | IMGFile deriving Eq

type API = Get '[HTML] BS.ByteString
    :<|> "html" :> Raw
    :<|> "css" :> Raw
    :<|> "js" :> Raw
    :<|> "img" :> Raw
    -- :<|> "login" :> Capture "username" BS.ByteString :> Capture "password" BS.ByteString :> Post '[HTML] BS.ByteString

webServer :: Server API
webServer = getToppage
        :<|> getStatics HTMLFile
        :<|> getStatics CSSFile
        :<|> getStatics JSFile
        :<|> getStatics IMGFile
        -- :<|> loginWeb

getToppage :: Handler BS.ByteString
getToppage = liftIO $ BS.readFile "html/index.html"

getStatics HTMLFile = serveDirectoryWebApp "html"
getStatics CSSFile = serveDirectoryWebApp "css"
getStatics JSFile = serveDirectoryWebApp "js"
getStatics IMGFile = serveDirectoryWebApp "img"

loginWeb :: BS.ByteString -> BS.ByteString -> Handler BS.ByteString
loginWeb name pass = return $ BS.fromStrict "ok"

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 $ serve api webServer