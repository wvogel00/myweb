{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Network.HTTP.Media ( (//), (/:) )
import qualified Data.ByteString.Lazy as BS hiding (pack)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import qualified Codec.Binary.UTF8.String as UTF8
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status (status200)
import Servant
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text.Lazy
import qualified Data.Text.Lazy.IO as TIO


data HTML
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML BS.ByteString where
    mimeRender _ = id
instance MimeUnrender HTML BS.ByteString where
    mimeUnrender _ = Right . id

data FileType = MdFile | HTMLFile | CSSFile | JSFile | IMGFile deriving Eq

type API = Get '[HTML] BS.ByteString
    :<|> "blog" :> Get '[HTML] BS.ByteString
    :<|> "devs" :> Get '[HTML] BS.ByteString
    :<|> "pubs" :> Get '[HTML] BS.ByteString
    :<|> "contact" :> Get '[HTML] BS.ByteString
    :<|> "html" :> Raw
    :<|> "css" :> Raw
    :<|> "js" :> Raw
    :<|> "img" :> Raw

top, blog, devs, pubs, contact :: FilePath
top = "html/index.html"
blog = "html/blog.html"
devs = "html/devs.html"
pubs = "html/pubs.html"
contact = "html/contact.html"

webServer :: Server API
webServer = getHtml top
        :<|> getHtml blog
        :<|> getHtml devs
        :<|> getHtml pubs
        :<|> getHtml contact
        :<|> getStatics HTMLFile
        :<|> getStatics CSSFile
        :<|> getStatics JSFile
        :<|> getStatics IMGFile

getHtml :: FilePath -> Handler BS.ByteString
getHtml = liftIO . BS.readFile

getStatics HTMLFile = serveDirectoryWebApp "html"
getStatics CSSFile = serveDirectoryWebApp "css"
getStatics JSFile = serveDirectoryWebApp "js"
getStatics IMGFile = serveDirectoryWebApp "img"

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 $ serve api webServer