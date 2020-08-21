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
import Text.Markdown
import Text.Blaze.Html.Renderer.String (renderHtml)


data HTML
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML BS.ByteString where
    mimeRender _ = id
instance MimeUnrender HTML BS.ByteString where
    mimeUnrender _ = Right . id

data FileType = MdFile | HTMLFile | CSSFile | JSFile | IMGFile deriving Eq

data ActionType = Done | Fail | FailBecause BS.ByteString deriving Eq

type API = Get '[HTML] BS.ByteString
    :<|> "blog" :> Get '[HTML] BS.ByteString
    :<|> "html" :> Raw
    :<|> "css" :> Raw
    :<|> "js" :> Raw
    :<|> "img" :> Raw
    :<|> "login" :> QueryParam "user" String :> QueryParam "pass" String :> Post '[HTML] BS.ByteString
    :<|> "update" :> QueryParam "file" FilePath :> ReqBody '[HTML] BS.ByteString :> Post '[PlainText] T.Text

webServer :: Server API
webServer = getToppage
        :<|> getHtmlfromMd
        :<|> getStatics HTMLFile
        :<|> getStatics CSSFile
        :<|> getStatics JSFile
        :<|> getStatics IMGFile
        :<|> loginWeb
        :<|> updateHtml

getToppage :: Handler BS.ByteString
getToppage = liftIO $ BS.readFile "html/index.html"

getStatics HTMLFile = serveDirectoryWebApp "html"
getStatics CSSFile = serveDirectoryWebApp "css"
getStatics JSFile = serveDirectoryWebApp "js"
getStatics IMGFile = serveDirectoryWebApp "img"

getHtmlfromMd :: Handler BS.ByteString
getHtmlfromMd = do
    md <- liftIO $ markdown defaultMarkdownSettings <$> TIO.readFile "md/open.md"
    return $ BS.pack . UTF8.decodeString $ renderHtml md

loginWeb :: Maybe String -> Maybe String -> Handler BS.ByteString
loginWeb (Just name) (Just pass) = do
    liftIO . putStrLn $ "loginREQ : (name,pass) = (" ++  name ++ "," ++ pass ++ ")"
    liftIO $ BS.readFile "html/update.html"
loginWeb _ _ = do
    return "ログインデータが不足しています"

updateHtml :: Maybe FilePath -> BS.ByteString -> Handler T.Text
updateHtml (Just file) bs = do
    liftIO $ BS.writeFile file bs
    return $ "ファイル更新に成功しました"
updateHtml Nothing _ = do
    return "更新ファイルを指定してください"

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 $ serve api webServer