module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef)
import Crypto.Simple (BaseEncoding(..), Hash(..), baseEncode, hash, toString)
import Data.Array (length)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, find, foldMap)
import Data.Maybe (Maybe(..))
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.TemplateString ((<^>))
import Data.Tuple.Nested ((/\))
import Data.URI.URI (parse, print)
import Node.Buffer (BUFFER)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Express.App (App, get, listenHttp, post)
import Node.Express.Request (getBodyParam, getQueryParam, getRouteParam)
import Node.Express.Response (redirect, send, sendJson)
import Node.Express.Types (EXPRESS)
import Node.FS (FS)
import Node.FS.Sync (readFile)
import Node.HTTP (Server)

type Url = { id :: String, url :: String }

index :: forall m. String -> Array Url -> String
index template urls = do
  let urlList = "<ul>" <> (foldMap (\url -> "<li><a href='/" <> url.id <> "' target='_blank'>" <> url.id <> " => " <> url.url <> "</a></li>") urls) <> "</ul>"
  template <^> ["urls" /\ urlList]

fileToString :: forall eff. String -> Eff (fs :: FS, exception :: EXCEPTION, buffer :: BUFFER | eff) String
fileToString file = do
  file <- readFile "templates/index.html"
  Buffer.toString UTF8 file

urlToId :: String -> Either String String
urlToId "" = Left "URL cannot be empty"
urlToId url = do
  url <- bimap (const "Invalid URL") print $ parse url
  pure $ toString $ hash SHA256 url

appSetup :: forall eff. String -> Ref (Array Url) -> App (console :: CONSOLE, ref :: REF | eff) 
appSetup template ref = do
  get "/" do
    urls <- liftEff $ readRef ref
    send $ index template urls
  get "/:id" do
    idParam <- getRouteParam "id"
    urls <- liftEff $ readRef ref
    case idParam of
      Nothing -> send "Must provide a path"
      Just id -> 
      case find (\url -> url.id == id) urls of
        Nothing -> send "Unable to redirect to unknown destination"
        Just destination -> redirect destination.url
  get "/urls/create" do
    urlParam <- getQueryParam "url"
    case urlParam of
      Nothing -> send "You must provide a URL"
      Just url -> 
        case urlToId url of
          Left error -> send error
          Right encodedUrl -> liftEff do 
            modifyRef ref (\urls -> urls <> [{ id: encodedUrl, url: url }])
            log $ "URL added: " <> url
    redirect "/" -- TODO: cannot set headers after sending an error

main :: forall h eff. Eff (console :: CONSOLE, express :: EXPRESS, ref :: REF, fs :: FS, exception :: EXCEPTION, buffer :: BUFFER | eff) Server
main = do
  template <- fileToString "templates/index.html"
  ref <- newRef []
  listenHttp (appSetup template ref) 8080 (\_ -> log "Listening on 8080...")
