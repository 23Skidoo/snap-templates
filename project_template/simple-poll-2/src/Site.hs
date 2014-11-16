{-# LANGUAGE OverloadedStrings #-}
module Site (app) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad (mfilter)
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.ByteString (ByteString)
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Map.Syntax ((##))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import qualified Text.XmlHtml as X

import Application

pollChoices :: [Text]
pollChoices = ["Real World Haskell"
              ,"Learn You a Haskell"
              ,"Beginning Haskell"
              ,"Programming Haskell"]

packInt :: Int -> Text
packInt = T.pack . show

poll :: AppHandler ()
poll = renderWithSplices "poll" $ "form-content" ## splice
  where
    radioButton n = [X.Element "input"
                     [("type", "radio"), ("name", "answer"), ("value", n)]
                     [],
                     X.TextNode n, X.Element "br" [] [], X.TextNode "\n"]
    splice        = return $ concatMap radioButton pollChoices

postResults :: AppHandler ()
postResults = do
  r   <- gets (view mapRef)
  par <- getParam "answer"
  let isValidKey k = k `elem` pollChoices
      key          = fromMaybe "???" $ mfilter isValidKey (E.decodeUtf8 <$> par)
  liftIO $ atomicModifyIORef r (M.insertWith (+) key 1)
  showResults

showResults :: AppHandler ()
showResults = do
  r <- gets (view mapRef)
  m <- liftIO $ readIORef r
  renderWithSplices "results" $ "table-content" ## splice m
    where
      getVal k m   = packInt $ M.findWithDefault 0 k m
      tableRow k m = [X.Element "tr" []
                      [X.Element "td" [] [X.TextNode k]
                      ,X.Element "td" [] [X.TextNode $ getVal k m]]
                     ,X.TextNode "\n"]
      splice m     = return $ concatMap (flip tableRow m) pollChoices

error404 :: MonadSnap m => m ()
error404 = do modifyResponse $ setResponseStatus 404 "File Not Found"
              writeBS "File Not Found"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("",        error404)
         , ("results", method POST postResults)
         , ("results", method GET  showResults)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "poll" "A simple poll application." Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  r <- liftIO (newIORef M.empty)
  addRoutes routes
  wrapSite (ifTop poll <|>)
  return $ App h r
