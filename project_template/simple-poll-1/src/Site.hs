{-# LANGUAGE OverloadedStrings #-}
module Site (app) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad (mfilter)
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Snap.Core
import           Snap.Snaplet

import Application

pollChoices :: [ByteString]
pollChoices = ["Real World Haskell"
              ,"Learn You a Haskell"
              ,"Beginning Haskell"
              ,"Programming Haskell"]

packInt :: Int -> ByteString
packInt = B8.pack . show

poll :: AppHandler ()
poll = writeBS $ B.concat [
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  "<title>Snap example: poll</title>",
  "</head>",
  "<body>",
  "<form method=\"post\" action=\"/results\">",
  "<b>What is the best Haskell book for beginners?</b><br>",
  "<input type=\"radio\" name=\"answer\" value=\"Real World Haskell\">Real World Haskell<br>",
  "<input type=\"radio\" name=\"answer\" value=\"Learn You a Haskell\">Learn You a Haskell<br>",
  "<input type=\"radio\" name=\"answer\" value=\"Beginning Haskell\">Beginning Haskell<br>",
  "<input type=\"radio\" name=\"answer\" value=\"Programming Haskell\">Programming Haskell<br>",
  "<input type=\"submit\" name=\"submit\" value=\"Submit\"><br>",
  "</form>",
  "</body>",
  "</html>"
  ]

postResults :: AppHandler ()
postResults = do
  r   <- gets (view mapRef)
  par <- getParam "answer"
  let isValidKey k = k `elem` pollChoices
      key          = fromMaybe "???" $ mfilter isValidKey par
  liftIO $ atomicModifyIORef r (M.insertWith (+) key 1)
  showResults

showResults :: AppHandler ()
showResults = do
  r            <- gets (view mapRef)
  m            <- liftIO $ readIORef r
  let getVal  k = packInt $ M.findWithDefault 0 k m
  writeBS $ B.concat [
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<title>Snap example: poll results</title>",
    "<style>",
    "table, td {",
    "    border: 1px solid black;",
    "    border-collapse: collapse;",
    "}",
    "td {",
    "    padding: 5px;",
    "}",
    "</style>",
    "</head>",
    "<body>",
    "<b>Results:</b><br>",
    "<table>",
    "<tr><td>Real World Haskell</td><td>",  getVal "Real World Haskell",
    "</td></tr>",
    "<tr><td>Learn You a Haskell</td><td>", getVal "Learn You a Haskell",
    "</td></tr>",
    "<tr><td>Beginning Haskell</td><td>",   getVal "Beginning Haskell",
    "</td></tr>",
    "<tr><td>Programming Haskell</td><td>", getVal "Programming Haskell",
    "</td></tr>",
    "</table>",
    "</body>",
    "</html>"
    ]

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
  r <- liftIO (newIORef M.empty)
  addRoutes routes
  wrapSite (ifTop poll <|>)
  return $ App r
