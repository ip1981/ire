{-# LANGUAGE OverloadedStrings #-}

module IRE.Application
  ( app
  ) where

import Control.Monad.Trans (liftIO)
import Network.Wai (Application)
import Network.Wai.Middleware.Static
       ((>->), addBase, hasPrefix, staticPolicy)
import Network.Wai.Parse (FileInfo(..))
import System.FilePath.Posix ((</>))
import Web.Scotty
       (ActionM, ScottyM, file, files, get, json, middleware, post,
        scottyApp)

import IRE.Application.YOLO (findItems)
import IRE.YOLO (Detector)

app :: FilePath -> Detector -> IO Application
app rootDir net = scottyApp $ ire rootDir net

ire :: FilePath -> Detector -> ScottyM ()
ire rootDir net = do
  middleware $ staticPolicy (hasPrefix "static" >-> addBase rootDir)
  get "/" $ file (rootDir </> "webui.html")
  post "/findItems" $ apiFindItems net

apiFindItems :: Detector -> ActionM ()
apiFindItems net = do
  fs <- files
  let fc = head [fileContent fi | (_, fi) <- fs]
  resp <- liftIO $ findItems net fc
  json resp
