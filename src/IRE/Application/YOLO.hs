module IRE.Application.YOLO
  ( findItems
  ) where

import qualified Data.ByteString.Lazy as LBS

import qualified IRE.Logging as Log
import IRE.YOLO (Detector, Item, detect)

findItems :: Detector -> LBS.ByteString -> IO [Item]
findItems d fc = do
  r <- detect d 0.3 0.3 (LBS.toStrict fc)
  Log.debug $ show r
  return r
