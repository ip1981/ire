{-# LANGUAGE OverloadedStrings #-}

module IRE.YOLO (
  Detector
, Item(..)
, detect
, newDetector
) where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int32)
import Data.Text (Text)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CChar, CFloat(..), CSize(..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import qualified Control.Concurrent.Lock as L
import qualified Data.Array.IArray as A
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import IRE.Config (YOLO(..))

#include "libdarknet.h"

data Detector = Detector (Ptr ()) (A.Array Int Text) L.Lock

data Item = Item {
  itemClass      :: Int    -- ^ Object class number
, itemName       :: Text   -- ^ human-readable description, e. g. "cat", "backpack".
, itemConfidence :: Float  -- ^ A.K.A. probability
, itemBox        :: (Float, Float, Float, Float)
} deriving (Show)

instance Storable Item where
  sizeOf _ = #{size libdarknet_item}
  alignment _ = #{alignment libdarknet_item}
  poke = undefined
  peek p = do
    _c <- #{peek libdarknet_item, klass} p :: IO Int32
    _p <- #{peek libdarknet_item, confidence} p
    _x <- #{peek libdarknet_item, x} p
    _y <- #{peek libdarknet_item, y} p
    _h <- #{peek libdarknet_item, h} p
    _w <- #{peek libdarknet_item, w} p
    return $ Item (fromIntegral _c) "?" _p (_x, _y, _h, _w)

instance ToJSON Item where
  toJSON (Item c n a (x, y, h, w)) =
    object
      [ "class" .= c
      , "name" .= n
      , "confidence" .= a
      , "box" .= object
          [ "x" .= x
          , "y" .= y
          , "h" .= h
          , "w" .= w
          ]
      ]

newDetector :: YOLO -> IO Detector
newDetector (YOLO cfg weights names) =
  withCString cfg (\c ->
    withCString weights (\w -> do
      n <- T.lines <$> TIO.readFile names
      let a = A.listArray (0, length n) n
      l <- L.new
      d <- libdarknet_new_detector c w
      return $ Detector d a l))


detect :: Detector -> Float -> Float -> ByteString -> IO [Item]
detect (Detector d ns lk) threshold tree_threshold imgdata =
  unsafeUseAsCStringLen imgdata (\(img, len) -> do
    items <- L.with lk $ do
      CSize s <- libdarknet_detect d
                    (CFloat threshold) (CFloat tree_threshold)
                    img (CSize $ fromIntegral len)
      let ptr = libdarknet_get_items d
      peekArray (fromIntegral s) ptr
    return $ map (\i@(Item c _ _ _) -> i{itemName = ns A.! c}) items
    )


foreign import ccall safe "libdarknet_new_detector"
  libdarknet_new_detector :: CString -> CString -> IO (Ptr ())

foreign import ccall safe "libdarknet_detect"
  libdarknet_detect :: Ptr () -> CFloat -> CFloat -> Ptr CChar -> CSize -> IO CSize

foreign import ccall unsafe "libdarknet_get_items"
  libdarknet_get_items :: Ptr () -> Ptr Item

