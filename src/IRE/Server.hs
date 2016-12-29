module IRE.Server (
  server
) where

import Control.Exception.Base (throwIO, catch, bracket)
import Data.Yaml.Include (decodeFileEither)
import Data.Bits ((.|.))
import Network.Socket (socket, setSocketOption, bind, listen, close,
  maxListenQueue, getSocketName, inet_addr, Family(AF_UNIX, AF_INET),
  SocketType(Stream), SocketOption(ReuseAddr), Socket, SockAddr(..))
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettingsSocket)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files ( fileExist, groupReadMode, groupWriteMode,
  ownerReadMode, ownerWriteMode, removeLink, setFileMode, socketMode )

import IRE.Application (app)
import IRE.Config (ConfigFile(..), defaultConfig)
import IRE.YOLO (newDetector)
import qualified IRE.Logging as Log

type Listen = Either Port FilePath

server :: FilePath -> FilePath -> IO ()
server rootDir configFile = do
  cf <- readConfigFile configFile
  net <- newDetector (cfYOLO cf)
  Log.start Log.Debug
  let socketSpec =
        case cfSocket cf of
          Just p  -> Right p
          Nothing -> Left $ cfPort cf
  bracket
    (createSocket socketSpec)
    closeSocket
    ( \sock -> do
      listen sock maxListenQueue
      runSettingsSocket defaultSettings sock =<< app rootDir net)


readConfigFile :: FilePath -> IO ConfigFile
readConfigFile f = do
  e <- fileExist f
  if e then do
    r <- decodeFileEither f
    case r of
      Left ex -> do
        hPutStrLn stderr $ "FATAL: " ++ f ++ ": " ++ show ex
        exitFailure
      Right cf -> return cf
  else return defaultConfig


createSocket :: Listen -> IO Socket
createSocket (Right path) = do
  removeIfExists path
  sock <- socket AF_UNIX Stream 0
  bind sock $ SockAddrUnix path
  setFileMode path $ socketMode
                  .|. ownerWriteMode .|. ownerReadMode
                  .|. groupWriteMode .|. groupReadMode
  Log.info $ "Listening on UNIX socket `" ++ path ++ "'"
  return sock
createSocket (Left port) = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  addr <- inet_addr "127.0.0.1"
  bind sock $ SockAddrInet (fromIntegral port) addr
  Log.info $ "Listening on localhost:" ++ show port
  return sock


closeSocket :: Socket -> IO ()
closeSocket sock = do
  name <- getSocketName sock
  close sock
  case name of
    SockAddrUnix path -> removeIfExists path
    _ -> return ()


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeLink fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

