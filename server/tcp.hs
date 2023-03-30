{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Main where

import           Network.Simple.TCP
import           Control.Concurrent
import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Builder
import           Data.Maybe ( isNothing )
import           Crypto.Hash
import           Data.Time.Clock
import           Data.Time.Format

main = do
  putStrLn "Starting server..."
  putStrLn "Cual archivo quiere mandar?"
  file <- getLine
  putStrLn "Cuantos clientes quieres atender a la vez"
  clientes <- readLn
  clientesConn <- newMVar 0


  contenido <- L.readFile file
  let bsList = groupBySize 50000 contenido
  let hashed = hashlazy contenido :: Digest SHA256
  let bsHashed = encodeUtf8Txt $ show hashed
  putStrLn $ "Hash del archivo: " ++ show hashed

  time <- getCurrentTime
  let logFile = "./logs/<"++formatMyTime time++">.txt"
  logMvar <- newMVar logFile

  serve (Host "0.0.0.0") "80" (handleSocket logMvar clientesConn clientes bsList bsHashed file)
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.

handleSocket :: MVar String -> MVar Integer -> Integer -> [L.ByteString] -> L.ByteString -> String -> (Socket, SockAddr) -> IO()
handleSocket logMvar totalMvar clientes bsList bsHashed file (connectionSocket,remoteAddr) = do
  putStrLn $ "TCP connection established from " ++ show remoteAddr
  maybeReady <- recv connectionSocket 10
  -- TODO exception
  putStrLn $ "Cliente listo: " ++ show remoteAddr
  clientNum <- takeMVar totalMvar
  putMVar totalMvar (clientNum + 1)
  untilMVar (== clientes) totalMvar

  --Mandar numero cliente
  sendLazy connectionSocket $ encodeUtf8Txt $ show clientNum

  --Mandar numero total clientes
  sendLazy connectionSocket $ encodeUtf8Txt $ show clientes

  --Mandar hash
  threadDelay 10000
  sendLazy connectionSocket bsHashed

  --Recibir listo para recibir archivo
  maybeReady <- recv connectionSocket 10

  -- Mandar archivo
  time0 <- getCurrentTime
  mapM_ (sendLazy connectionSocket) bsList
  putStrLn $ "Cliente atendido: " ++ show remoteAddr
  time1 <- getCurrentTime

  --Decrementar numero de clientes
  clients <- takeMVar totalMvar
  putMVar totalMvar (clients - 1)

  --TODO log
  logFile <- takeMVar logMvar
  currTime <- getCurrentTime
  appendFile logFile $ "["++formatMyTime currTime++":"++show remoteAddr ++ "] Transferencia exitosa. Archivo "++file++" Mandado en "++ show (diffUTCTime time1 time0) ++ "Seg."
  putMVar logMvar logFile
  putStrLn $ "Listo, cerrando :" ++ show remoteAddr


untilMVar :: (a -> Bool) -> MVar a -> IO()
untilMVar p mvar = do
  x <- readMVar mvar
  if not (p x)
    then untilMVar p mvar
    else return ()

encodeUtf8Txt :: String -> L.ByteString
encodeUtf8Txt = toLazyByteString . stringUtf8

groupBySize :: Int -> L.ByteString -> [L.ByteString]
groupBySize size bs = go bs
  where
    go bytes
      | L.null bytes = []
      | otherwise      = let (chunk, rest) = L.splitAt (fromIntegral size) bytes
                         in chunk : go rest


formatMyTime :: UTCTime -> String
formatMyTime = formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S"