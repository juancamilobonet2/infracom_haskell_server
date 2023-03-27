{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module TCP where

import           Network.Simple.TCP
import           Control.Concurrent
import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Builder
import           Data.Maybe ( isNothing )
--import           Control.Monad.Loops(untilM_)

main = do
  putStrLn "Starting server..."
  putStrLn "Cual archivo quiere mandar?"
  file <- readLn
  putStrLn "Cuantos clientes quieres atender a la vez"
  clientes <- readLn
  clientesConn <- newMVar 0
  serve (Host "0.0.0.0") "8000" (handleSocket clientesConn clientes file)
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.kkkk

handleSocket :: MVar Integer -> Integer -> String -> (Socket, SockAddr) -> IO()
handleSocket totalMvar clientes file (connectionSocket,remoteAddr) = do
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

  -- Mandar archivo
  contenido <- L.readFile file
  sendLazy connectionSocket contenido
  putStrLn $ "Cliente atendido: " ++ show remoteAddr

  --TODO hash y logs


untilMVar :: (a -> Bool) -> MVar a -> IO()
untilMVar p mvar = do
  x <- readMVar mvar
  if not (p x)
    then untilMVar p mvar
    else return ()

encodeUtf8Txt :: String -> L.ByteString
encodeUtf8Txt = toLazyByteString . stringUtf8