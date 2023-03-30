module Main where

import Network.Simple.TCP
import System.Environment
import Data.Maybe (fromMaybe, isNothing)
import Data.Text.Encoding
import Data.Text ( unpack )
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy               as L
import qualified Data.ByteString                    as B
import Control.Monad.Loops (untilM', untilM, whileJust)
import Crypto.Hash

main = do
  connect "192.168.202.135" "80" cliente

cliente :: (Socket, SockAddr) -> IO()
cliente (connectionSocket, remoteAddr) = do
  putStrLn $ "Connection established to " ++ show remoteAddr

  --Mandar listo
  sendLazy connectionSocket $ encodeUtf8Txt "ready"


  --Recibir num cliente
  maybeNumCliente <- recv connectionSocket 5
  let numCliente = maybe "Err" (unpack . decodeUtf8) maybeNumCliente

  --Recibir num clientes totales
  maybeClientes <- recv connectionSocket 5
  let clientes = maybe "Err" (unpack . decodeUtf8) maybeClientes

  --Recibir hash
  maybeHash <- recv connectionSocket 100
  let hash = fromMaybe B.empty maybeHash

  --Mandar listo para recibir archivo
  sendLazy connectionSocket $ encodeUtf8Txt "ready"

  --Recibir archivo
  bsList <- whileJust (recv connectionSocket 600000) return

  putStrLn $ "Recibido cliente " ++ numCliente

  --Verificar hash
  let hashed = hashlazy (L.fromChunks bsList) :: Digest SHA256
  let bsHashed = encodeUtf8Txt $ show hashed

  if bsHashed == L.fromStrict hash
    then putStrLn $ "Hash correcto cliente " ++ numCliente
    else putStrLn $ "Hash incorrecto cliente " ++ numCliente

  let fileName = "./ArchivosRecibidos/Cliente"++numCliente++"-Prueba-"++clientes++".txt"
  let file = L.fromChunks bsList
  L.writeFile fileName file



  putStrLn $ "Listo cliente " ++ numCliente



encodeUtf8Txt :: String -> L.ByteString
encodeUtf8Txt = toLazyByteString . stringUtf8
