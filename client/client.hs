module Client where

import Network.Simple.TCP
import System.Environment
import Data.Maybe (fromMaybe)
import Data.Text.Encoding
import Data.Text ( unpack )
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy               as L
import qualified Data.ByteString                    as B

main = do
  connect "192.168.202.135" "8000" cliente

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

  --Recibir archivo
  maybeFile <- recv connectionSocket 300000000
  let fileName = "./ArchivosRecibidos/Cliente"++numCliente++"-Prueba-"++clientes++".txt"
  let file = fromMaybe B.empty maybeFile
  B.writeFile fileName file

  putStrLn "Listo"



encodeUtf8Txt :: String -> L.ByteString
encodeUtf8Txt = toLazyByteString . stringUtf8
