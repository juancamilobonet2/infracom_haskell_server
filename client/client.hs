module Client where

import Network.Simple.TCP
import System.Environment
import Data.Maybe (fromMaybe, isNothing)
import Data.Text.Encoding
import Data.Text ( unpack )
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy               as L
import qualified Data.ByteString                    as B
import Control.Monad.Loops (untilM', untilM, whileJust)
import CPrim (bSwapLabel)

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

  --Recibir archivo
  bsList <- whileJust (recv connectionSocket 600000) return

  let fileName = "./ArchivosRecibidos/Cliente"++numCliente++"-Prueba-"++clientes++".txt"
  let file = L.fromChunks bsList
  L.writeFile fileName file

  putStrLn "Listo"



encodeUtf8Txt :: String -> L.ByteString
encodeUtf8Txt = toLazyByteString . stringUtf8
