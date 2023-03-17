module TCP where

import Network.Simple.TCP

main = do 
  putStrLn "Starting server..."
  serve (Host "127.0.0.1") "8000" $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "TCP connection established from " ++ show remoteAddr
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.