module Client where

import Network.Simple.TCP

main = do
  connect "192.168.202.135" "8000" $ \(connectionSocket, remoteAddr) -> do
    putStrLn $ "Connection established to " ++ show remoteAddr
    -- Now you may use connectionSocket as you please within this scope,
    -- possibly using recv and send to interact with the remote end.