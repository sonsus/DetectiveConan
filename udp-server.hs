import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "8003")
  s <- socket (addrFamily server) Datagram defaultProtocol
  bind s (addrAddress server)-- >> return s
  putStrLn "Server started at port 8003 ..."
  handleConnections s

handleConnections :: Socket -> IO ()
handleConnections conn = do
  (text, _, _) <- recvFrom conn 1024
  putStrLn text
  handleConnections conn
