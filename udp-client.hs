import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  (server:_) <- getAddrInfo Nothing (Just "localhost") (Just "8003")
  s <- socket (addrFamily server) Datagram defaultProtocol
  connect s (addrAddress server)
  send s "Hello, World!"
  sClose s
