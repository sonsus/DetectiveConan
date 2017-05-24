import Network
import System.IO

htmlRes :: String
htmlRes = "\
  \HTTP/0.1 200 OK\r\n\
  \Content-Type: text/html\r\n\
  \Content-Length: 14\r\n\
  \Connection: close\r\n\
  \\r\n\
  \Hello World!\r\n\
  \"

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 8003
  putStrLn "Starting server at port 8003 ..."
  handleConnections sock

handleConnections :: Socket -> IO ()
handleConnections sock = do
  (handle, host, port) <- accept sock
  req <- hGetLine handle
  putStrLn $ "Got request starting with " ++ req
  hPutStrLn handle htmlRes
  putStrLn $ "Sent respond to " ++ show host ++ " " ++ show port
  handleConnections sock
