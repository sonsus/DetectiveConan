import Network
import System.IO

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 3001
  putStrLn "Starting server at port 3001 ..."
  handleConnections sock
  
handleConnections :: Socket -> IO ()
handleConnections sock = do
  (handle, host, port) <- accept sock
  output <- hGetLine handle
  putStrLn output
  handleConnections sock
