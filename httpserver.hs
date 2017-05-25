import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.Socket
import Network.URL
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

simpleEchoHandler :: Handler String
simpleEchoHandler saddr url req = do
  print saddr
  print url
  print req
  return simpleResponse

c = defaultConfig { srvLog = stdLogger
                  , srvHost = "localhost"
                  , srvPort = 8004
                  }
gsip = 0
gsport = 0

emptyResponse = Response
  (statusCodeTriplet OK)
  (reason OK)
  [ mkHeader HdrConnection "close" ]
  "Hello World!\r\n"

simpleResponse = Response
  (statusCodeTriplet OK)
  (reason OK)
  [ mkHeader HdrServer "Detective Connan Server"
  , mkHeader HdrContentType "text/html"
  , mkHeader HdrContentLength "14"
  , mkHeader HdrConnection "close"
  ]
  "Hello World!\r\n"

main :: IO ()
main = serverWith c handler

handler :: Handler String
handler saddr url req =
  case rqMethod req of
    GET  -> getHandler saddr url req
    POST -> postHandler saddr url req

getHandler :: Handler String
getHandler saddr url req = do
  putStrLn "<<- Got GET request ->>"
  putStr "saddr: "
  print saddr
  putStr "url: "
  print url
  putStrLn "req: "
  print req
  print (rqBody req)
  putStrLn ""

  res <- case url_path url of
    "max_time" -> htmlResponse "max_time.html"
    "max_cpu"  -> htmlResponse "max_cpu.html"
    "avg_cpu"  -> htmlResponse "avg_cpu.html"
    _ -> return $ err_response BadRequest

  putStrLn "<<- Sent response ->>"
  print res
  print (rspBody res)
  putStrLn ""
  return res

htmlResponse :: String -> IO (Response String)
htmlResponse filename = withFile filename ReadMode (\file -> do
  contents <- hGetContents file
  return $ Response
    (statusCodeTriplet OK) (reason OK)
    [ mkHeader HdrServer "Detective Connan Server"
    , mkHeader HdrContentType "text/html"
    -- , mkHeader HdrContentLength "14"
    , mkHeader HdrConnection "close"
    ]
    contents -- ++ "\r\n"
  )

postHandler :: Handler String
postHandler saddr url req = do
  putStrLn "<<- Got POST request ->>"
  putStr "saddr: "
  print saddr
  putStr "url: "
  print url
  putStr "req: "
  print req
  print (rqBody req)
  putStrLn ""

  res <- if url_path url /= "data"
   then return $ err_response Forbidden
   else if not $ checkAddr saddr
         then return $ err_response BadRequest
         else updateData (rqBody req)
               >> return (respond OK)

  putStrLn "<<- Sent response ->>"
  print res
  print (rspBody res)
  putStrLn ""

  return res

  where checkAddr (SockAddrInet gsport gsip) = True
        checkAddr _ = False

updateData :: String -> IO ()
updateData a = do
  putStrLn "New data updated"
  print a
  putStrLn ""
