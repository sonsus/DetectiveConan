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
servername = "Detective Connan Server"

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
handler saddr url req = do
  putStrLn "<<- Got request ->>"
  putStr "saddr: "
  print saddr
  putStr "url: "
  print url
  putStrLn "req: "
  print req
  print (rqBody req)
  putStrLn ""

  res <- case rqMethod req of
    GET  -> getHandler saddr url req
    POST -> postHandler saddr url req

  putStrLn "<<- Sent response ->>"
  print res
  print (rspBody res)
  putStrLn ""
  return res


getHandler :: Handler String
getHandler saddr url req = do
  case url_path url of
    "max_time" -> htmlResponse "max_time.html"
    "max_cpu"  -> htmlResponse "max_cpu.html"
    "avg_cpu"  -> htmlResponse "avg_cpu.html"
    _ -> return $ errResponse NotFound

postHandler :: Handler String
postHandler saddr url req
  | url_path url /= "data" = return $ errResponse Forbidden
  | not $ checkAddr saddr  = return $ errResponse BadRequest
  | otherwise = updateData (rqBody req) >> return (respond OK)
  where checkAddr (SockAddrInet gsport gsip) = True
        checkAddr _ = False

errResponse :: StatusCode -> Response String
errResponse sc =
  let (a,b,c) = statusCodeTriplet sc
      errorcode = show a ++ show b ++ show c
      message = errorcode ++ " " ++ reason sc
      html = "<html> <head> <title>" ++ message ++ "</title> </head>"
               ++ "<body> <h1>" ++ message ++ "</h1> </body> </html>"
  in Response (a,b,c) (reason sc)
       [ mkHeader HdrServer servername
       , mkHeader HdrConnection "close"
       ] html

htmlResponse :: String -> IO (Response String)
htmlResponse filename = withFile filename ReadMode (\file -> do
  contents <- hGetContents file
  return $ Response
    (statusCodeTriplet OK) (reason OK)
    [ mkHeader HdrServer servername
    , mkHeader HdrContentType "text/html"
    -- , mkHeader HdrContentLength "14"
    , mkHeader HdrConnection "close"
    ]
    contents -- ++ "\r\n"
  )

updateData :: String -> IO ()
updateData a = do
  putStrLn "New data updated"
  print a
  putStrLn ""