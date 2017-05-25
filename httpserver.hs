import Network.HTTP.Server
import Network.HTTP.Server.Logger

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
