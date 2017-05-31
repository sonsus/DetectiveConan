import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.Socket
import Network.URL
import Network
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Time.Calendar (showGregorian)
import Data.Map (toList)
import Data.List (sortBy, intercalate)
import Data.Ord (comparing)
import Control.Monad (zipWithM_)

import Analyze
import Data.Aeson (eitherDecode)

import Html

-- macro-like values
-- address of python deamon (= ip of GS server)
gsip = 0

-- name of this server
servername = "Detective Connan Server"


main :: IO ()
main = serverWith defaultConfig
  { srvLog = stdLogger -- log to stdout
  , srvHost = "0.0.0.0" -- open to all client
  --, srvHost = 172.17.0.86.* -- open to the subnet
  , srvPort = 8004
  } handler


-- handle http requests
handler :: Handler String
handler saddr url req = do
  -- log to stdout
  putStrLn "<<- Got request ->>"
  putStr "saddr: "
  print saddr
  putStr "url: "
  print url
  putStrLn "req: "
  print req
  print (rqBody req)
  putStrLn ""

  -- handle only GET and POST method request
  res <- case rqMethod req of
    GET  -> getHandler saddr url req
    POST -> postHandler saddr url req

  -- log to stdout
  putStrLn "<<- Sent response ->>"
  print res
  print (rspBody res)
  putStrLn ""
  return res


-- http handler for get request
-- send generated html files
getHandler :: Handler String
getHandler saddr url req = do
  case url_path url of
    "Use_time" -> htmlResponse "Use_Time.html"
    "avg_cpu"  -> htmlResponse "Total_Cpu.html"
    "max_cpu"  -> htmlResponse "Max_Cpu.html"
    "css.css"  -> htmlResponse "css.css"
    _ -> return $ errResponse NotFound


-- http handler for post request
-- get's json value from python deamon,
-- reproduce html files
-- send mail to top recorders
postHandler :: Handler String
postHandler saddr url req
  | url_path url /= "data" = return $ errResponse Forbidden
  | not $ checkAddr saddr  = return $ errResponse BadRequest
  | otherwise = case (eitherDecode . pack . rqBody $ req) of
      Left err -> do
        putStrLn "recieved corrupted json data:"
        print err
        return $ errResponse Conflict
      Right js -> do
        putStrLn "recieved json data"
        updateData js
        return $ respond Accepted
  where checkAddr (SockAddrInet _ gsip) = True
        checkAddr _ = False


-- construct simple http respond with given error code
errResponse :: StatusCode -> Response String
errResponse sc =
  let (a,b,c) = statusCodeTriplet sc
      errorcode = show a ++ show b ++ show c
      message = errorcode ++ " " ++ reason sc
      html = unpack $ genErrPage message
  in Response (a,b,c) (reason sc)
       [ mkHeader HdrServer servername
       , mkHeader HdrConnection "close"
       ] html


-- construct simple http respond containing the argument as data
-- used to send html files in response to get request
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


-- format cpu usage (float) into pretty string (shown on html table)
formatCpu :: Float -> String
formatCpu cpu =
  let int = floor cpu
      fra = floor (cpu*10) - int
   in show int ++ "." ++ show fra


-- format TimeStamp into pretty string (shown on html table)
formatTime :: TimeStamp -> String
formatTime (TimeStamp dt s) =
  -- remove leading "20" or "19"
  let date = drop 2 $ showGregorian dt
      (s', sec) = s `divMod` 60
      (s'', mnt) = s' `divMod` 60
      (s''', hrs) = s'' `divMod` 60
      ssec = f $ show sec
      mmnt = f $ show mnt
      hhrs = f $ show hrs
   in date ++ "; " ++ hhrs ++ ":" ++ mmnt ++ ":" ++ ssec
  where f [] = "00"
        f [a] = ['0',a]
        f a = a

-- format second (int) into human readable string (shown on html table)
formatSec :: Int -> String
formatSec sec' =
  let (min', sec) = sec' `divMod` 60
      (hrs', min) = min' `divMod` 60
      (day , hrs) = hrs' `divMod` 60
      coef = dropWhile (==0) [day, hrs, min, sec]
      strs = zipWith f (reverse coef) (words "sec min hrs day")
      str = intercalate ", " strs
   in if null str then "0 sec" else str
  where f n s = show n ++ " " ++ s


type US = (String, UserSpec)


-- update data when post request arrived
-- analyze data, generate html files and send mails
updateData :: [PeakPoint] -> IO ()
updateData js =
  let us = toList $ analyze js
      procdat =
        [ ("Use_Time", "TOTAL USING TIME TOP RECORDERS"
          , comparing (total_time.snd), ts
          , ["rank", "user id", "total time"])
        , ("Total_Cpu", "TOTAL CPU USAGE TOP RECORDERS"
          , comparing (total_cpu.snd), cs
          , ["rank", "user id", "total cpu"])
        , ("Max_Cpu", "MAX CPU USAGE TOP RECORDERS"
          , comparing (max_cpu.snd), ms
          , ["rank","user id","max cpu usage","command","timestamp"])
        ]
   in mapM_ (\(a,b,c,d,e) -> do
        let tabledat = e : zipWith f [1..] (top20 c d us)
        updateHtml (a,b) tabledat
        --sendMail a . take 3 . map tail . tail $ tabledat
      ) procdat
  where f n s = show n : s
        ts (user,us) = [user, formatSec.total_time $ us]
        cs (user,us) = [user, formatCpu.total_cpu $ us]
        ms (user,us) = [user, formatCpu.max_cpu $ us,
                        max_proc us, formatTime.max_time $ us]
        top20 :: (US -> US -> Ordering) -> (US -> [String])
              -> [US] -> [[String]]
        top20 ext form = take 20 . map form . sortBy ext


-- update html file with given table data
updateHtml :: (String,String) -> [[String]] -> IO ()
updateHtml p@(title, headline) dat =
    writeFile (title ++ ".html") . unpack . genScoreBoard p $ dat


-- generate mail content for sending to 3 top recorders
mailContent :: String -> Int -> IO String
mailContent field n = do
  let place = ["", "1st", "2nd", "3rd"] ++ map ((++"st").show) [4 .. ]
  conan <- readFile "conan.txt"
  return $ conan ++ "\n" ++
    "Congrats!" ++ "\n" ++
    "You recorded " ++ (place !! n) ++ " place in " ++ field ++
    " on CS server this week." ++ "\n" ++
    "Beware not to monopolize the server if your job requires " ++
    "bunch of resources."


-- send mail with generated contents to 3 top recorders
sendMail :: String -> [[String]] -> IO ()
sendMail field dat =
  let users = map head dat
   in zipWithM_ f [1..] users
  where f place user =
          let rmail = user ++ "@gist.ac.kr"
              smail = "prize@detective.conan"
              title = "Hi I'm Connan"
              conte = mailContent field place
           in conte >>= putSMTP smail rmail title

-- interact with mail server using smtp prtocol
putSMTP :: String -> String -> String -> String -> IO Bool
putSMTP send recv title cont = withSocketsDo $ do
  let ('@':mserv) = dropWhile (/='@') recv
  handle <- connectTo ("mail."++mserv) (PortNumber 25)
  putStrLn ("mail."++mserv)
  let c = checkAns handle
      h = \s -> putStrLn ("C: " ++ s) >> hPutStrLn handle s
  c 220
  h $ "HELO " ++ mserv
  c 250
  c 250
  c 250
  h $ "MAIL FROM: <" ++ send ++ ">"
  c 250
  h $ "RCPT TO: <" ++ recv ++ ">"
  c 250
  h $ "DATA"
  c 354
  h $ "From: " ++ send
  h $ "To: " ++ recv
--  h $ "Date"
  h $ "Subject: " ++ title
  h $ ""
  h $ cont
  hPutStr handle "\r\n.\r\n"
  putStrLn "."
  c 250
  h $ "QUIT"
  c 221
  return True
  where checkAns h n = do
          line <- hGetLine h
          putStrLn ("S: " ++ line)
          case reads line of
            (m,_):_ -> if m == n then return ()
                         else fail "server malfunction"
            _ -> fail "server malfunction"
