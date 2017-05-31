{-# LANGUAGE OverloadedStrings #-}

module Analyze where


import Data.Aeson
import Data.Semigroup
import Data.Map.Lazy hiding (map, foldl, foldr)
import Data.Function
import Data.List
import Data.Time.Calendar
import Data.ByteString.Lazy.Char8 (pack)

data PeakPoint = PeakPoint
  { time_stamp :: TimeStamp
  , act_data :: [Activity]
  } deriving (Show, Read, Eq)

data TimeStamp = TimeStamp
  { date_stamp :: Day
  , second_stamp :: Int
  } deriving (Show, Read, Eq)

data Activity = Activity
  { proc_name :: String
  , cpu_usage :: Float
  , user_id :: String
  } deriving (Show, Read, Eq)


instance FromJSON PeakPoint where
  parseJSON = withObject "PeakPoint" $ \obj -> PeakPoint
    <$> obj .: "tstmp"
    <*> obj .: "data"

instance ToJSON PeakPoint where
  toJSON (PeakPoint ts da) =
    object [ "tstmp" .= ts, "data" .= da ]
  toEncoding (PeakPoint ts da) =
    pairs ( "tstmp" .= ts <> "data" .= da )


instance FromJSON TimeStamp where
  parseJSON = withObject "TimeStamp" $ \obj -> do
    y <- obj .: "yr"
    m <- obj .: "month"
    d <- obj .: "date"
    s <- obj .: "second"
    case fromGregorianValid y m d of
      Just j -> return $ TimeStamp j s
      Nothing -> fail "expected valid time"

instance ToJSON TimeStamp where
  toJSON (TimeStamp j s) = let (y, m, d) = toGregorian j in object
    [ "yr"    .= y
    , "month" .= m
    , "date"  .= d
    , "second".= s
    ]
  toEncoding (TimeStamp j s) = let (y, m, d) = toGregorian j in pairs
    (  "yr"    .= y
    <> "month" .= m
    <> "date"  .= d
    <> "second".= s
    )


instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \obj -> Activity
    <$> obj .: "proc"
    <*> obj .: "cpu"
    <*> obj .: "user"

instance ToJSON Activity where
  toJSON (Activity u c p) = object
    [ "user" .= u
    , "cpu"  .= c
    , "proc" .= p
    ]
  toEncoding (Activity u c p) = pairs
    (  "user" .= u
    <> "cpu"  .= c
    <> "proc" .= p
    )

nullTimeStamp :: TimeStamp
nullTimeStamp =  TimeStamp
  { date_stamp = fromGregorian 0 0 0
  , second_stamp = 0
  }

timediff :: TimeStamp -> TimeStamp -> Int
timediff (TimeStamp j1 s1) (TimeStamp j2 s2) =
  24 * 60 * 60 * (fromInteger $ diffDays j1 j2) + (s2 - s1)


data UserSpec = UserSpec
  { total_time :: Int
  , total_cpu  :: Float
  , max_cpu    :: Float
  , max_proc   :: String
  , max_time   :: TimeStamp
  } deriving (Show, Read, Eq)

type UserData = Map String UserSpec

-- Map user (total time, total cpu, max cpu, proc, time_stamp)
analyze :: [PeakPoint] -> UserData
analyze [] = empty
analyze pps =
      -- dump element to accumulate all data
  let bootstrap = (head pps) { act_data = [] }
      diffs = zip (bootstrap:pps) pps
   in foldl (flip accumulate) empty diffs

accumulate :: (PeakPoint, PeakPoint) -> UserData -> UserData
accumulate (oldPP, newPP) ud =
  let residents = (intersect `on` nub.map user_id.act_data) oldPP newPP
      elapse = (timediff `on` time_stamp) oldPP newPP
      ud' = foldr (alter $ accUseTime elapse) ud residents
      newtime = time_stamp newPP
   in foldr (alterCpu newtime) ud' $ act_data newPP

accUseTime :: Int -> Maybe UserSpec -> Maybe UserSpec
accUseTime t (Just us) =
  let tt = total_time us
   in Just us { total_time = tt + t }
accUseTime t Nothing = -- the user first apears
  Just $ UserSpec t 0 0 "" nullTimeStamp

alterCpu :: TimeStamp -> Activity -> UserData -> UserData
alterCpu ts (Activity ps cpu u) = alter accCpu u
  where accCpu (Just us') =
          let tcpu = total_cpu us' + cpu
              mcpu = max_cpu us'
              us = us' { total_cpu = tcpu }
          in Just $ if mcpu > cpu then us
                    else us { max_cpu = cpu
                            , max_proc = ps
                            , max_time = ts
                            }
        --h Nothing = undefined -- `accumulate` already handled
        accCpu Nothing = Nothing

ff :: IO [PeakPoint]
ff = readFile "json.json"
  >>= (\(Right x) -> return x) . eitherDecode . pack
