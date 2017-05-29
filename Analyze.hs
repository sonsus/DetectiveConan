{-# LANGUAGE OverloadedStrings #-}

module Analyze where


import Data.Aeson
import Data.Semigroup
import Data.Map.Lazy hiding (map, foldl, foldr)
import Data.Function
import Data.List
import Data.Time.Calendar

data PeakPoint = PeakPoint
  { time_stamp :: TimeStamp
  , act_data :: [Activity]
  } deriving (Show, Read, Eq)

data TimeStamp = TimeStamp
  { date_stamp :: Day
  , second_stamp :: Int
  } deriving (Show, Read, Eq)

data Activity = Activity
  { proc_name :: Maybe String
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
    <$> obj .: "user"
    <*> obj .: "cpu"
    <*> obj .: "proc"

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

timediff :: TimeStamp -> TimeStamp -> Int
timediff (TimeStamp j1 s1) (TimeStamp j2 s2) =
  24 * 60 * 60 * (fromInteger $ diffDays j1 j2) + (s2 - s1)


type UserSpec = Map String (Int, Float, Float)

-- Map user (total time, total cpu, max cpu)
analyze :: [PeakPoint] -> UserSpec
analyze [] = empty
analyze pps =
      -- dump element to accumulate all data
  let bootstrap = (head pps) { act_data = [] }
      diffs = zip (bootstrap:pps) pps
   in foldl (flip accumulate) empty diffs

accumulate :: (PeakPoint, PeakPoint) -> UserSpec -> UserSpec
accumulate (oldPP, newPP) us =
  let residents = (intersect `on` nub.map user_id.act_data) oldPP newPP
      elapse = (timediff `on` time_stamp) oldPP newPP
      us' = foldl (flip $ alter $ f elapse) us residents
   in foldl (flip uptAct) us' $ act_data newPP
  where f t (Just (u,c,m)) = Just (u+t,c,m)
        f t Nothing = Just (0,0,0) -- the user first apears

uptAct :: Activity -> UserSpec -> UserSpec
uptAct a = alter (h $ cpu_usage a) $ user_id a
  where h cpu (Just (u,c,m)) = Just (u, c + cpu, max m cpu)
        h cpu Nothing = Just (0,cpu,cpu) -- the user first apears
