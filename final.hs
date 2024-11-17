data Time = Time{ 
  hour :: Int ,
  minute :: Int 
} deriving Show

data Place  = Place{
  locationID :: Int,
  placeId :: Int,
  place: String
}deriving Show

data Location = Location{
  locationId :: Int,
  location:: String
} deriving Show

data Event = Event
  { 
  scheduleId :: Int,
  eventId   :: Int,
  name      :: String,
  startTime :: Time,
  endTime   :: Time,
  place  :: Place,
  speaker   :: String
  } deriving Show

data ConferenceSchedule = ConferenceSchedule
  {
  scheduleId :: Int,
  location   :: Location,
  } deriving Show


safeAdd :: Event -> ConferenceSchedule -> Either String ConferenceSchedule
safeAdd event schedule
  | checkTimeSlotAvailability event schedule = Left "This time is already taken by another event"
  | otherwise = Right (ConferenceSchedule id location ([event] ++ (events schedule)))
  where
    id = scheduleId schedule
    location = conferenceLocation schedule


addEventToSchedule :: Event -> ConferenceSchedule -> Either String ConferenceSchedule
addEventToSchedule event schedule = (safeAdd event schedule)
  


timeConvertor :: Time -> Double
timeConvertor (Time h m) = fromIntegral h + (fromIntegral m)*0.01 

compareTime :: Event -> Event -> Bool
compareTime event1 event2 
  | (start1 >= start2) && (start1 < end2) = True
  | (end1 >= start2) && (end1 <= end2) = True
  | otherwise = False
  where
    start1 = timeConvertor(startTime event1)
    start2 = timeConvertor(startTime event2)
    end1 = timeConvertor(endTime event1)
    end2 = timeConvertor(endTime event2)


checkTimeSlotAvailability :: Event -> ConferenceSchedule -> Bool
checkTimeSlotAvailability event schedule = all (\eventInSchedule -> compareTime event eventInSchedule) (events schedule)



main :: IO()
main = do
