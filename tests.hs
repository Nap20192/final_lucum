-- Assuming these data types
data Time = Time { hour :: Int, minute :: Int } deriving (Show)
data Place = Place { placeId :: Int, locationID :: Int, places :: String } deriving (Show)
data Location = Location { locationId :: Int, location :: String } deriving (Show)

data Event = Event { eventId :: Int, name :: String, startTime :: Time, endTime :: Time, eventLocation :: Place, speaker :: String } deriving (Show, Eq)

data ConferenceSchedule = ConferenceSchedule { scheduleId :: Int, conferenceLocation :: Location, events :: [Event] } deriving (Show)

-- SafeAdd function (with simple checks)
safeAdd :: Event -> ConferenceSchedule -> Either String ConferenceSchedule
safeAdd event schedule
  | elem event (events schedule) = Left "Event already in schedule"
  | otherwise = Right $ ConferenceSchedule (scheduleId schedule) (conferenceLocation schedule) (event : events schedule)

-- handleEvent function
handleEvent :: Event -> ConferenceSchedule -> Either String [Event]
handleEvent event schedule = fmap events (safeAdd event schedule)

-- Test Data
event1 = Event 1 "Tech Talk" (Time 10 0) (Time 11 0) (Place 1 1 "Main Hall") "John Doe"
event2 = Event 2 "AI Workshop" (Time 12 0) (Time 14 0) (Place 2 1 "Room 101") "Jane Smith"
schedule = ConferenceSchedule 1 (Location 1 "Conference Center") [event1]

-- Test the function
main :: IO ()
main = do
  let result1 = handleEvent event2 schedule
  case result1 of
    Right updatedEvents -> putStrLn $ "Updated events: " ++ show updatedEvents
    Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
