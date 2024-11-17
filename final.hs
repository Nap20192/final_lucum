data Time = Time{ 
  hour :: Int ,
  minute :: Int 
} deriving Show

data Location  = Location{
  locationID :: Int,
  places: [String]
} 

data Event = Event
  { 
  scheduleId :: Int,
  eventId   :: Int,
  name      :: String,
  startTime :: Time,
  endTime   :: Time,
  event_location  :: String,
  speaker   :: String
  } deriving Show

data ConferenceSchedule = ConferenceSchedule
  {
  scheduleId :: Int,
  conf_location   :: Location,
  events     :: [Event]
  } deriving Show

TimeCheck:: Time->Time->Time->Time->Bool

main :: IO()
main = do
