data Time = Time{ 
  hour :: Int ,
  minute :: Int 
} deriving Show

data place  = place{
  locationID :: Int,
  placeId :: Int,
  places: String
}
data Location = Location{
  locationId :: Int,
  location:: String
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


main :: IO()
main = do
