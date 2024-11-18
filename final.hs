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
  
findTime:: [Event]->Time->[Event]
findTime [] _ = []
findTime(x:xs) time 
  | current < timec =findTime xs time
  | current >= timec = xs
  where:
  current = timeConvertor(startTime x)
  timec = timeConvertor(startTime time)

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

compareLocation


checkTimeSlotAvailability :: Event -> ConferenceSchedule -> Bool
checkTimeSlotAvailability event schedule = all (\eventInSchedule -> compareTime event eventInSchedule) (events schedule)


generateEventReport :: ConferenceSchedule -> String
generateEventReport schedule = "Total events: " ++ length(events schedule) ++ "\nLocations: " ++ show byLocation ++ "\nSpeakers: " ++ show bySpeaker

showbyLocation :: [Events]->[Location]->String
showbyLocation [] _ = ""
showbyLocation _ [] = ""
showbyLocation (x:xs) (y:ys) 
  |locationID(place x) == locationId y = "location: "++ name y ++ " event: "++ name x ++"\n"++showbyLocation xs y ++showbyLocation ys x
  |otherwise showbyLocation xs y ++showbyLocation ys x

cities :: [Location]
cities =
  [ Location 1 "New York"
  , Location 2 "Los Angeles"
  , Location 3 "Chicago"
  , Location 4 "Houston"
  , Location 5 "Phoenix"
  ]

places :: [Place]
places =
  [ Place 1 101 "Central Park"
  , Place 1 102 "Times Square"
  , Place 1 103 "Statue of Liberty"
  , Place 1 104 "Empire State Building"
  , Place 1 105 "Brooklyn Bridge"
  , Place 2 201 "Hollywood Walk of Fame"
  , Place 2 202 "Venice Beach"
  , Place 2 203 "Santa Monica Pier"
  , Place 2 204 "Griffith Observatory"
  , Place 2 205 "Rodeo Drive"
  , Place 3 301 "Millennium Park"
  , Place 3 302 "Navy Pier"
  , Place 3 303 "The Art Institute of Chicago"
  , Place 3 304 "Willis Tower"
  , Place 3 305 "Lincoln Park Zoo"
  , Place 4 401 "Eiffel Tower"
  , Place 4 402 "Louvre Museum"
  , Place 4 403 "Notre-Dame Cathedral"
  , Place 4 404 "Champs-Élysées"
  , Place 4 405 "Sacre-Cœur"
  , Place 5 501 "Big Ben"
  , Place 5 502 "London Eye"
  , Place 5 503 "Buckingham Palace"
  , Place 5 504 "Tower Bridge"
  , Place 5 505 "British Museum"
  , Place 6 601 "Taj Mahal"
  , Place 6 602 "Qutub Minar"
  , Place 6 603 "Gateway of India"
  , Place 6 604 "Red Fort"
  , Place 6 605 "India Gate"
  ]
conferenceSchedules :: [ConferenceSchedule]
conferenceSchedules =
  [ ConferenceSchedule 101 (Location 1 "New York")
  , ConferenceSchedule 102 (Location 2 "Los Angeles")
  , ConferenceSchedule 103 (Location 3 "Chicago")
  , ConferenceSchedule 104 (Location 4 "Paris")
  , ConferenceSchedule 105 (Location 5 "London")
  ]

main :: IO()
main = do
