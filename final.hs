import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (find, delete)

data Event = Event {
  eventId :: Int,
  name :: String,
  startTime :: Time,
  endTime :: Time,
  eventLocation :: Place,
  speaker :: String
} deriving (Show, Eq)


data ConferenceSchedule = ConferenceSchedule {
  scheduleId :: Int,
  conferenceLocation :: Location,
  events :: [Event]
} deriving (Show)


data Time = Time {
  hour :: Int,
  minute :: Int
} deriving (Show, Eq)


data Place  = Place {
  placeId :: Int,
  locationID :: Int,
  places :: String
} deriving (Show, Eq)

data Location = Location {
  locationId :: Int,
  location :: String
} deriving (Show, Eq)


safeAdd :: Event -> ConferenceSchedule -> Either String ConferenceSchedule
safeAdd event schedule
  | elem event (events schedule) = Left "This Event is already in the schedule"
  | checkTimeSlotAvailability event schedule = Left "This time is already taken by another event"
  | otherwise = Right (ConferenceSchedule id location ((events schedule) ++ [event]))
  where
    id = scheduleId schedule
    location = conferenceLocation schedule

addEventToSchedule :: Event -> ConferenceSchedule -> ConferenceSchedule
addEventToSchedule event schedule = 
  case safeAdd event schedule of
    Right updatedSchedule -> updatedSchedule
    Left errorMsg -> schedule
  
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
checkTimeSlotAvailability event schedule = any (\eventInSchedule -> (compareTime event eventInSchedule) && (placeId (eventLocation event) == placeId(eventLocation eventInSchedule)) && (locationID (eventLocation event) == locationID (eventLocation eventInSchedule))) (events schedule)


findEventById :: Int -> ConferenceSchedule -> Maybe Event
findEventById id schedule = find (\event -> eventId event == id) (events schedule)

updateEventName :: Int -> ConferenceSchedule -> String -> ConferenceSchedule
updateEventName id schedule newName =
  case findEventById id schedule of
    Just event -> 
      schedule {events = updatedEvents}
      where
        updatedEvent = event {name = newName}
        updatedEvents = map (\eventInSchedule -> if eventId eventInSchedule == id then updatedEvent else eventInSchedule) (events schedule)
    Nothing -> schedule

updateEventLocation :: Int -> ConferenceSchedule -> Place -> ConferenceSchedule
updateEventLocation id schedule newPlace =
  case findEventById id schedule of
    Just event ->
      schedule {events = updatedEvents}
      where
        updatedEvent = event {eventLocation = newPlace}
        updatedEvents = map (\eventInSchedule -> if (eventId eventInSchedule == id && (checkTimeSlotAvailability updatedEvent schedule)) then updatedEvent else eventInSchedule) (events schedule)
    Nothing -> schedule

updateEventStartTime:: Int -> ConferenceSchedule -> Time -> ConferenceSchedule
updateEventStartTime id schedule newTime =
  case findEventById id schedule of
    Just event ->
      schedule {events = updatedEvents}
      where
        updatedEvent = event {startTime = newTime}
        updatedEvents = map (\eventInSchedule -> if (eventId eventInSchedule == id && (checkTimeSlotAvailability updatedEvent schedule)) then updatedEvent else eventInSchedule) (events schedule)
    Nothing -> schedule

updateEventEndTime:: Int -> ConferenceSchedule -> Time -> ConferenceSchedule
updateEventEndTime id schedule newTime =
  case findEventById id schedule of
    Just event ->
      schedule {events = updatedEvents}
      where
        updatedEvent = event {endTime = newTime}
        convertedNewTime = timeConvertor newTime
        updatedEvents = map (\eventInSchedule -> if (eventId eventInSchedule == id && (checkTimeSlotAvailability updatedEvent schedule) && not(convertedNewTime <= timeConvertor(startTime eventInSchedule))) then updatedEvent else eventInSchedule) (events schedule)
    Nothing -> schedule

checkLocation :: Event -> Location -> Bool
checkLocation e l = locationId l == locationID (eventLocation e)

showbyLocation :: [Event] -> [Location] -> String
showbyLocation _ [] = ""
showbyLocation events (loc:locs) =
  "Location: " ++ location loc ++ ", Events: " ++ show (length (filter (`checkLocation` loc) events)) ++ "\n"
  ++ showbyLocation events locs



generateEventReport :: [ConferenceSchedule] -> [Location]->[String]->String
generateEventReport [] _  _= ""
generateEventReport (x:xs) l s= "id: "++show(scheduleId x)++"\n"++"Total events: " ++ show(length(events x))  ++"\n"++ showbyLocation (events x) l++ "\n" ++"\nSpeakers: \n" ++ showSpeakers (events x) s ++ generateEventReport xs l s


removeDuplicates :: [String] -> [String]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise   = x : removeDuplicates xs

listSpeakers :: [Event]->[String]
listSpeakers [] = [] 
listSpeakers (x:xs) = (speaker x : listSpeakers xs)
location1 = Location 1 "Castle"

setSpeakers :: [String]->[String]
setSpeakers a = removeDuplicates a

checkSpeaker :: Event -> String -> Bool
checkSpeaker e s = speaker e == s

showSpeakers:: [Event] -> [String]->String
showSpeakers _ [] = ""
showSpeakers events (s:ss) ="Speaker: " ++  s ++ ", Amount: " ++ show (length (filter (`checkSpeaker` s) events)) ++ "\n" ++ showSpeakers events ss


bedroom = Place 1 1 "Bedroom"

seriousDiscussionsRoom = Place 2 1 "Seriuos Discussion Room"


location2 = Location 2 "Mansion"

vegasRoom = Place 1 1 "Las Vegas Room"

gothamRoom = Place 2 1 "Gotham Room"


location3 = Location 3 "Tokyo"

shibuyaDistrict = Place 1 3 "Shibuya District"

locations = [location1,location2,location3]

event1 = Event 1 "How to be ninja" (Time 9 0) (Time 10 0) vegasRoom "R. H."

event2 = Event 2 "What it's like to be Batman" (Time 9 30) (Time 10 30) gothamRoom "Nikolay Kogay"

event3 = Event 3 "Itroduction to Advanced Lying in Bed" (Time 11 0) (Time 12 0) bedroom "Sleeping Beauty"

event4 = Event 4 "1 Billion Lions vs Every Pokemon: Let's Settle This Once and For All" (Time 13 0) (Time 14 0) seriousDiscussionsRoom "J"

event5 = Event 5 "What it's like to be Batman" (Time 9 30) (Time 10 30) gothamRoom "Nikolay Kogay"

event6 = Event 6 "Gojo Satoru's Arrival" (Time 20 31) (Time 21 26) shibuyaDistrict "Gojo Satoru"

event7 = Event 7 "What " (Time 21 27) (Time 22 30) gothamRoom "Nikolay Kogay"


schedule :: ConferenceSchedule
schedule = ConferenceSchedule 1  location1 [event1]

main :: IO ()
main = do
  let updatedSchedule1 = addEventToSchedule event2 schedule
  print updatedSchedule1
  putStrLn "\n"

  let updatedSchedule2 = addEventToSchedule event3 updatedSchedule1
  print updatedSchedule2
  putStrLn "\n"

  let updatedSchedule3 = addEventToSchedule event1 updatedSchedule2
  print updatedSchedule3
  putStrLn "\n"

  let updatedSchedule4 = addEventToSchedule event4 updatedSchedule3
  print updatedSchedule4
  putStrLn "\n"

  let updatedSchedule5 = addEventToSchedule event5 updatedSchedule4
  print updatedSchedule5

  let updatedSchedule6 = updateEventName 1 updatedSchedule5 "Don't be Ninja"
  print updatedSchedule6
  putStrLn "\n"

  let updatedSchedule7 = updateEventLocation 2 updatedSchedule6 vegasRoom
  print updatedSchedule7
  putStrLn "\n"

  let updatedSchedule8 = updateEventStartTime 4 updatedSchedule7 (Time 11 0)
  print updatedSchedule8
  putStrLn "\n"

  let updatedSchedule9 = updateEventEndTime 4 updatedSchedule8 (Time 11 0)
  print updatedSchedule9
  putStrLn "\n"


  let updatedSchedule10 = addEventToSchedule event7 updatedSchedule9
  print updatedSchedule10

  let schedules = [updatedSchedule1, updatedSchedule10]
 
  let a = listSpeakers (events updatedSchedule10)
  let aa = setSpeakers a 

  putStrLn(generateEventReport schedules locations aa) 
