data Location = Location { locationId :: Int, location :: String } deriving Show
data Place = Place { locationID :: Int, placeId :: Int, lace :: String } deriving Show
data Event = Event 
    { eventId   :: Int
    , name      :: String
    , startTime :: String
    , endTime   :: String
    , place     :: Place
    , speaker   :: String
    } deriving Show

locations :: [Location]
locations = 
  [ Location 1 "Main Hall"
  , Location 2 "Conference Room A"
  , Location 3 "Outdoor Stage"
  ]

events :: [Event]
events = 
  [ Event 101 "Keynote Speech" "10:00" "11:00" (Place 1 201 "Main Hall") "Dr. Smith"
  , Event 102 "Workshop on AI" "12:00" "14:00" (Place 2 202 "Conference Room A") "Prof. Johnson"
  , Event 103 "Music Concert" "18:00" "20:00" (Place 3 203 "Outdoor Stage") "Band XYZ"
  , Event 104 "Unknown Event" "15:00" "16:00" (Place 4 204 "Unknown Room") "Unknown Speaker"
  ]


showbyLocation :: [Event] -> [Location] -> String
showbyLocation [] _ = ""
showbyLocation (x:xs) locations = 
  case find (\loc -> locationId loc == locationID (place x)) locations of
    Just loc -> "Location: " ++ location loc ++ ", Event: " ++ eventName x ++ "\n" ++ showbyLocation xs locations
    Nothing  -> "Location: Unknown, Event: " ++ eventName x ++ "\n" ++ showbyLocation xs locations

main :: IO ()
main = do 
    putStrLn $ showbyLocation events locations