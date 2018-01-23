module Main where
import Data.Char
import System.Console.ANSI
import System.Directory

data DateTime = DateTime { day :: Int
                        , month :: Int
                        , year :: Int
                        , hours :: Int
                        , minutes :: Int
                        } deriving (Show)

data Event = Event { idx :: Int
                , date :: DateTime
                , event :: String
                } deriving (Show)

main :: IO ()
main = do
    clearScreen
    printMenu [] "init"

help :: [Event] -> IO () 
help ev = do
    putStrLn "1. Add event:"
    putStrLn "   ∫ follow the rules to avoid mistakes"
    putStrLn "   ∫ it is a sequential process so you must follow the sequence (there is no other way)"
    putStrLn "   ∫ the process can not be interrupted"
    putStrLn "   ∫ if you made a mistake then just finish entering data and start again\n"
    putStrLn "   Ω event name:  any characters except ';'"
    putStrLn "   Ω day:         only digits [1..31]"
    putStrLn "   Ω month:       only digits [1..12]"
    putStrLn "   Ω year:        only digits [2018..3018]"
    putStrLn "   Ω hours:       only digits [0..23]"
    putStrLn "   Ω minutes:     only digits [0..59]\n"
    putStrLn "   ≈ if the name of the event contains ';','[',']' then will be shown the following message: ';','[',']' is not allowed..."
    putStrLn "   ≈ if something is entered incorrectly then: Invalid date/time format..."
    putStrLn "   ≈ otherwise will be shown initial menu\n"
    putStrLn "2. Print events:"
    putStrLn "   ∫ select the sort type you need\n"
    putStrLn "   Ω no sort:                         in the order of adding events"
    putStrLn "   Ω sort alphabetically (asc):       by first letter ascending"
    putStrLn "   Ω sort alphabetically (desc):      by first letter descending"
    putStrLn "   Ω sort by datetime (asc):          by date and time ascending"
    putStrLn "   Ω sort by datetime (desc):         by date and time descending\n"
    putStrLn "   ≈ if events list is empty you will not be able to sort them and show (it is logical)\n"
    putStrLn "3. Remove events:"
    putStrLn "   ∫ print events (any sort)"
    putStrLn "   ∫ remember the event indexes you want to remove"
    putStrLn "   ∫ enter the desired indexes (separate by a comma, as example: 1,3,5)"
    putStrLn "   ∫ enter '0' to remove all events"
    putStrLn "   ∫ then you must confirm your choice"
    putStrLn "   ∫ enter 'y' if you want to remove events, otherwise 'n' (small or big letter - no matter)\n"
    putStrLn "   ≈ if the delimeter is not a comma then will be shown the following message: The delimeter is not a comma..."
    putStrLn "   ≈ if the format is not what is specified or/and the string contains unresolved characters "
    putStrLn "     ≈ (like: -,:,a, etc.) then: The string contains unresolved characters..." 
    putStrLn "   ≈ if the string contains '0' and something else then: '0' and other numbers are not compatible..."
    putStrLn "   ≈ if no one of the indexes exists then: No event from the list exists..."
    putStrLn "   ≈ if the answer is 'y' then: Successfully removed"
    putStrLn "   ≈ otherwise: Was not removed\n"
    putStrLn "4. Import"
    putStrLn "   ∫ first of all you should enter the name of the file (any allowed characters for file naming in unix)"
    putStrLn "   ∫ then you must confirm your choice"
    putStrLn "   ∫ enter 'y' if you want to import events, otherwise 'n' (small or big letter - no matter)\n"
    putStrLn "   ≈ if the answer is 'y' then will be shown the following message: Successfully imported" 
    putStrLn "   ≈ otherwise: Was not imported"   
    putStrLn "   ≈ if the file is empty you will not be able to import its contents (it is logical)"
    putStrLn "   ≈ if the file structure is not what is specified or/and the file contains unresolved characters"
    putStrLn "     ≈ then will be shown the following message: Something went wrong, check the file\n"
    putStrLn "   !!! import process will remove all actual events, be careful\n"
    putStrLn "5. Export"
    putStrLn "   ∫ first of all you should enter the name of the file (any allowed characters for file naming in unix)"
    putStrLn "   ∫ then you must confirm your choice"
    putStrLn "   ∫ enter 'y' if you want to export events, otherwise 'n' (small or big letter - no matter)\n"
    putStrLn "   ≈ if the answer is 'y' then will be shown the following message: Successfully exported"
    putStrLn "   ≈ otherwise: Was not exported"
    putStrLn "   ≈ if events list is empty you will not be able to export them (it is logical)\n"
    putStrLn "   !!! if such file already exists, it will be replaced by a new one\n"
    putStrLn "6. Help"
    putStrLn "   ∫ there is nothing to say...\n"
    putStrLn "7. Exit"
    putStrLn "   ∫ after this step all not exported events will be removed"
    putStrLn "   ∫ so do not forget to export them"
    putStrLn "  "
    finish ev ""
  
add :: [Event] -> IO ()
add ev = do
    putStrLn "Event name: "
    event <- getLine
    clearScreen
    putStrLn "Day: [1-31]"
    dd <- getLine
    clearScreen
    putStrLn "Month: [1-12]"
    mm <- getLine
    clearScreen
    putStrLn "Year: [2018-3018]"
    yy <- getLine
    clearScreen
    putStrLn "Hours: [0-23]"
    hh <- getLine
    clearScreen
    putStrLn "Minutes: [0-59]"
    mn <- getLine
    clearScreen
    if length (filter (\x -> x ==';' || x == ',' || x == '[' || x == ']') event) > 0
    then do
        putStrLn "';','[',']' is not allowed..."
        finish ev ""
    else do
        if (isInRange dd 1 31) &&
            (isInRange mm 1 12) &&
            (isInRange yy 2018 3018) &&
            (isInRange hh 0 23) &&
            (isInRange mn 0 59)
        then do
            let time = DateTime (read dd :: Int) 
                                (read mm :: Int) 
                                (read yy :: Int)
                                (read hh :: Int)
                                (read mn :: Int)

            let newEvent  = Event ((getLastId ev) + 1) time event
            let evt = ev ++ [newEvent]
            printMenu evt ""            
        else do
            putStrLn "\nInvalid date/time format..."
            finish ev ""
  
remove :: [Event] -> IO ()
remove ev = do
    if length ev == 0
    then do
        putStrLn "There is nothing to remove..."
        finish ev ""
    else do
        putStrLn "Please enter events indexes: "
        ids <- getLine
        clearScreen
        if length ids < 1
        then do
            remove ev
        else do
            let idslst = commaDelimitedStrToLstInt ids
            let lastid = last idslst
            if lastid < 0
            then do
                if lastid == -3
                then do
                    putStrLn "'0' and other numbers are not compatible..."
                else if lastid == -2
                then do
                    putStrLn "The string contains unresolved characters..."
                else do
                    putStrLn "The delimeter is not a comma..."
                finish ev ""
            else do
                let is1exists = isAtLeastOneEventExists ev idslst
                if is1exists
                then do
                    printEventsByIds ev idslst
                    putStrLn "\nRemove this events? (y/n)"
                    yn <- getLine
                    clearScreen
                    if (head yn == 'Y') || (head yn == 'y')
                    then do
                        let evts = removeEventsByIds ev idslst
                        putStrLn "Successfully removed"
                        finish evts ""
                    else do
                        putStrLn "Was not removed"
                        finish ev ""                    
                else do
                    putStrLn "No event from the list exists..."
                    finish ev ""
  
shw :: [Event] -> IO ()
shw ev = do
    if length ev /= 0
    then do
        putStrLn ("1. No sort\n"
            ++ "2. Sort alphabetically (asc)\n"
            ++ "3. Sort alphabetically (desc)\n"
            ++ "4. Sort by datetime (asc)\n"
            ++ "5. Sort by datetime (desc)\n")
        putStrLn "Enter the option: "
        opt <- getLine
        clearScreen
        if isNum opt
        then do
            if isInRange opt 1 5
            then do
                putStrLn "Index. [Event] [End Date/Time]"
                printEvents (sort ev opt) 0
                putStrLn " "
            else do
                shw ev
        else do
            putStrLn ("\n" ++ opt ++ " is not a number")
    else do
        putStrLn "There is nothing to show..."
    finish ev ""

imprt :: [Event] -> IO ()
imprt ev = do
    putStrLn "File name: "
    file <- getLine
    clearScreen
    fileExists <- doesFileExist file
    if fileExists
    then do
        putStrLn ("do you want to import all events from the file '"
            ++ file ++ "'' ? (y/n)")    
        yn <- getLine
        clearScreen
        if (head yn == 'Y') || (head yn == 'y')
        then do
            content <- readFile file
            if length content == 0
            then do
                putStrLn "the file is empty..."
                finish ev ""
            else do
                let str = splt (fst (splitAt (length content - 2) content))
                let str1 = splt content
                let evs = importF str1
                if length evs == 0
                then do
                    putStrLn "Something went wrong, check the file"
                else do
                    putStrLn "Successfully imported"
                    finish evs ""
        else do
            putStrLn "Was not imported"
    else do
        putStrLn "The file does not exist"
    finish ev ""

exprt :: [Event] -> IO ()
exprt ev = do
    if length ev == 0
    then do
        putStrLn "There is nothing to export..."
    else do
        putStrLn "File name: "
        file <- getLine
        clearScreen
        putStrLn ("do you want to export all events to a file '"
            ++ file ++ "'' ? (y/n)")
        yn <- getLine
        clearScreen
        if (head yn == 'Y') || (head yn == 'y')
        then do
            exportF file ev
            putStrLn "Successfully exported"
        else do
            putStrLn "Was not exported"
    finish ev ""

exit :: [Event] -> IO ()
exit ev = do
    return ()

printMenu :: [Event] -> [Char] -> IO ()
printMenu ev cond = do
    clearScreen
    if cond /= "repeat"
    then 
        putStrLn ("1. Add\n2. "
        ++ "Show\n3. Remove\n4. " 
        ++ "Import\n5. Export\n6. " 
        ++ "Help\n7. Exit")
    else
        putStrLn ""
    putStrLn "Enter the option (text or number): "
    opt <- getLine
    clearScreen
    menu ev (toLowerStr opt)

wrong :: [Event] -> IO ()
wrong ev = do
    printMenu ev ""

menu :: [Event] -> [Char] -> IO ()
menu ev "1"      = add ev
menu ev "add"    = add ev
menu ev "2"      = shw ev
menu ev "show"   = shw ev 
menu ev "3"      = remove ev
menu ev "remove" = remove ev
menu ev "4"      = imprt ev
menu ev "import" = imprt ev
menu ev "5"      = exprt ev
menu ev "export" = exprt ev
menu ev "6"      = help ev
menu ev "help"   = help ev
menu ev "7"      = exit ev
menu ev "exit"   = exit ev
menu ev _        = wrong ev

finish :: [Event] -> [Char] -> IO ()
finish ev cond = do
    putStrLn "Press enter to exit"
    _ <- getLine
    printMenu ev ""

printEvents :: [Event] -> Int -> IO ()
printEvents ev idx = do
    if idx < length ev
    then do
        printEventByIdx ev (idx + 1)
        printEvents ev (idx + 1)
    else
        return ()

isEventExists :: [Event] -> Int -> Bool
isEventExists ev id = do
    if length ev == 0 then False
    else do
        if (idx (ev !! 0)) == id
        then do
            True
        else do
            isEventExists (tail ev) id

isAtLeastOneEventExists :: [Event] -> [Int] -> Bool
isAtLeastOneEventExists ev ids = do
    if length (filter (== 0) ids) > 0
    then do
        True
    else do
        if length ids == 0
        then do
            False
        else do
            let res = isEventExists ev (head ids)
            if res then True
            else isAtLeastOneEventExists ev (tail ids)

printEventById :: [Event] -> Int -> IO ()
printEventById ev id = do
    if length ev == 0
    then do
        return ()
    else do
        if (idx (ev !! 0)) == id
        then do
            putStrLn (show (id) ++ ". ["
                ++ (event (ev !! 0)) ++ "] ["
                ++ toFormatXX (show (day (date (ev !! 0)))) ++ "."
                ++ toFormatXX (show (month (date (ev !! 0)))) ++ "."
                ++ (show (year (date (ev !! 0)))) ++ " "
                ++ toFormatXX (show (hours (date (ev !! 0)))) ++ ":"
                ++ toFormatXX (show (minutes (date (ev !! 0)))) ++ "]")           
        else do
            printEventById (tail ev) id

printEventsByIds :: [Event] -> [Int] -> IO ()
printEventsByIds ev ids = do
    if length ids > 0
    then do
        if head ids == 0
        then do
            printEvents ev 0
        else do
            printEventById ev (head ids)
            printEventsByIds ev (filter (/= (head ids)) ids)
    else do
        return ()

printEventByIdx :: [Event] -> Int -> IO ()
printEventByIdx ev id = do
    putStrLn (show (idx (ev !! (id - 1))) ++ ". ["
      ++ (event (ev !! (id - 1))) ++ "] ["
      ++ toFormatXX (show (day (date (ev !! (id - 1))))) ++ "."
      ++ toFormatXX (show (month (date (ev !! (id - 1))))) ++ "."
      ++ (show (year (date (ev !! (id - 1))))) ++ " "
      ++ toFormatXX (show (hours (date (ev !! (id - 1))))) ++ ":"
      ++ toFormatXX (show (minutes (date (ev !! (id - 1))))) ++ "]")   

removeEventsByIds :: [Event] -> [Int] -> [Event]
removeEventsByIds ev ids = do
    if length ids == 0 || length ev == 0
    then do
        ev
    else do
        if head ids == 0
        then do
            []
        else do
            let newevs = removeEventsByIdsR ev ev (head ids) 0
            removeEventsByIds newevs (filter (/= (head ids)) ids)

removeEventsByIdsR :: [Event] -> [Event] -> Int -> Int -> [Event]
removeEventsByIdsR ev evc id idc = do
    if length ev == 0
    then do
        evc
    else do
        if (idx (ev !! 0)) == id
        then do
            let (fevc,sevc) = splitAt (idc) evc in fevc ++ (tail sevc)
        else do
            removeEventsByIdsR (tail ev) evc id (idc + 1)

sort :: [Event] -> [Char] -> [Event]
sort ev "1" = ev
sort ev "2" = (sortR ev [] 1 1)
sort ev "3" = (sortR ev [] 1 2)
sort ev "4" = (sortR ev [] 1 3)
sort ev "5" = (sortR ev [] 1 4)

sortR :: [Event] -> [Event] -> Int -> Int -> [Event]
sortR evi evn step mode = do
    if length evi == 0
    then do
        evn
    else if step == length evi || length evi == 1
    then do
        sortR (init evi) 
            ([(evi !! (length evi - 1))] ++ evn) 1 mode
    else do
        if mode == 1
        then do
            if ord (head (event (evi !! (step - 1)))) >
                ord (head (event (evi !! step)))
            then do
                sortR (swap evi step) 
                    evn (step + 1) mode
            else do
                sortR evi 
                    evn (step + 1) mode    
        else if mode == 2
        then do
            if ord (head (event (evi !! (step - 1)))) <
                ord (head (event (evi !! step)))
            then do
                sortR (swap evi step) 
                    evn (step + 1) mode
            else do
                sortR evi 
                    evn (step + 1) mode
        else if mode == 3
        then do
            let res = compareDateTime (evi !! (step - 1)) (evi !! step) True
            if res
            then do
                sortR (swap evi step) 
                    evn (step + 1) mode
            else do
                sortR evi 
                    evn (step + 1) mode
        else if mode == 4
        then do
            let res = compareDateTime (evi !! (step - 1)) (evi !! step) False
            if res
            then do
                sortR (swap evi step) 
                    evn (step + 1) mode
            else do
                sortR evi 
                    evn (step + 1) mode
        else do
            evn

compareDateTime :: Event -> Event -> Bool -> Bool
compareDateTime e1 e2 mode = do
    let yy = fromIntegral (year (date e1)) / 1
    let mm = fromIntegral (month (date e1)) / 12
    let dd = fromIntegral (day (date e1)) / 372
    let mn = fromIntegral (minutes (date e1)) / 535680
    let hh = fromIntegral (hours (date e1)) / 8928
    let totalE1 = yy + mm + dd + mn + hh
    let yy = fromIntegral (year (date e2)) / 1
    let mm = fromIntegral (month (date e2)) / 12
    let dd = fromIntegral (day (date e2)) / 372
    let mn = fromIntegral (minutes (date e2)) / 535680
    let hh = fromIntegral (hours (date e2)) / 8928
    let totalE2 = yy + mm + dd + mn + hh
    if mode
    then do
        if totalE1 > totalE2
        then True
        else False
    else do
        if totalE1 < totalE2
        then True
        else False

swap :: [Event] -> Int -> [Event]
swap ev pos = (init (fst (splitAt pos ev)) ++ [ev !! pos]
    ++ [ev !! (pos - 1)] ++ tail (snd (splitAt pos ev)))

isNum :: [Char] -> Bool
isNum n = do
    if length n == 0
    then True
    else if isDigit (head n) == True
    then isNum (tail n)
    else False

isInRange :: [Char] -> Int -> Int -> Bool
isInRange n left right = do
    if (not (isNum n)) || (n == "") then False
    else do
        if ((read n :: Int) >= left) &&
            ((read n :: Int) <= right)
        then True
        else False

getLastId :: [Event] -> Int
getLastId ev = do
    if length ev == 0
    then do
        0
    else do
        idx (ev !! ((length ev) - 1))

commaDelimitedStrToLstInt :: [Char] -> [Int]
commaDelimitedStrToLstInt str = do
    let res = commaDelimitedStrToLstIntR str "" "," True
    if length (filter (< 0) res) == 0
    then do
        if length res > 1 && length (filter (== 0) res) > 0
        then do
            [-3]
        else do
            res
    else do
        [last res]

commaDelimitedStrToLstIntR :: [Char] -> [Char] -> [Char] -> Bool -> [Int]
commaDelimitedStrToLstIntR str buf del cond = do
  if length str == 0
  then [read buf]
  else do
    if isDigit (head str) then commaDelimitedStrToLstIntR
        (tail str) (buf ++ ([head str])) del False
    else if ([head str]) == del && cond == False
    then do
      if length str > 1
      then do
        [read buf] ++ commaDelimitedStrToLstIntR (tail str) 
         "" del True
      else do
        [read buf]
    else if cond == True
    then do
      [-2]
    else do
      [-1]

toLowerStr :: [Char] -> [Char]
toLowerStr str = map toLower str

toFormatXX :: [Char] -> [Char]
toFormatXX str = do
  if length str < 2
  then do
    "0" ++ str
  else do
    str

exportF :: [Char] -> [Event] -> IO ()
exportF file evs = do
    exportFR file evs ""

exportFR :: [Char] -> [Event] -> [Char] -> IO ()
exportFR file evs res = do
  if length evs == 0
  then do
    writeFile file res
  else do  
    let partres = "[" ++ show (idx (evs !! 0)) ++ ","
                ++ show (event (evs !! 0)) ++ ","
                ++ show (day (date (evs !! 0))) ++ ","
                ++ show (month (date (evs !! 0))) ++ ","
                ++ show (year (date (evs !! 0))) ++ ","
                ++ show (hours (date (evs !! 0))) ++ ","
                ++ show (minutes (date (evs !! 0))) ++ "]"   
    exportFR file (tail evs) (res ++ partres)

splt :: [Char] -> [[Char]]
splt str = tail (spltR str [] [])


spltR :: [Char] -> [Char] -> [[Char]] -> [[Char]]
spltR str buf res = do
    if length str > 0
    then do
        if head str == '['
        then do
            spltR (tail str) "" (res ++ [buf])
        else if head str == ']'
        then do
            spltR (tail str) buf res
        else do
            spltR (tail str) (buf ++ [head str]) res
    else do
        res ++ [buf]

importF :: [[Char]] -> [Event]
importF str = do
  let res = importFR str []
  if length res > 0
  then do
    res
  else do
    []

importFR :: [[Char]] -> [Event] -> [Event]
importFR str evs = do
  if length str > 0
  then do
    let ev = lstToEvent (commaDelimitedStrToLstChar (str !! 0))
    if idx ev > 0
    then do
      importFR (tail str) (evs ++ [ev])
    else do
      []
  else do
    evs

commaDelimitedStrToLstChar :: [Char] -> [[Char]]
commaDelimitedStrToLstChar str = commaDelimitedStrToLstCharR str [] "," True []

commaDelimitedStrToLstCharR :: [Char] -> [Char] -> [Char] -> Bool -> [[Char]] -> [[Char]]
commaDelimitedStrToLstCharR str buf del cond res = do
  if length str == 0
  then res ++ [buf]
  else do
    if ([head str]) /= del then commaDelimitedStrToLstCharR
        (tail str) (buf ++ ([head str])) del False res 
    else if cond == False
    then do
      if length str > 0
      then do
        commaDelimitedStrToLstCharR (tail str) 
         [] del True (res ++ [buf])
      else do
        res
    else do
      [";"]

lstToEvent :: [[Char]] -> Event
lstToEvent lst = do
  let t = DateTime (0) (0) (0) (0) (0)
  if length lst == 7
  then do
    if length (filter (\x -> x ==';' || x == ',' || x == '[' || x == ']') (lst !! 1)) == 0
    then do
      if (isInRange (lst !! 2) 1 31) &&
        (isInRange (lst !! 3) 1 12) &&
        (isInRange (lst !! 4) 2018 3018) &&
        (isInRange (lst !! 5) 0 23) &&
        (isInRange (lst !! 6) 0 59)
      then do
        let time = DateTime (read (lst !! 2) :: Int) 
                            (read (lst !! 3) :: Int) 
                            (read (lst !! 4) :: Int)
                            (read (lst !! 5) :: Int)
                            (read (lst !! 6) :: Int) 
        let res = Event (read (lst !! 0) :: Int) time (lst !! 1)
        res   
      else do
        let res = Event (-3) t "Invalid date/time format"
        res
    else do
      let res  = Event (-2) t "';','[',']' is not allowed"
      res
  else do
    let res  = Event (-1) t "Invalid file"
    res
    






