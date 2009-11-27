{-
 * Copyright (C) 2009 Byron James Johnson

 * oc-stats is free: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 2 of the License

 * This file is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License.
 * If not, see <http://www.gnu.org/licenses/>.
-}

-- TODO: links on top; top players, etc.

import Data.Bits
import Data.Char
import System.Directory
import qualified Data.Map as Map
import Data.Function
import Data.List as List
import Data.List.Split
import Data.Maybe
import Control.Monad (forM, mapM, when)
import Data.ByteString.Internal (c2w)

import System.Posix.Files (getFileStatus, isSymbolicLink, readSymbolicLink)
import System.FilePath.Posix ((</>))
import System.IO (hClose, openFile, IOMode (ReadMode), hGetContents)

import Network.CGI.Cookie ()
import Network.CGI.Monad ()
import Network.CGI.Protocol (getCGIVars)
import Network.URI (unEscapeString)
import Text.Html (toHtml)

domain = "http://bobis.boldlygoingnowhere.org/"
index = "oc-stats/"
url = domain ++ index

statsDirectory = "/var/www-data/stats"


data Score = Score { s_map       :: String
                   , s_layout    :: String
                   , s_type      :: ScoreType
                   , s_num       :: Int
                   , s_maxCount  :: Int
                   , s_count     :: Int
                   , s_time      :: Int
                   , s_name      :: String
                   , s_date      :: String
                   , s_guid      :: String
                   , s_ip        :: String
                   , s_adminName :: String
                   } deriving (Eq)

data ScoreType = ArmType | MediType | NoneType deriving (Eq)

type Query = Map.Map String String

instance Ord Score where
    (Score {s_map = am, s_layout = al, s_type = at, s_num = an}) `compare ` (Score {s_map = bm, s_layout = bl, s_type = bt, s_num = bn})
        | am /= bm  = (List.map toLower am `compare` (List.map toLower bm))
        | al /= bl  = al `compare` bl
        | at /= bt  = at `compare` bt
        | an /= bn  = an `compare` bn
        | otherwise = EQ

instance Ord ScoreType where
    -- Win scores first
    (MediType) `compare` (ArmType)  = GT
    (ArmType)  `compare` (MediType) = LT
    _          `compare` _          = EQ

scoreDateCmp :: Score -> Score -> Ordering
scoreDateCmp (Score {s_date = ad}) (Score {s_date = bd}) =
    let ap  = unintercalate " " ad
        bp  = unintercalate " " bd
        am_ = ap !! 0 :: String
        bm_ = bp !! 0 :: String
        am  = month am_
        bm  = month bm_
        ada = read $ ap  !! 1 :: Int
        bda = read $ bp  !! 1 :: Int
        ay  = read $ ap  !! 2 :: Int
        by  = read $ bp  !! 2 :: Int
        ap2 = unintercalate ":" $ ap !! 3
        bp2 = unintercalate ":" $ bp !! 3
        amn = read $ ap2 !! 0 :: Int
        bmn = read $ bp2 !! 0 :: Int
        as  = read $ ap2 !! 1 :: Int
        bs  = read $ bp2 !! 1 :: Int
    in if length ap >= 4 && length bp >= 4 && length ap2 >= 2 && length bp2 >= 2 then
           if ay /= by then
               ay `compare` by
           else if am /= bm then
                    am `compare` bm
                else if ada /= bda then
                         ada `compare` bda
                     else if amn /= bmn then
                              amn `compare` bmn
                          else
                              as `compare` bs
       else
           ad `compare` bd

main = do
    vars <- getCGIVars

    let query = queryVars $ case List.lookup "QUERY_STRING" vars of
                                (Just something) -> something
                                (Nothing)        -> ""

    writeHeader query

    -- write header
    let action = case Map.lookup "action" query of
                     (Just action) -> action
                     _             -> "invalid"

    case (List.map toLower action) of
        "viewstats"       -> viewStats     query
        "viewplayers"     -> viewPlayers   query
        _                 -> defaultAction query

    writeFooter query

writeHeader :: Query -> IO ()
writeHeader vars = do
    putStrNl "Content-type: text/html; charset=utf-8"
    putStrNl ""
    putStrNl "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
    putStrNl "<html dir=\"ltr\" xml:lang=\"en-US\" xmlns=\"http://www.w3.org/1999/xhtml\"><head>"
    putStrNl "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />"
    putStrNl "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"

writeFooter :: Query -> IO ()
writeFooter vars = do
    putStrNl "<hr class=\"separator\" /><pre><br /><br /></pre><div class=\"footer\"><a href=\"http://validator.w3.org/check?uri=referer\"><img src=\"http://www.w3.org/Icons/valid-xhtml11-blue\" alt=\"Valid XHTML 1.1\" style=\"border:0;width:88px;height:31px\" /> </a><a href=\"http://jigsaw.w3.org/css-validator/check/referer\"><img style=\"border:0;width:88px;height:31px\" src=\"http://jigsaw.w3.org/css-validator/images/vcss-blue\" alt=\"Valid CSS!\" /></a></div>"
    putStrNl "</body></html>"

writeNavigation :: Query -> IO ()
writeNavigation vars = do
    putStrNl $ "<form method=\"get\" action=\"" ++ url ++ "\" class=\"searchForm\"><div class=\"search\"><input type=\"text\" name=\"search\" class=\"searchBox\" /><input type=\"submit\" value=\"Search\" /><input type=\"reset\" /></div></form><p />"
    putStrNl $ "<div class=\"navigation\"><a href=\"" ++ url ++ "?action=viewStats\">Records</a> | <a href=\"" ++ url ++ "?action=viewPlayers\">Players</a></div><hr class=\"separator\" /><pre><br /><br /></pre>"

invalid :: Query -> IO ()
invalid vars = do
    putStr "<title>"
    putStr "Invalid Action"
    putStr "</title></head>"
    putStr ""
    putStr "<body>Invalid Action"

viewStats :: Query -> IO ()
viewStats vars = do
    let search = case Map.lookup "search" vars of
                      (Just s)  -> unEscapeString . List.map (\x -> if x == '+' then ' ' else toLower x) $ s
                      (Nothing) -> ""

    putStr $ "<title>"
    putStr $ "Viewing Stats"
    putStr $ "</title></head><body>"
    writeNavigation vars

    exists <- doesDirectoryExist' statsDirectory
    if exists
        then do
            contents <- getDirectoryContents' statsDirectory
            if List.null contents
                then do
                    putStrNl "<p class=\"notice\">No maps were found</p>"
                else do
                    rawScores <- forM contents $ mapDir vars
                    let allScores = List.concat rawScores

                    let l = (++ ", ") . List.map toLower
                        f (Score {s_map = m, s_layout = layout, s_count = count, s_maxCount = max, s_name = name, s_date = date, s_time = time, s_adminName = adminName}) =
                            let countStr    = show count ++ " / " ++ show max
                                timeStr     = mins' time ++ "m:" ++ secs' time ++ "s:" ++ msec' time ++ "ms"
                                nameStr     = clean name
                                altCountStr = show count ++ "/" ++ show max

                                l         = (++ ", ") . List.map toLower

                                searchStr = l countStr ++ l altCountStr ++ l timeStr ++ l date ++ l nameStr ++ l m ++ l layout ++ l adminName
                            in search `isInfixOf` searchStr
                        scores         = sort . List.filter f $ allScores
                        playersUnq     = uniquePlayers scores
                        numShowingUnq  = min 10 $ length playersUnq
                        players        = take 10 $ List.filter (not . null . (List.filter f) . snd) $ zip [1..] $ toPlayers allScores

                    -- print players
                    when (not $ List.null search) $ do
                        putStrNl $ "<p class=\"playerReport\">Found " ++ (show $ length playersUnq) ++ " players with map, layout, score, time, date, or name matching \"" ++ search ++ "\"; showing " ++ show numShowingUnq ++ ".</p>"

                    printPlayers vars players

                    putStrNl $ "<hr class=\"separator\" />"

                    -- print scores
                    when (not $ List.null search) $ do
                        putStrNl $ "<p class=\"searchReport\">Found " ++ (show $ length scores) ++ " scores with map, layout, score, time, date, or name matching \"" ++ search ++ "\"</p>"

                    printScores vars scores
        else do
            putStrNl $ "stats directory doesn't exist! (" ++ statsDirectory ++ ")"

viewPlayers :: Query -> IO ()
viewPlayers vars = do
    let search = case Map.lookup "search" vars of
                      (Just s)  -> unEscapeString . List.map (\x -> if x == '+' then ' ' else toLower x) $ s
                      (Nothing) -> ""

    putStr $ "<title>"
    putStr $ "Viewing All Players"
    putStr $ "</title></head><body>"
    writeNavigation vars

    putStr $ "<h1 class=\"topPlayers\">Players With Most Records</h1>"

    exists <- doesDirectoryExist' statsDirectory
    if exists
        then do
            contents <- getDirectoryContents' statsDirectory
            if List.null contents
                then do
                    putStrNl "<p class=\"notice\">No maps were found</p>"
                else do
                    rawScores <- forM contents $ mapDir vars
                    let allScores = List.concat rawScores

                    let l = (++ ", ") . List.map toLower
                        f (Score {s_map = m, s_layout = layout, s_count = count, s_maxCount = max, s_name = name, s_date = date, s_time = time, s_adminName = adminName}) =
                            let countStr    = show count ++ " / " ++ show max
                                timeStr     = mins' time ++ "m:" ++ secs' time ++ "s:" ++ msec' time ++ "ms"
                                nameStr     = clean name
                                altCountStr = show count ++ "/" ++ show max

                                l         = (++ ", ") . List.map toLower

                                searchStr = l countStr ++ l altCountStr ++ l timeStr ++ l date ++ l nameStr ++ l m ++ l layout ++ l adminName
                            in search `isInfixOf` searchStr
                        scores         = sort . List.filter f $ allScores
                        playersUnq     = take 10 $ uniquePlayers scores
                        players        = List.filter (not . null . (List.filter f) . snd) $ zip [1..] $ toPlayers allScores

                    -- print players
                    when (not $ List.null search) $ do
                        putStrNl $ "<p class=\"playerReport\">Found " ++ (show $ length playersUnq) ++ " players with map, layout, score, time, date, or name matching \"" ++ search ++ "\"</p>"

                    printPlayers vars players
        else do
            putStrNl $ "stats directory doesn't exist! (" ++ statsDirectory ++ ")"

defaultAction :: Query -> IO ()
--defaultAction = invalid
defaultAction = viewStats

printPlayer :: Query -> Bool -> (Int, [Score]) -> [(Int, [Score])] -> IO ()
printPlayer vars first (currentID, currentPlayer) remainingPlayers = do
    when first $ do
        putStr $ "<div class=\"player\">"
        putStr $ "<table border=\"1\" cellpadding=\"5\" cellspacing=\"5\" rules=\"rows\" summary=\"scores\" width=\"100%\" class=\"playerTable\">"
        putStr $ "<tr align=\"left\" valign=\"middle\" class=\"playerHeader\"><td>#</td> <td>Player</td> <td>Number of Records</td> <td>Date of First Record</td> <td>Date of Last Record</td></tr>"

    let f (Score {s_adminName = adminName}) = adminName /= "noname"
        currentPlayerDate = sortBy scoreDateCmp currentPlayer
        admins  = List.filter f currentPlayer
        current = if length admins > 0 then
                      head admins
                  else
                      head currentPlayer
        num'          = if currentID > 10 then
                      "Rest"
                  else
                      show currentID

        name_         = s_name current
        name          = "<a href=\"" ++ url ++ "?search=" ++ name_ ++ "\">" ++ name_ ++ "</a>"

    putStrNl $ "<tr align=\"left\" valign=\"middle\" class=\"player" ++ num' ++ "\">"
    putStrNl $ "<td>"
    putStrNl $ show currentID
    putStrNl $ "</td>"
    putStrNl $ "<td>"
    putStrNl $ name
    putStrNl $ "</td>"
    putStrNl $ "<td>"
    putStrNl $ show $ length currentPlayerDate
    putStrNl $ "</td>"
    putStrNl $ "<td>"
    putStrNl $ s_date . head $ currentPlayerDate
    putStrNl $ "</td>"
    putStrNl $ "<td>"
    putStrNl $ s_date . last $ currentPlayerDate
    putStrNl $ "</td>"
    putStrNl $ "</tr>"

    if (length remainingPlayers > 0)
        then do
            printPlayer vars False (head remainingPlayers) (tail remainingPlayers)
        else do
            putStrNl "</table></div>"

printPlayers :: Query -> [(Int, [Score])] -> IO ()
printPlayers vars players = do
    if length players > 0
        then do
            printPlayer vars True (head players) (tail players)
        else do
            putStrNl "<p class=\"notice\">No players</p>"

printScore :: Query -> Bool -> String -> String -> ScoreType -> Score -> [Score] -> IO ()
printScore vars first lastMap lastLayout lastType (Score {s_map = m, s_layout = layout, s_type = t, s_num = num, s_maxCount = maxCount, s_count = count, s_time = time, s_name = name, s_date = date}) remainingScores = do
    let countStr = show count ++ " / " ++ show maxCount
        timeStr  = mins' time ++ "m:" ++ secs' time ++ "s:" ++ msec' time ++ "ms"
        nameStr  = colourString . escape $ name
        num' = if num > 10 then
                   "Rest"
               else
                   show num

    when (first || lastType /= t || lastLayout /= layout) $ do
        when (not first) $ do
            putStrNl $ "</table>"
    when (first || lastMap /= m) $ do
        when (not first) $ do
            putStrNl $ "</div><hr class=\"separator\" />"

        putStrNl $ "<div class=\"score\">"
        putStrNl $ "<h1 class=\"map\">High-scores for courses on <b class=\"mapname\">" ++ m ++ "</b></h1>"
    when (first || lastMap /= m || lastLayout /= layout) $ do
        putStrNl $ "<h2 class=\"layoutname\">" ++ layout ++ "</h2>"
    when (first || lastType /= t || lastLayout /= layout) $ do
        putStrNl $ case t of
                        (ArmType)  -> "<h3 class=\"armDescription\">Winning high-scores</h3>"
                        (MediType) -> "<h3 class=\"mediDescription\">Medical station high-scores</h3>"

        putStrNl $ case t of
                        (ArmType)  -> "<table border=\"1\" cellpadding=\"5\" cellspacing=\"5\" rules=\"rows\" summary=\"scores\" width=\"100%\" class=\"armTable\">"
                        (MediType) -> "<table border=\"1\" cellpadding=\"5\" cellspacing=\"5\" rules=\"rows\" summary=\"scores\" width=\"100%\" class=\"mediTable\">"
        putStrNl $ case t of
                        (ArmType)  -> "<tr align=\"left\" valign=\"middle\" class=\"armHeader\"><th>#</th> <th>time</th> <th>name</th> <th>date</th></tr>"
                        (MediType) -> "<tr align=\"left\" valign=\"middle\" class=\"mediHeader\"><th>#</th> <th>Count</th> <th>time</th> <th>name</th> <th>date</th></tr>"

    putStr $ case t of
                  (ArmType)  -> "<tr align=\"left\" valign=\"middle\" class=\"armScore"  ++ num' ++ "\">"
                  (MediType) -> "<tr align=\"left\" valign=\"middle\" class=\"mediScore" ++ num' ++ "\">"
    putStr $ "<td>"
    putStr $ show num
    putStr $ "</td>"
    putStr $ case t of
                    (ArmType)  -> ""
                    (MediType) -> "<td>" ++ countStr ++ "</td>"
    putStr $ "<td>"
    putStr $ timeStr
    putStr $ "</td>"
    putStr $ "<td>"
    putStr $ nameStr
    putStr $ "</td>"
    putStr $ "<td>"
    putStr $ date
    putStr $ "</td>"
    putStr $ "</tr>"

    if (length remainingScores > 0)
        then do
            printScore vars False m layout t (head remainingScores) (tail remainingScores)
        else do
            when (not first) $ do
                putStrNl "</table></div>"

-- the scores need to be sorted already
printScores :: Query -> [Score] -> IO ()
printScores vars scores = do
    if length scores > 0
        then do
            printScore vars True "" "" NoneType (head scores) (tail scores)
        else do
            putStrNl "<p class=\"notice\">No scores</p>"

score :: Query -> String -> String -> ScoreType -> String -> (Int, String) -> Maybe Score
score vars mapname layoutname scoreType numLine (id, scoreLine) = do
    let numArms   = read $ head $ words numLine :: Int
        numMedis  = read $ last $ words numLine :: Int
        num       = case scoreType of
                         (ArmType)  -> numArms
                         (MediType) -> numMedis

        score     = unintercalate "\x01" scoreLine
        count     = read $ score !! 0 :: Int
        time      = read $ score !! 1 :: Int
        name      = score !! 2 :: String
        date      = score !! 3 :: String
        guid      = score !! 4 :: String
        ip        = score !! 5 :: String
        adminName = score !! 6 :: String

    if (length scoreLine > 0 && length numLine > 0 && length (unintercalate "\x01" scoreLine) >= 6 && length (words scoreLine) >= 2)
        then do
            Just $ Score mapname layoutname scoreType id num count time name date guid ip adminName
        else do
            Nothing

samePlayer :: Score -> Score -> Bool
samePlayer (Score {s_guid = ag, s_ip = ai, s_adminName = aa}) (Score {s_guid = bg, s_ip = bi, s_adminName = ba})
    | ag == bg                   = True
    | ai == bi                   = True
    | aa == ba && aa /= "noname" = True
    | otherwise                  = False

samePlayerCmp :: Score -> Score -> Ordering
samePlayerCmp (Score {s_guid = ag, s_ip = ai, s_adminName = aa}) (Score {s_guid = bg, s_ip = bi, s_adminName = ba})
    | ag /= bg                                     = ag `compare` bg
    | ai /= bi                                     = ai `compare` bi
    | aa /= ba && aa /= "noname" && ba /= "Noname" = aa `compare` ba
    | otherwise                                    = EQ

sl :: [(a, b)] -> [b]
sl = snd . unzip

uniquePlayers :: [Score] -> [Score]
uniquePlayers = nubBy samePlayer

scoresOf :: Score -> [Score] -> [Score]
scoresOf player = List.filter $ samePlayer player

toPlayers :: [Score] -> [[Score]]
toPlayers = List.map sort . sortBy notLengthOrdering . groupBy samePlayer . sortBy samePlayerCmp

sortPlayers :: [Score] -> [Score]
sortPlayers = List.concat . List.map sort . sortBy notLengthOrdering . groupBy samePlayer . sortBy samePlayerCmp

lengthOrdering :: [a] -> [a] -> Ordering
lengthOrdering a b = (length a) `compare` (length b)

notLengthOrdering :: [a] -> [a] -> Ordering
notLengthOrdering a b = (length b) `compare` (length a)

layoutDir :: Query -> String -> String -> IO [Score]
layoutDir vars mapname layoutname = do
    let filename = statsDirectory </> mapname </> layoutname

    let search = case Map.lookup "search" vars of
                      (Just s)  -> List.map toLower s
                      (Nothing) -> ""
        don'tSearch = search `isInfixOf` mapname || search `isInfixOf` layoutname

    layoutIsDirectory <- doesDirectoryExist' filename
    if ((head layoutname /= '.') && (layoutIsDirectory)) then do
        let winFilename  = filename </> "win.dat"
            mediFilename = filename </> "med.dat"

        winFileExists  <- doesFileExist winFilename
        mediFileExists <- doesFileExist mediFilename
        if winFileExists && mediFileExists
            then do
                winHandle  <- openFile winFilename ReadMode
                winData    <- hGetContents winHandle
                mediHandle <- openFile winFilename ReadMode
                mediData   <- hGetContents mediHandle

                let wAllLines   = List.lines winData
                    wNumLine    = if length wAllLines > 0 then head wAllLines else []
                    wScoreLines = if length wAllLines > 0 then tail wAllLines else [[]]
                let mAllLines   = List.lines mediData
                    mNumLine    = if length mAllLines > 0 then head mAllLines else []
                    mScoreLines = if length mAllLines > 0 then tail mAllLines else [[]]

                let wScoresMaybes = (flip List.map) (zip [1..] wScoreLines) $ score vars mapname layoutname ArmType  wNumLine
                let mScoresMaybes = (flip List.map) (zip [1..] mScoreLines) $ score vars mapname layoutname MediType mNumLine
                let wScores = catMaybes wScoresMaybes
                let mScores = catMaybes mScoresMaybes
                return $ wScores ++ mScores

                --hClose winHandle
                --hClose minHandle
            else do
                if winFileExists
                    then do
                        winHandle  <- openFile winFilename ReadMode
                        winData    <- hGetContents winHandle

                        let wAllLines   = List.lines winData
                            wNumLine    = if length wAllLines > 0 then head wAllLines else []
                            wScoreLines = if length wAllLines > 0 then tail wAllLines else [[]]

                        let wScoresMaybes = (flip List.map) (zip [1..] wScoreLines) $ score vars mapname layoutname ArmType wNumLine
                        let wScores = catMaybes wScoresMaybes
                        return wScores

                        --hClose winHandle
                    else do
                        if winFileExists
                            then do
                                mediHandle <- openFile winFilename ReadMode
                                mediData   <- hGetContents mediHandle

                                let mAllLines   = List.lines mediData
                                    mNumLine    = if length mAllLines > 0 then head mAllLines else []
                                    mScoreLines = if length mAllLines > 0 then tail mAllLines else [[]]

                                let mScoresMaybes = (flip List.map) (zip [1..] mScoreLines) $ score vars mapname layoutname MediType mNumLine
                                let mScores = catMaybes mScoresMaybes
                                return mScores

                                --hClose minHandle
                            else do
                                return []

        else
            return []

mapDir :: Query -> String -> IO [Score]
mapDir vars mapname = do
    let filename = statsDirectory </> mapname

    mapIsDirectory <- doesDirectoryExist' filename
    if (head mapname /= '.' && mapIsDirectory)
        then do
            contents <- getDirectoryContents' filename
            if List.null contents
                then do
                    return []
                else do
                    rawScores <- forM contents $ layoutDir vars mapname
                    return $ List.concat rawScores
        else do
            return []

tailAlways :: [a] -> [a]
tailAlways [] = []
tailAlways xs = tail xs

splitTwo' :: String -> [(String, String)] -> String -> [(String, String)]
splitTwo' _ acc [] = acc
splitTwo' delimiters acc string = splitTwo' delimiters ((former, latter):acc) rest
    where former = takeWhile isNotDelimiter string
          latter = takeWhile isNotDelimiter $ nextPart string
          rest   = takeWhile isNotDelimiter $ nextPart . nextPart $ string
          isNotDelimiter = (\x -> not $ x `elem` delimiters)
          nextPart = tailAlways . dropWhile isNotDelimiter

splitTwo :: String -> String -> [(String, String)]
splitTwo delimiters = splitTwo' delimiters []

queryVars :: String -> Query
queryVars = Map.fromList . splitTwo "=&"

msec   :: Int   -> Int
secs   :: Int   -> Int
mins   :: Int   -> Int
msec'  :: Int   -> String
secs'  :: Int   -> String
mins'  :: Int   -> String
msec'' :: Score -> String
secs'' :: Score -> String
mins'' :: Score -> String
msec t = ((t) - (((t) `div` 1000) * 1000))
secs t = (((t) - (((t) `div` 60000) * 60000)) `div` 1000)
mins t = ((t) `div` 60000)
msec'  = show . msec
secs'  = show . secs
mins'  = show . mins
msec'' = show . msec . s_time
secs'' = show . secs . s_time
mins'' = show . mins . s_time

month :: String -> Int
month m
    | "ja"  `isInfixOf` (List.map toLower m) = 1
    | "f"   `isInfixOf` (List.map toLower m) = 2
    | "mar" `isInfixOf` (List.map toLower m) = 3
    | "ap"  `isInfixOf` (List.map toLower m) = 4
    | "may" `isInfixOf` (List.map toLower m) = 5
    | "jun" `isInfixOf` (List.map toLower m) = 6
    | "jul" `isInfixOf` (List.map toLower m) = 7
    | "au"  `isInfixOf` (List.map toLower m) = 8
    | "s"   `isInfixOf` (List.map toLower m) = 9
    | "o"   `isInfixOf` (List.map toLower m) = 10
    | "n"   `isInfixOf` (List.map toLower m) = 11
    | "d"   `isInfixOf` (List.map toLower m) = 12
    | otherwise                              = 13

colour :: Char -> String
colour c = case ((c2w c) - (c2w '0')) .&. 0x07 of
                0 -> "Black"
                1 -> "Red"
                2 -> "Lime"
                3 -> "Yellow"
                4 -> "Blue"
                5 -> "Aqua"
                6 -> "Fuchsia"
                7 -> "White"

colourSpan :: Char -> String
colourSpan c = "<span style=\"color:" ++ colour' ++ ";\">"
               where colour' = case ((c2w c) - (c2w '0')) .&. 0x07 of
                                    0 -> "Black"
                                    1 -> "Red"
                                    2 -> "Lime"
                                    3 -> "Yellow"
                                    4 -> "Blue"
                                    5 -> "Aqua"
                                    6 -> "Fuchsia"
                                    7 -> "White"

clean :: String -> String
clean []        = ""
clean ('^':c:s) = colour c ++ clean s
clean ('^':_)   = ""
clean (f:l)     = f:l

escape :: String -> String
escape = show . toHtml

repeatList :: Int -> [a] -> [a]
repeatList n xs = take (n * (length xs)) $ cycle xs

colourString :: String -> String
colourString = colourString' 0

colourString' :: Int -> String -> String
colourString' n []        = repeatList n "</span>"
colourString' n ('^':c:s) = colourSpan c ++ colourString' (succ n) s
colourString' n ('^':_)   = repeatList n "</span>"
colourString' n (f:l)     = f:l ++ (repeatList n "</span>")

listTruncate :: Int -> [a] -> [a]
listTruncate n = reverse . drop n . reverse

resolve :: FilePath -> IO FilePath
resolve filename = do
    status <- getFileStatus filename
    if isSymbolicLink status
        then do
            realFilename <- readSymbolicLink filename
            resolve realFilename
        else do
            return filename

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' filename = do
    realFilename <- resolve filename
    getDirectoryContents realFilename

doesDirectoryExist' :: FilePath -> IO Bool
doesDirectoryExist' filename = do
    realFilename <- resolve filename
    doesDirectoryExist realFilename

putStrNl = putStrLn
