module Utils where

-- Dungeon constants
dungeonWidth :: Int
dungeonWidth = 50

dungeonHeight :: Int
dungeonHeight = 20

wrapText :: Int -> String -> [String]
wrapText width text = reverse $ foldl (addWord width) [] (words text)
  where
    addWord w [] word = [word]
    addWord w (line : rest) word
      | length line + length word + 1 <= w = (line ++ " " ++ word) : rest
      | otherwise = word : line : rest

-- ANSI color codes
resetColor :: String
resetColor = "\ESC[0m"

red, green, yellow, blue, cyan, white, brightRed :: String -> String
red s = "\ESC[31m" ++ s ++ resetColor
green s = "\ESC[32m" ++ s ++ resetColor
yellow s = "\ESC[33m" ++ s ++ resetColor
blue s = "\ESC[34m" ++ s ++ resetColor
-- magenta s = "\ESC[35m" ++ s ++ resetColor
cyan s = "\ESC[36m" ++ s ++ resetColor
white s = "\ESC[37m" ++ s ++ resetColor
brightRed s = "\ESC[91m" ++ s ++ resetColor

bold :: String -> String
bold s = "\ESC[1m" ++ s ++ resetColor

-- dimmed :: String -> String
-- dimmed s = "\ESC[90m" ++ s ++ resetColor

-- Clear the terminal screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- Clear the rest of the current line
clearRestOfLine :: String
clearRestOfLine = "\ESC[K"

-- Hide the cursor
hideCursor :: String
hideCursor = "\ESC[?25l"

-- Show the cursor
showCursor :: String
showCursor = "\ESC[?25h"
