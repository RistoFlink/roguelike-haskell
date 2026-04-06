module Utils where

wrapText :: Int -> String -> [String]

wraptext width text = reverse $ foldl (addWord width) [] (words text)
  where
    addWord w [] word = [word]
    addWord w (line : rest) word
      | length line + length word + 1 <= w = (line ++ " " ++ word) : rest
      | otherwise = word : line : rest
