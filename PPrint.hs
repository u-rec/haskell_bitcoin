module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = showString k . showString ": " . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS $ showChar '\n'
pprH = intercalateS $ showChar ' '

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep = foldl (\s n -> if s "" == "" then n else s . sep . n) (showString "")

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith fun = pprV . map fun

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
