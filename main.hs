import System.Directory
import System.Environment
import System.Exit

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
getExtension :: String -> String
getExtension s = (!! 1) $ wordsWhen (=='.') s

getFilesFromType :: [FilePath] -> String -> [FilePath]
getFilesFromType list fileType = filter ( (== fileType) . (getExtension)  ) list

main = do
    args <- getArgs
    if not ( (length args) >= 2)
        then do putStrLn "Not enough parameters"
                putStrLn "Usage: renamefiles [prefix] [file_extension]"
                exitFailure
        else do let prefix = args !! 0
                let fileType = args !! 1
                all <- getDirectoryContents "."
                let files = filter (\x -> x /= "." && x /= ".." ) all
                let filesFromExtension = getFilesFromType files fileType
                let actions = map (\file -> renameFile file (prefix ++ file)) filesFromExtension
                sequence actions
