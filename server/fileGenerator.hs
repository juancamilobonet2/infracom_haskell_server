module Main where
import Data.List (foldl')
import Control.Monad.Loops
import System.Posix.Files

main = do
    let file1 = "./data/100mb.txt"
    let file2 = "./data/250mb.txt"
    writeFile file1 "100MB. This is a file that has a size of 100MB. \n"
    whileM_ (fmap (< 100000000) (getFileSize file1)) (appendFile file1 "aaaaaaaaaabbbbbbbbbbaaaaaaaaabbbbbbbbbbaaaaaaaaaa\n")
    putStrLn "file 1 done"
    writeFile file2 "250MB. This is a file that has a size of 250MB. \n"
    whileM_ (fmap (< 250000000) (getFileSize file2)) (appendFile file2 "aaaaaaaaaabbbbbbbbbbaaaaaaaaabbbbbbbbbbaaaaaaaaaa\n")
    putStrLn "file 2 done"



makeString :: Int -> String
makeString x = foldl' (++) "" $ replicate x "aaaaaaaaaabbbbbbbbbbaaaaaaaaabbbbbbbbbbaaaaaaaaaa\n"

getFileSize :: String -> IO Integer
getFileSize path = do
    stat <- getFileStatus path
    return $ fromIntegral (fileSize stat)
