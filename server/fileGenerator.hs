module Main where

main = do
    let file1 = "./data/100mb.txt"
    let file2 = "./data/250mb.txt"
    writeFile file1 ("100MB. This is a file that has a size of 100MB. \n"  ++ makeString 10000000)
    putStrLn "file 1 done"
    writeFile file2 ("250MB. This is a file that has a size of 250MB. \n"  ++ makeString 25000000)
    putStrLn "file 2 done"



makeString :: Int -> String
makeString = (!!) (iterate ("aaaaaaaaaa\n" ++) "")