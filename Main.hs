{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad (when, unless)
import Data.List (delete, intersperse)
import System.Console.CmdArgs
import System.Directory (doesFileExist, removeFile)
import System.Environment (getEnvironment)

data Options = Options  { filename :: String, number :: Int, show_ :: Bool, clean :: Bool, line :: [String] }
    deriving (Show, Data, Typeable)

getDefaultFile = do
    found <- fmap (lookup "HOME") getEnvironment
    case found of
        Nothing   -> error "There is no $HOME environment variable"
        Just home -> return (home ++ "/.linecache")

getOptions = do
    defaultFile <- getDefaultFile
    return $ mode Options
        { filename = defaultFile &= typFile & text "Cache file path"
        , number   = 10 &= typ "NUM" & text "Cache capacity"
        , show_    = False &= text "Show line cache"
        , clean    = False &= text "Clean line cache"
        , line     = def &= args &  typ "NEW-LINE" } &= text "Line cache manager"

lru :: (Eq a) => a -> [a] -> [a]
lru x xs = x : (x `delete` xs)

force :: [a] -> IO ()
force = mapM_ void

ret = const . return

void = ret ()

main = do
    options <- getOptions
    args    <- cmdArgs "linecache v 0.1, (C) Dmitry Antonyuk 2010" [options]

    let newLine = unwords (line args)
        cacheFile = filename args

    when (clean args) $ do
        exists <- doesFileExist cacheFile
        when exists (removeFile cacheFile)

    cache <- fmap lines (readFile cacheFile `catch` ret "")
    force cache

    unless (null newLine) $ do
        let newCache = take (number args) $ lru newLine cache
        writeFile cacheFile (unlines newCache)

    when (show_ args) $ putStr $ unlines cache
