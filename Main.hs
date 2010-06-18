{-# LANGUAGE DeriveDataTypeable #-}

import Data.List (delete, intersperse)
import System.Console.CmdArgs
import System.Environment (getEnvironment)

data Options = Options  { filename :: String, number :: Int, line :: [String] }
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
        , line     = def &= args &  typ "NEW-LINE" } &= text "Line cache manager"

lru :: (Eq a) => a -> [a] -> [a]
lru x xs = x : (x `delete` xs)

force :: [a] -> IO ()
force = mapM_ (\_ -> return ())

main = do
    options <- getOptions
    Options filename number rest <- cmdArgs "linecache v 0.1, (C) Dmitry Antonyuk 2010" [options]

    cache <- fmap lines $ readFile filename `catch` (\_ -> return "")

    let line = concat $ intersperse " " rest

        handle ""   = list
        handle line = add

        list = putStr (unlines cache)

        add = do
            let newCache = take number $ lru line cache
            force newCache
            writeFile filename (unlines newCache)

    handle line
  where

