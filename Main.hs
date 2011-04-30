{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}

import Control.Monad (when, unless)
import Data.List (delete)
import System.Console.CmdArgs
import System.Directory (doesFileExist, removeFile)
import System.Environment (getEnvironment)

data Options = Options
    { filename  :: String
    , number    :: Int
    , show_     :: Bool
    , clean     :: Bool
    , line      :: [String]
    }
    deriving (Show, Data, Typeable)

getDefaultFile :: IO [Char]
getDefaultFile = do
    found <- lookup "HOME" `fmap` getEnvironment
    case found of
        Nothing   -> error "There is no $HOME environment variable"
        Just home -> return (home ++ "/.linecache")

getOptions :: IO Options
getOptions = do
    defaultFile <- getDefaultFile
    let defaultCapacity = 10
    return $ Options
        { filename = defaultFile &= typFile &= help ("Cache file path (default=" ++ defaultFile ++ ")")
        , number   = defaultCapacity &= typ "NUM" &= help ("Cache capacity (default=" ++ show defaultCapacity++ ")")
        , show_    = False &= help "Show line cache"
        , clean    = False &= help "Clean line cache"
        , line     = def &= args &= typ "NEW-LINE" }
        &= summary "Line Cache Manager v 0.1.1, (c) Dmitry Antonyuk 2010-2011"
        &= program "linecache"

lru :: (Eq a) => a -> [a] -> [a]
lru x xs = x : (x `delete` xs)

force :: [a] -> IO ()
force = mapM_ (ret ())

ret :: a -> b -> IO a
ret = const . return

main :: IO ()
main = do
    Options _file _capacity _list _delete (unwords -> _line) <- getOptions >>= cmdArgs

    when _delete $ do
        exists <- doesFileExist _file
        when exists (removeFile _file)

    cache <- fmap lines (readFile _file `catch` ret "")
    force cache

    unless (null _line) $ do
        let newCache = take _capacity $ lru _line cache
        writeFile _file (unlines newCache)

    when _list $ putStr $ unlines cache
