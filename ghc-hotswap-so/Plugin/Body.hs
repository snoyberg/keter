module Plugin.Body
  ( initialize
  , middleware
  ) where

initialize :: Int -> IO ()
initialize _ = putStrLn "Hello from plugin initialize!"

middleware ::Int -> IO ()
middleware _ = putStrLn "Hello from plugin middleware"
