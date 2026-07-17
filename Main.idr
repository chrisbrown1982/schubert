module Main

import System
import Data.List

import Scales
import Duration
import Melody
import Midi

import Twinkle

||| Example MIDI files this program can generate, keyed by the name used
||| to select them (also used to name the resulting file in Midi.outputDir).
||| Adding a new example -- scale, melody, or otherwise -- is just one more
||| entry here.
namedExamples : List (String, IO (Either String ()))
namedExamples =
  [ ("c-major", writeScale "c-major.mid" C 4)
  , ("g-major", writeScale "g-major.mid" G 4)
  , ("f-major", writeScale "f-major.mid" F 4)
  , ("twinkle", writeMelody "twinkle.mid" 480 96 (resolve (majorScale (MkPitch C 4)) twinkle))
  ]

usage : IO ()
usage = do
  putStrLn "Usage: schubert <example>"
  putStrLn "Available examples:"
  traverse_ (\(name, _) => putStrLn ("  " ++ name)) namedExamples

||| Run a named example's write action, reporting success or failure.
render : String -> IO (Either String ()) -> IO ()
render name action = do
  Right () <- action
    | Left err => putStrLn ("failed to write " ++ name ++ ".mid: " ++ err)
  putStrLn ("wrote " ++ outputDir ++ "/" ++ name ++ ".mid")

main : IO ()
main = do
  args <- getArgs
  case args of
    (_ :: name :: _) =>
      case lookup name namedExamples of
        Just action => render name action
        Nothing     => do putStrLn ("no such example: " ++ name)
                          usage
    _ => usage
