{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import           Data.Maybe          (mapMaybe)
import qualified Data.Vector         as V
import           Options.Applicative
import           System.Random       (RandomGen, getStdGen, randomR)

data Options =
  Options
    { optsLowerCase :: Bool
    , optsUpperCase :: Bool
    , optsNumbers   :: Bool
    , optsSymbols   :: Bool
    , optsLength    :: Int
    }
  deriving (Show)

optionsParser :: Parser Options
optionsParser = do
  noLowerCase <-
    switch (long "no-lower-case" <> help "Don't use lower case letters")
  noUpperCase <-
    switch (long "no-upper-case" <> help "Don't use upper case letters")
  noNumbers <- switch (long "no-numbers" <> help "Don't use numbers")
  noSymbols <- switch (long "no-symbols" <> help "Don't use symbols")
  len <-
    option
      auto
      (long "length" <>
       help "Length of the password" <> value 16 <> showDefault <> metavar "LEN")
  return $
    Options
      { optsLowerCase = not noLowerCase
      , optsUpperCase = not noUpperCase
      , optsNumbers = not noNumbers
      , optsSymbols = not noSymbols
      , optsLength = len
      }

withOptions :: (Options -> IO a) -> IO a
withOptions fn = fn =<< execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        (fullDesc <>
         progDesc "Generate random password strings" <>
         header "pwgen - a simple CLI password generator")

availableChars :: Options -> V.Vector Char
availableChars Options {..} =
  V.fromList $
  concat $
  mapMaybe
    (\(a, b) ->
       if a
         then Just b
         else Nothing)
    [ (optsLowerCase, ['a' .. 'z'])
    , (optsUpperCase, ['A' .. 'Z'])
    , (optsNumbers, ['0' .. '9'])
    , (optsSymbols, "!@#$%^&*()-+=?.,|~{}[]<>/\\'\"")
    ]

mkPassword :: RandomGen g => Int -> g -> V.Vector Char -> String -> String
mkPassword 0 _ _ pw = pw
mkPassword loop rng ac pw =
  let mx = V.length ac - 1
      (idx, nrng) = randomR (0, mx) rng
      c = (ac V.! idx)
   in mkPassword (loop - 1) nrng ac (c : pw)

main :: IO ()
main =
  withOptions $ \options -> do
    let ln = optsLength options
        ac = availableChars options
    rng <- getStdGen
    putStrLn $ mkPassword ln rng ac ""
