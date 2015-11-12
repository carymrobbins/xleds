{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import Text.Read

usage = intercalate "\n"
  [ "Usage: xleds [options]"
  , ""
  , "Options:"
  , "  -get"
  , "  -set <percentage> or = <percentage>"
  , "  -inc <percentage> or + <percentage>"
  , "  -dec <percentage> or - <percentage>"
  ]

ledsDir = "/sys/class/leds/smc::kbd_backlight"
ledsBrightnessFile = ledsDir ++ "/brightness"
ledsMaxBrightnessFile = ledsDir ++ "/max_brightness"

newtype Percent = Percent Float deriving (Eq, Fractional, Ord, Num)

mkPercent :: Int -> Int -> Percent
mkPercent n d = Percent $ fromIntegral n / fromIntegral d

parsePercent :: String -> Percent
parsePercent s =
  case readMaybe n of
    Just p | p >= 0 && p <= 100 -> Percent $ p / 100
    _ -> error $ "Invalid percentage value: " ++ n
  where
  -- Remove the percent, if it exists.
  (n, _) = break (=='%') s

toBrightness :: Percent -> Int -> Int
toBrightness (Percent perc) maxVal
  | perc <= 0 = 0
  | perc >= fromIntegral maxVal = maxVal
  | otherwise = floor $ perc * fromIntegral maxVal

instance Show Percent where
  show (Percent f) = show (f * 100) ++ "%"

main :: IO ()
main =
  getArgs >>= \case
    ["-get"] -> runGet

    [cmd, s] -> runCommand cmd s

    [help]
      | help `elem` ["--help", "-h", "-?"] ->
        putStrLn usage

    _ -> do
      hPutStrLn stderr usage
      exitFailure

getCurrentBrightness :: IO Int
getCurrentBrightness = do
  s <- readFile ledsBrightnessFile
  case readMaybe s of
    Just n -> return n
    Nothing -> error $ "Invalid brightness file value: " ++ s

getMaxBrightness :: IO Int
getMaxBrightness = do
  s <- readFile ledsMaxBrightnessFile
  case readMaybe s of
    Just n -> return n
    Nothing -> error $ "Invalid max brightness file value: " ++ s

setBrightness :: Int -> IO ()
setBrightness n = writeFile ledsBrightnessFile $ show n

runGet :: IO ()
runGet = do
  current <- getCurrentBrightness
  maxVal <- getMaxBrightness
  print $ mkPercent current maxVal

runCommand :: String -> String -> IO ()
runCommand cmd s = do
  let perc = parsePercent s
  current <- getCurrentBrightness
  maxVal <- getMaxBrightness
  let currentPerc = mkPercent current maxVal
  case () of
    _ | cmd `elem` ["-set", "="] -> do
          setBrightness $ toBrightness perc maxVal
          print perc

      | cmd `elem` ["-inc", "+"] -> do
          let newPerc = min 1.0 $ currentPerc + perc
          setBrightness $ toBrightness newPerc maxVal
          print newPerc

      | cmd `elem` ["-dec", "-"] -> do
          let newPerc = max 0.0 $ currentPerc - perc
          setBrightness $ toBrightness newPerc maxVal
          print newPerc
