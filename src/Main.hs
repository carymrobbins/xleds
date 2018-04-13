{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Data.List
import Data.Maybe
import Data.Monoid
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Read

usage = intercalate "\n"
  [ "Usage: xleds [key|screen] [command]"
  , ""
  , "Commands:"
  , "  -get"
  , "  -set <amount|percentage%> or = <amount|percentage%>"
  , "  -inc <amount|percentage%> or + <amount|percentage%>"
  , "  -dec <amount|percentage%> or - <amount|percentage%>"
  ]

data Args = Args
  { argDevice :: Device
  , argCommand :: Command
  }

data Device = Keyboard | Screen deriving Show

data Command
  = Get
  | Set Amount
  | Inc Amount
  | Dec Amount

data Amount
  = RawAmount Int
  | PercentAmount Percent

data ArgFailure = ShowHelp | InvalidUsage String

parseArgs :: [String] -> Either ArgFailure Args
parseArgs args = case args of
  [] -> Left $ InvalidUsage "Missing argument"
  [help] | help `elem` ["-h", "--help"] -> Left ShowHelp
  device : command -> Args <$> parseDevice device <*> parseCommand command
  where
  parseDevice = \case
    "key" -> Right Keyboard
    "screen" -> Right Screen
    other -> Left $ InvalidUsage $ "Expected key or screen, got: " <> other

  parseCommand = \case
    ["-get"] -> Right Get
    [set, val] | set `elem` ["-set", "="] -> Set <$> parseAmount val
    [inc, val] | inc `elem` ["-inc", "+"] -> Inc <$> parseAmount val
    [dec, val] | dec `elem` ["-dec", "-"] -> Dec <$> parseAmount val
    _ -> Left $ InvalidUsage "Invalid command"

  parseAmount s = case s of
    "" -> Left $ InvalidUsage "Empty amount value"
    _ -> case readMaybe s of
      Just n -> Right $ RawAmount n
      Nothing -> case readMaybe $ init s of
        Just p | p >= 0 && p <= 100 -> Right $ PercentAmount $ Percent $ p / 100
        Nothing -> Left $ InvalidUsage $ "Invalid amount value: " <> s

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f xs = case xs of
  [] -> return Nothing
  y:ys -> do
    b <- f y
    if b then return (Just y) else findM f ys

getLedsDir :: Device -> IO String
getLedsDir device = case device of
  Keyboard -> findDir keyboardDirs
  Screen -> findDir screenDirs
  where
  findDir dirs = findM doesDirectoryExist dirs >>= \case
    Just d -> return d
    Nothing -> do
      hPutStrLn stderr $ "Could not locate backlight for device: " <> show device
      exitFailure

  keyboardDirs =
    [ "/sys/class/leds/smc::kbd_backlight"
    , "/sys/class/leds/dell::kbd_backlight"
    ]

  screenDirs =
    [ "/sys/class/backlight/intel_backlight"
    ]

getLedsBrightnessFile :: FilePath -> String
getLedsBrightnessFile = (++ "/brightness")

getLedsMaxBrightnessFile :: FilePath -> String
getLedsMaxBrightnessFile = (++ "/max_brightness")

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
main = (parseArgs <$> getArgs) >>= \case
  Left ShowHelp -> putStrLn usage
  Left (InvalidUsage msg) -> hPutStrLn stderr (msg <> "\n" <> usage) >> exitFailure
  Right args -> runCommand args

getCurrentBrightness :: FilePath -> IO Int
getCurrentBrightness ledsDir = do
  s <- readFile $ getLedsBrightnessFile ledsDir
  case readMaybe s of
    Just n -> return n
    Nothing -> error $ "Invalid brightness file value: " ++ s

getMaxBrightness :: FilePath -> IO Int
getMaxBrightness ledsDir = do
  s <- readFile $ getLedsMaxBrightnessFile ledsDir
  case readMaybe s of
    Just n -> return n
    Nothing -> error $ "Invalid max brightness file value: " ++ s

setBrightness :: FilePath -> Int -> IO ()
setBrightness ledsDir n = writeFile (getLedsBrightnessFile ledsDir) $ show n

runCommand :: Args -> IO ()
runCommand Args{..} = do
  ledsDir <- getLedsDir argDevice
  maxVal <- getMaxBrightness ledsDir

  let modify
        :: Amount
        -> (forall a. (Ord a, Fractional a) => a -> a)
        -> (forall a. Num a => a -> a -> a)
        -> IO ()
      modify amount limit op = do
        curVal <- getCurrentBrightness ledsDir
        let curPercent = mkPercent curVal maxVal
        case amount of
          RawAmount n -> setBrightness ledsDir $ curVal `op` n
          PercentAmount p -> do
            let newPercent = limit $ curPercent `op` p
            setBrightness ledsDir $ toBrightness newPercent maxVal
            print newPercent

  case argCommand of
    Get -> do
      current <- getCurrentBrightness ledsDir
      maxVal <- getMaxBrightness ledsDir
      print $ mkPercent current maxVal

    Set amount -> case amount of
      RawAmount n -> setBrightness ledsDir n
      PercentAmount p -> setBrightness ledsDir $ toBrightness p maxVal

    Inc amount -> modify amount (min 1.0) (+)
    Dec amount -> modify amount (max 0.0) (-)

