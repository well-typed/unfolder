module Cmdline (
    Cmdline(..)
  , Command(..)
  , Output(..)
  , Algorithm(..)
  , SurfaceParams(..)
  , getCmdline
  ) where

import Control.Applicative (asum)
import Options.Applicative (Parser, (<**>))
import Options.Applicative qualified as Opt
import Text.Read (readMaybe)

import Line (Line(..))
import Line qualified

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      command :: Command
    }
  deriving stock (Show)

data Command =
    Analyse FilePath Algorithm
  | GenRandomData Output Line.RandomDataParams
  | GenRegressionSurface FilePath SurfaceParams
  deriving stock (Show)

data SurfaceParams = SurfaceParams {
      interceptActual :: Double
    , interceptMin    :: Double
    , interceptMax    :: Double
    , interceptStep   :: Double
    , slopeActual     :: Double
    , slopeMin        :: Double
    , slopeMax        :: Double
    , slopeStep       :: Double
    }
  deriving stock (Show)

data Output =
    Gnuplot FilePath
  | Raw FilePath
  deriving stock (Show)

data Algorithm =
    Baseline
  | Parallel1
  | Parallel2 Int
  | Parallel3 Int
  | Parallel4 Int
  | Parallel5 Int
  | Parallel6 Int
  deriving stock (Show)

getCmdline :: IO Cmdline
getCmdline = Opt.execParser opts
  where
    opts :: Opt.ParserInfo Cmdline
    opts = Opt.info (parseCmdline <**> Opt.helper) $ mconcat [
          Opt.fullDesc
        , Opt.progDesc "Haskell Unfolder episode 48: pure parallelism"
        ]

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
    pure Cmdline
      <*> parseCommand

parseCommand :: Parser Command
parseCommand = Opt.hsubparser $ mconcat [
      cmd "analyse"
          ( pure Analyse
              <*> parseInput
              <*> parseAlgorithm
          )
          "Apply linear regression"
    , cmd "gen-random-data"
          ( pure GenRandomData
              <*> parseOutput
              <*> parseRandomDataParams
          )
          "Generate random input"
    , cmd "gen-regression-surface"
          ( pure GenRegressionSurface
              <*> parseInput
              <*> parseSurfaceParams
          )
          "Generate regression surface"
    ]

parseSurfaceParams :: Parser SurfaceParams
parseSurfaceParams =
    pure SurfaceParams
      <*> param "intercept-actual" interceptActual
      <*> param "intercept-min"    (interceptActual - interceptRange)
      <*> param "intercept-max"    (interceptActual + interceptRange)
      <*> param "intercept-step"   (2 * interceptRange / 20)
      <*> param "slope-actual"     slopeActual
      <*> param "slope-min"        (slopeActual - slopeRange)
      <*> param "slope-max"        (slopeActual + slopeRange)
      <*> param "slope-step"       (2 * slopeRange / 20)
  where
    interceptActual, interceptRange, slopeActual, slopeRange :: Double
    interceptActual = 5
    interceptRange  = 120
    slopeActual     = 2
    slopeRange      = 2

    param :: String -> Double -> Parser Double
    param name defaultValue = Opt.option Opt.auto $ mconcat [
          Opt.long name
        , Opt.value defaultValue
        , Opt.showDefault
        ]

parseAlgorithm :: Parser Algorithm
parseAlgorithm = asum [
      unchunked Baseline  "baseline"
    , unchunked Parallel1 "parallel1"
    , chunked   Parallel2 "parallel2"
    , chunked   Parallel3 "parallel3"
    , chunked   Parallel4 "parallel4"
    , chunked   Parallel5 "parallel5"
    , chunked   Parallel6 "parallel6"
    ]
  where
    unchunked :: Algorithm -> String -> Parser Algorithm
    unchunked alg name = Opt.flag' alg $ mconcat [
          Opt.long name
        ]

    chunked :: (Int -> Algorithm) -> String -> Parser Algorithm
    chunked alg name = alg <$> (Opt.option readInt $ mconcat [
          Opt.long name
        , Opt.metavar "CHUNK_SIZE"
        ])

parseInput :: Parser FilePath
parseInput =
    Opt.strOption (mconcat [
        Opt.long "raw"
      , Opt.help "Raw data"
      ])

parseOutput :: Parser Output
parseOutput = asum [
      Gnuplot <$> parseWriteGnuplot
    , Raw     <$> parseWriteRaw
    ]

parseWriteGnuplot :: Parser FilePath
parseWriteGnuplot =
    Opt.strOption (mconcat [
        Opt.long "gnuplot"
      , Opt.help "Write data in a format suitable for gnuplot"
      ])

parseWriteRaw :: Parser FilePath
parseWriteRaw =
    Opt.strOption (mconcat [
        Opt.long "raw"
      , Opt.help "Write raw data"
      ])

parseRandomDataParams :: Parser Line.RandomDataParams
parseRandomDataParams =
    pure Line.RandomDataParams
      <*> parseNumPoints
      <*> parseLine
      <*> parseRange "deviation" "deviation from the model" (0, 0)
      <*> parseRange "distance"  "distance between points"  (1, 1)

parseNumPoints :: Parser Int
parseNumPoints =
    Opt.option readInt (mconcat [
        Opt.long "num-points"
      , Opt.help "Number of points"
      ])

parseLine :: Parser Line
parseLine =
    pure Line
      <*> Opt.option Opt.auto (mconcat [
              Opt.long "intercept"
            ])
      <*> Opt.option Opt.auto (mconcat [
              Opt.long "slope"
            ])

parseRange :: String -> String -> (Double, Double) -> Parser (Double, Double)
parseRange name descr (defMin, defMax) =
    pure (,)
      <*> Opt.option Opt.auto (mconcat [
              Opt.long $ "min-" ++ name
            , Opt.help $ "Minimum " ++ descr
            , Opt.value defMin
            , Opt.showDefault
            ])
      <*> Opt.option Opt.auto (mconcat [
              Opt.long $ "max-" ++ name
            , Opt.help $ "Maximum " ++ descr
            , Opt.value defMax
            , Opt.showDefault
            ])

readInt :: Opt.ReadM Int
readInt = do
    str <- Opt.str
    case readMaybe (filter (/= '_') str) of
      Just x  -> return x
      Nothing -> fail $ "Could not parse " ++ show str

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

cmd :: String -> Parser a -> String -> Opt.Mod Opt.CommandFields a
cmd name parser desc = Opt.command name (Opt.info parser $ Opt.progDesc desc)