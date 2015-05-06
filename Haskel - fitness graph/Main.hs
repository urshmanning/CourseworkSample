{-# OPTIONS_GHC -fspec-constr-count=12 #-}

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Control.Monad.State
import Data.Maybe
import Math.Statistics
import PlotFitnesses
import System.Console.GetOpt
import System.Random
import System (getArgs)
import IteratedPD
import CanonicalGA

data Flag = MutationRate Double
          | CrossoverRate Double
          | Generations Int
          | GenerationSize Int
          | MachineReadable
          | UseGen StdGen
          | ImageFile FilePath
          deriving (Eq, Show, Read)
instance Eq StdGen where
    a == b = show a == show b

options :: [OptDescr Flag]
options =
    [ Option "m" ["mutation-rate"] (ReqArg (MutationRate . read) "<mutation rate>") "The probability of each bit being flipped"
    , Option "c" ["crossover-rate"] (ReqArg (CrossoverRate . read) "<crossover rate>") "The probability of parents being bred"
    , Option "n" ["generations"] (ReqArg (Generations . read) "<generations>") "The number of generations to run for before outputting"
    , Option "s" ["generation-size"] (ReqArg (GenerationSize . read) "<generation size>") "The size of each generation"
    , Option "r" ["machine-readable"] (NoArg MachineReadable) "Set for machine readable output, leave unset for human readable output"
    , Option "g" ["generator"] (ReqArg (UseGen . read) "<generator>") "If set, will use given generator of type StdGen"
    , Option "i" ["image-file"] (ReqArg ImageFile "<image file to output to>") "If set, will output graph to PNG"
    ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: iteratedpd [OPTION...]"

humanReadable :: Double -> Double -> Int -> Int -> [PDIndividual] -> StdGen -> [[Double]] -> String
humanReadable pM pC gens genSize pop gen fitnesseses = iniString ++ "\n\n" ++ popString ++ "\n\n" ++ avgString ++ "\n\n" ++ maxString ++ "\n"
    where popString = "Final population:\n"++show pop
          fitString = "Fitnesses as generated:\n"++show fitnesseses
          avgString = "Average fitness per generation:\n"++(show $ map mean fitnesseses)
          maxString = "Maximum fitness per generation:\n"++(show $ map maximum fitnesseses)
          genString = "Random generator to recreate test:\t"++show gen
          pMuString = "Mutation probability:\t\t\t"++show pM
          pCrString = "Crossover probability:\t\t\t"++show pC
          nGenString = "Number of generations:\t\t\t"++show gens
          gSiString = "Size of each generation:\t\t"++show genSize
          iniString = pMuString++"\n"++pCrString++"\n"++nGenString++"\n"++gSiString++"\n"++genString

machineReadable :: Double -> Double -> Int -> Int -> [PDIndividual] -> StdGen -> [[Double]] -> String
machineReadable pM pC gens genSize pop gen fitnesseses = "("++show pM++","++show pC++","++show gens++","++show genSize++","++show gen++","++show pop++","++show fitnesseses++")\n"

sameFlag :: Flag -> Flag -> Bool
sameFlag (CrossoverRate _) (CrossoverRate _) = True
sameFlag (MutationRate _) (MutationRate _) = True
sameFlag (Generations _) (Generations _) = True
sameFlag (GenerationSize _) (GenerationSize _) = True
sameFlag MachineReadable MachineReadable = True
sameFlag (UseGen _) (UseGen _) = True
sameFlag (ImageFile _) (ImageFile _) = True
sameFlag _ _= False

tryOpt :: Flag -> [Flag] -> Flag
tryOpt flag (f:flags) | sameFlag flag f = f
                      | otherwise = tryOpt flag flags
tryOpt flag [] = flag

tryOpt' :: Flag -> [Flag] -> Maybe Flag
tryOpt' flag (f:flags) | sameFlag flag f = Just f
                       | otherwise = tryOpt' flag flags
tryOpt' flag [] = Nothing

tryGen :: [Flag] -> IO StdGen
tryGen opts = do
    gen' <- getStdGen
    let (UseGen gen) = tryOpt (UseGen gen') opts
    return gen

main :: IO ()
main = do
    args <- getArgs
    (opts, otherArgs) <- compilerOpts args
    let (MutationRate pM) = tryOpt (MutationRate 0.001) opts
    let (CrossoverRate pC) = tryOpt (CrossoverRate 0.8) opts
    let (Generations gens) = tryOpt (Generations 50) opts
    let (GenerationSize genSize) = tryOpt (GenerationSize 20) opts
    gen <- tryGen opts
    let (pop, (_, fitnesseses')) = runState (evolve pM pC gens genSize) (gen, []) :: ([PDIndividual], (StdGen, [[Double]]))
    let fitnesseses = reverse fitnesseses'
    putStr $ (if MachineReadable `elem` opts then machineReadable else humanReadable) pM pC gens genSize pop gen fitnesseses
    case tryOpt' (ImageFile "") opts of
        Just (ImageFile filePath) -> renderableToSVGFile (putChart fitnesseses) 1024 768 filePath
        Nothing -> renderableToWindow (putChart fitnesseses) 800 600
