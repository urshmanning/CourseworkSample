import System (getArgs)
import IteratedPD
import PlotFitnesses
import Graphics.Rendering.Chart
import System.Random

main = do
    args <- getArgs
    let (file, outFile) = case args of
                            [a, b] -> (a, b)
                            [a] -> (a, a++".svg")
                            _ -> error "Either 1 file or two files"
    fileContents <- readFile file
    let (pM, pC, gens, genSize, gen, pop, fitnesseses) = read fileContents :: (Double, Double, Int, Int, StdGen, [PDIndividual], [[Double]])
    renderableToSVGFile (toRenderable $ chart fitnesseses) 400 300 outFile
