module PlotFitnesses where

import Data.Colour
import Data.Colour.Names
import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Math.Statistics

-- | Take average and maximum fitnesses and graph them
chart' :: [Double] -- ^ List of average fitnesses
       -> [Double] -- ^ List of maximum fitnesses
       -> Layout1 Int Double -- ^ A layout which can be turned into a graph
chart' avgs maxs = layout
    where
        averageFitness = plot_lines_style .> line_color ^= opaque blue
                       $ plot_lines_values ^= [zip [1..] avgs]
                       $ plot_lines_title ^= "Mean fitness"
                       $ defaultPlotLines
        maxFitness = plot_lines_style .> line_color ^= opaque red
                   $ plot_lines_values ^= [zip [1..] maxs]
                   $ plot_lines_title ^= "Best fitness"
                   $ defaultPlotLines
        layout = layout1_title ^= "Fitness over generations"
               $ layout1_left_axis ^: laxis_title ^= "Fitness" 
               $ layout1_bottom_axis ^: laxis_title ^= "Generation"
               $ layout1_plots ^= [Left (toPlot averageFitness), Left (toPlot maxFitness)]
               $ layout1_grid_last ^= False
               $ defaultLayout1

-- | Take all fitnesses for every generation and graph them
chart :: [[Double]] -- ^ A list of lists of fitnesses (per generation)
      -> Layout1 Int Double
chart fitnesseses = chart' (map mean fitnesseses) (map maximum fitnesseses)

putChart fitnesseses = toRenderable $ chart fitnesseses
