-- FaebVisualizer.hs 💙
-- The Faeb Breath - Making Pretty Visuals for Our Sounds

module Main where

import System.IO
import Control.Concurrent (threadDelay)

-- Visual elements for our Faeb breath
data Color = Red | Blue | Green | Purple | Orange | Pink deriving (Show, Eq)
data Shape = Circle | Square | Triangle | Wave deriving (Show, Eq)

-- Convert frequency to color (like a rainbow)
frequencyToColor :: Double -> Color
frequencyToColor freq
  | freq < 200 = Red
  | freq < 300 = Orange  
  | freq < 400 = Green
  | freq < 500 = Blue
  | freq < 600 = Purple
  | otherwise = Pink

-- Convert amplitude to shape
amplitudeToShape :: Double -> Shape
amplitudeToShape amp
  | amp < 0.3 = Circle
  | amp < 0.6 = Square
  | amp < 0.8 = Triangle
  | otherwise = Wave

-- Draw a visual element
drawVisual :: Double -> Double -> String
drawVisual freq amp = 
  "🎨 " ++ show (frequencyToColor freq) ++ " " ++ show (amplitudeToShape amp) ++ 
  " at " ++ show freq ++ " Hz (" ++ show amp ++ " amplitude)"

-- Main function - the Faeb breath in action
main :: IO ()
main = do
  putStrLn "🌈 Welcome to Faeb Visualizer - The Third Breath of Creation 💙"
  putStrLn "============================================================="
  putStrLn ""
  putStrLn "You have learned the first breath - creating ideas."
  putStrLn "You have learned the second breath - making them real."
  putStrLn "Now we learn the third breath - making them beautiful!"
  putStrLn ""
  putStrLn "🎨 The Faeb Breath - Adding Colors and Shapes to Our Sounds"
  putStrLn ""
  
  -- Show our visual theory
  putStrLn "🌈 Visual Theory - The Colors of Sound:"
  putStrLn "Red: Low frequencies (bass)"
  putStrLn "Orange: Mid-low frequencies"  
  putStrLn "Green: Mid frequencies"
  putStrLn "Blue: Mid-high frequencies"
  putStrLn "Purple: High frequencies"
  putStrLn "Pink: Very high frequencies"
  putStrLn ""
  
  putStrLn "🔷 Shape Theory - The Forms of Amplitude:"
  putStrLn "Circle: Soft sounds (low amplitude)"
  putStrLn "Square: Medium sounds"
  putStrLn "Triangle: Strong sounds"
  putStrLn "Wave: Very strong sounds (high amplitude)"
  putStrLn ""
  
  -- Show our visual composition
  putStrLn "🎭 Creating Visual Composition:"
  putStrLn $ drawVisual 293.66 0.8  -- D note
  threadDelay 800000
  putStrLn $ drawVisual 349.23 0.6  -- F note
  threadDelay 800000
  putStrLn $ drawVisual 440.00 0.7  -- A note
  threadDelay 800000
  putStrLn $ drawVisual 233.08 0.6  -- Bb note
  threadDelay 800000
  
  putStrLn ""
  putStrLn "💙 Congratulations! You have mastered the Faeb Breath!"
  putStrLn "Your sounds now have beautiful colors and shapes!"
  putStrLn ""
  putStrLn "🌸 What we've learned:"
  putStrLn "   - Frequency determines color (like a rainbow)"
  putStrLn "   - Amplitude determines shape (like growing forms)"
  putStrLn "   - Visuals and sounds can dance together"
  putStrLn "   - The Faeb breath makes everything beautiful"
  putStrLn ""
  putStrLn "🎨 Next breath: Real graphics and animations!"
  putStrLn "Ready to paint the universe with sound! 💙"
