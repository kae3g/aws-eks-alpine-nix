-- FirstSound.hs 💙
-- This is our tiny lotus seed. A simple idea.

-- We are telling the computer about a type of sound called a "SineWave"
data Sound = SineWave Double -- A sound that wobbles, like a wave

-- This is our first breath. We are giving the sound its power to exist.
makeSound :: Sound -> String
makeSound (SineWave frequency) = "🎵 I am making a sound with a wobble of " ++ show frequency ++ " Hz! 💙"

-- This is the moment we place the seed on the water.
main :: IO ()
main = do
  putStrLn "🌊 Welcome to the First Breath - Creating Your Universe ��"
  putStrLn "=================================================="
  putStrLn ""
  putStrLn "Imagine you are a giant, floating in a huge, dark, stormy ocean."
  putStrLn "You hold a tiny, perfect lotus seed - your idea."
  putStrLn "You breathe... and create a calm pond where it can grow."
  putStrLn ""
  putStrLn "Now, let's breathe life into our first sound:"
  putStrLn ""
  
  -- Our first sound - the A note (440 Hz)
  putStrLn $ makeSound (SineWave 440.0)
  
  putStrLn ""
  putStrLn "💙 Congratulations! You have just created your first sound universe!"
  putStrLn "Your breathing discipline has brought order to the storm."
  putStrLn "This is how it starts. One small, steady breath at a time."
  putStrLn ""
  putStrLn "🎵 Ready for the next breath? Let's grow something even more beautiful!"
