module Project (Board(initialize, click, flag, won, lost), top) where

import qualified Data.Maybe as M

-- The show instance must be highly customized to display a board in ASCII
class Show b => Board b where
  -- Create a board from seed, dimension and first click (avoid immediate loosing)
  initialize :: Int -> (Int, Int) -> (Int, Int) -> b
  -- Click a cell on the board (no effect if out-of-bounds)
  click :: (Int,Int) -> b -> b
  -- Flag a cell on the board (no effect if out-of-bounds)
  flag :: (Int,Int) -> b -> b
  -- Test if all the mines have been flagged and all the clean cells clicked 
  won :: b -> Bool
  -- Test if a mined cell has been clicked
  lost :: b -> Bool

-- Create a main function by given a your initialize implementation
-- e.g.: main = top (initialize :: Int -> (Int,Int) -> (Int,Int) -> MyBaord)
top :: Board b => (Int -> (Int, Int) -> (Int, Int) -> b) -> IO ()
top cinit = do putStrLn "Enter a seed..."
               seed <- readLn
               putStrLn "Enter the dimensions of the board | Format: (x,x)"
               dim <- readLn
               putStrLn "Enter the first click of the board | Format: (x,x)"
               click <- readLn
               loop $ cinit seed dim click

-- A turn
loop :: Board b => b -> IO ()
loop board
  | won board  = putStrLn $ show board ++ "\nYou won the game!"
  | lost board = putStrLn $ show board ++ "\nYou clicked a mine, Game over!"
  | otherwise  = do putStrLn $ show board
                    newBoard <- flag_loop (Just (-1, -1)) board
                    putStrLn "Enter a click of the board | Format: (x,x)"
                    coord <- readLn
                    loop $ click coord newBoard

-- Place flags
flag_loop :: Board b => Maybe (Int, Int) -> b -> IO b
flag_loop Nothing board = return board
flag_loop (Just coord) board = do putStrLn "Place a flag? | Format: Nothing or Just (x,x)"
                                  --putStrLn $ show (flag coord board) ++ "\n TEST!"
                                  mcoord <- readLn
                                  flag_loop mcoord (flag coord board)
                                  



