module Main where
    import qualified Data.Maybe as M
    import qualified Data.List.Split as S
    import qualified Data.List as L
    import Project as P

    -- Eventueel refactoren met type
    data MsCell = FCell { wasMine :: Bool}| UCell {surrMines :: Int} | MCell | UMine | MMine deriving (Eq) -- F = Flagged, M = Masked, U = Unmasked
    data MsBoard = MsBoard {getState :: [MsCell], xDim :: Int, yDim ::Int}

    -- returns a Cell represented as a string
    instance Show MsCell where
        show (FCell _) = "F"
        show MCell = " "
        show (UCell a) = show a
        show MMine = " "
        show UMine = "X"

    -- Returns a Board represented as a string
    instance Show MsBoard where
        show (MsBoard b x y) =  let borders = (map $ foldr (\z acc -> "+-" ++ acc) "+") $ S.chunksOf x b
                                    vals = (map $ foldr (\z acc -> "|" ++ show z ++ acc) "|") $ S.chunksOf x b
                                    bottom = head borders
                                in unlines $ (concatMap (\(z,y) -> [z,y]) $ zip borders vals) ++ [bottom]

    -- Create an instance of Board
    instance Board MsBoard where
        initialize seed (x,y) (cw,ch) = MsBoard [MMine,MCell,MCell,MCell] 2 2 --TODO

        click (x,y) b = click' $ getClickedVal (x, y) b
                        where click' (Just MMine) = updateCellValue (x,y) UMine b
                              click' (Just MCell) = updateCellValue (x,y) (UCell $ numSurrMines (x,y) b) b
                              click' (Just (FCell True)) = updateCellValue (x,y) UMine b
                              click' (Just (FCell False)) = updateCellValue (x,y) (UCell $ numSurrMines (x,y) b) b
                              click' _ = b

        flag (x,y) b = if isOutOfBounds (x,y) b then b else updateCellValue (x,y) (FCell False) b

        won b = (length $ filter (\x -> x == UMine || x == MMine || x == MCell) $ getState b) == 0 && numFlaggedNonMines b == 0-- No more mines or masked cells on the board

        lost b = (length $ filter (== UMine) $ getState b) > 0 -- At least 1 unmasked mine on the board or a wrongly flagged cell

    -- Main function
    main :: IO ()
    main = top (initialize :: Int -> (Int,Int) -> (Int,Int) -> MsBoard)
 
    updateCellValue :: (Int,Int) -> MsCell -> MsBoard -> MsBoard
    updateCellValue (x,y) val b = MsBoard ((fst splitted) ++ [newVal] ++ scd) (xDim b) (yDim b)
                                  where splitted = splitAt (x + (y * (xDim b))) $ getState b
                                        scd      = if null $ snd splitted then [] else tail $ snd splitted
                                        aMine    = if null $ snd splitted then False else (head $ snd splitted) == MMine || (head $ snd splitted) == FCell True
                                        newVal   = if val == (FCell False) then FCell aMine else val

    -- Needed to initialize the board
    numSurrMines :: (Int,Int) -> MsBoard -> Int
    numSurrMines (x,y) b = sum $ map length $ map (filter (\x -> x == MMine || x == FCell True)) $ map (take xTake . drop xDrop) $ (take yTake . drop yDrop) $ S.chunksOf (xDim b) $ getState b
                           where   xTake = if x <= 0 then 2 else 3
                                   yTake = if y <= 0 then 2 else 3
                                   xDrop = if x-1 <= 0 then 0 else x-1
                                   yDrop = if y-1 <= 0 then 0 else y-1

    numFlaggedNonMines :: MsBoard -> Int
    numFlaggedNonMines MsBoard {getState = s} = length $ filter (\x -> x == FCell False) s

    -- Returns a list of tuples with the coordinates of neighbours
    getSurrCells :: (Int,Int) -> MsBoard -> [(Int,Int)]
    getSurrCells (x,y) MsBoard {xDim = xD, yDim = yD} = L.delete (x,y) [(x1,y1) | x1 <- [xLBound..xUBound], y1 <- [yLBound..yUBound]]
                                                        where   xLBound = if x == 0 then 0 else x - 1
                                                                xUBound = if x == xD - 1 then xD - 1 else x + 1
                                                                yLBound = if y == 0 then 0 else y - 1
                                                                yUBound = if y == yD - 1 then yD -1 else y + 1

    -- Returns the clicked value if it is in bounds
    getClickedVal :: (Int,Int) -> MsBoard -> Maybe MsCell
    getClickedVal (x,y) b = if isOutOfBounds (x,y) $ b then Nothing else Just $ getState b !! (x + y * (xDim b))

    -- Checks whether a tuple is out of bounds
    isOutOfBounds :: (Int,Int) -> MsBoard -> Bool
    isOutOfBounds (x,y) MsBoard {xDim = xD, yDim = yD} = x < 0 || y < 0 || x >= xD || y >= yD
