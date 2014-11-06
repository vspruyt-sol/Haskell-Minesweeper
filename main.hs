module Main where
    import Project as P
    import qualified Data.Maybe as M
    import qualified Data.List as L

    data MsCell = FCell | UCell {surrMines :: Int} | MCell {surrMines :: Int} | UMine | MMine deriving (Eq) -- F = Flagged, M = Masked, U = Unmasked
    data MsBoard = MsBoard {getState :: [[MsCell]]}

    -- Geeft een string terug naargelang de cell
    instance Show MsCell where
        show FCell = "F"
        show (MCell a) = " "
        show (UCell a) = show a
        show MMine = " "
        show UMine = "X"

    -- Geeft een string terug in de vorm van een board
    instance Show MsBoard where
        show (MsBoard b) =  let borders = (map $ foldr (\x acc -> "+-" ++ acc) "+") b
                                vals = (map $ foldr (\x acc -> "|" ++ show x ++ acc) "|") b
                                bottom = head borders
                            in (unlines $ concatMap (\(x,y) -> [x,y]) $ zip borders vals) ++ bottom

    instance Board MsBoard where
        initialize seed (x,y) (cw,ch) = MsBoard [[FCell, MCell 1],[MCell 1, UCell 1]]

        click (x,y) b 
                        | M.isNothing val = b
                        | val == Just FCell = b
                        | val == Just MMine = b -- Todo: verloren
                        | val == Just UMine = b -- Todo: Normaal ben je hier al verloren, zou hij ni mogen in geraken
                        | isMCell (M.fromJust val) = b  -- Todo: een MCell verandert het bord 
                        | otherwise = b -- UCell, niets moet gebeuren
                        where val = getClickedVal (x,y) b

        flag (x,y) b = if isOutOfBounds (x,y) b then b else putFlag (x,y) b

        won b = (sum $ map length $ map (filter (\x -> x == UMine || x == MMine)) $ getState b) == 0

        lost b = (sum $ map length $ map (filter (== UMine)) $ getState b) > 0 -- Als er minstens 1 Unmasked mine op het bord is

    main :: IO ()
    main = top (initialize :: Int -> (Int,Int) -> (Int,Int) -> MsBoard)

    -- Helper function, ZOEK OPLOSSING ZONDER DIT
    isMCell :: MsCell -> Bool
    isMCell (MCell _) = True
    isMCell _ = False 

    putFlag :: (Int,Int) -> MsBoard -> MsBoard
    putFlag (x,y) MsBoard {getState = s} = MsBoard s

    -- Needed to initialize the board
    numSurrMines :: (Int,Int) -> MsBoard -> Int
    numSurrMines (x,y) MsBoard {getState = s} = sum $ map length $ map (filter (== MMine)) $ map (take xTake . drop xDrop) $ (take yTake . drop yDrop) s
                                                where   xTake = if x <= 0 then 2 else 3
                                                        yTake = if y <= 0 then 2 else 3
                                                        xDrop = if x-1 <= 0 then 0 else x-1
                                                        yDrop = if y-1 <= 0 then 0 else y-1

    getSurrCells :: (Int,Int) -> MsBoard -> [(Int,Int)]
    getSurrCells (x,y) MsBoard {getState = s} = L.delete (x,y) [(x1,y1) | x1 <- [xLBound..xUBound], y1 <- [yLBound..yUBound]]
                                                where   xLen = length (head s) - 1
                                                        xLBound = if x == 0 then 0 else x - 1
                                                        xUBound = if x == xLen then xLen else x + 1
                                                        yLen = length s - 1
                                                        yLBound = if y == 0 then 0 else y - 1
                                                        yUBound = if y == yLen then yLen else y + 1

    getClickedVal :: (Int,Int) -> MsBoard -> Maybe MsCell
    getClickedVal (x,y) b = if isOutOfBounds (x,y) $ b then Nothing else Just $ (getState b !! y) !! x

    isOutOfBounds :: (Int,Int) -> MsBoard -> Bool
    isOutOfBounds (x,y) MsBoard {getState = s} = x < 0 || y < 0 || x >= length (head s) || y >= length s

    -- Testing data
    mm = MMine
    um = UMine
    fc = FCell
    mc = MCell 0
    uc = UCell 0
    brd = MsBoard [[mm,um,fc,mc,uc],[mm,um,fc,mc,uc],[mm,um,fc,mc,uc],[mm,um,fc,mc,uc],[mm,um,fc,mc,uc]]
