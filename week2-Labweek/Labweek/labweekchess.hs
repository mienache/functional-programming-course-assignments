-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck



-- Exercise 9:

pic1 :: Picture
pic1 = beside (above knight (invert knight)) (above (invert knight) knight)

pic2 :: Picture
pic2 = beside (above knight (flipV (invert knight))) (above (invert knight) (flipV knight))

-- Exercise 10:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)

getPiecesArrangement :: Picture
getPiecesArrangement = beside (beside (beside rook knight) (beside bishop queen)) (beside (beside king bishop) (beside knight rook)) 

getWhitePawns :: Picture
getWhitePawns = (repeatH 8 pawn) `over` emptyRow

getBlackPawns :: Picture
getBlackPawns = (repeatH 8 pawn) `over` otherEmptyRow

getWhiteRow :: Picture
getWhiteRow = getPiecesArrangement `over` otherEmptyRow

getBlackRow :: Picture
getBlackRow = (invert getPiecesArrangement) `over` emptyRow

-- e)

populatedBoard :: Picture
populatedBoard = getWhiteRow `above` getWhitePawns `above` (repeatV 2 (otherEmptyRow `above` emptyRow)) `above` getBlackPawns `above` getBlackRow


-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = x `above` (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoAbove (twoBeside x)

