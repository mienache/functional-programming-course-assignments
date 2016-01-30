import Data.Char
import Data.List
import Test.QuickCheck

type Matrix = [[Double]]

-- subtractScalarRow takes two rows and a scalar as input and subtract from the jth element of the second row the jth element of the first row.
-- It is used when computing the Triangular Form of a matrix (the Row Echelon Form).
subtractScalarRow :: [Double] -> [Double] -> Double -> [Double]
subtractScalarRow a b k = zipWith (\x y -> x - y * k) b a

-- getRestOfMatrix takes a matrix as input and cuts the first row of the matrix and makes every entry in other row equal to 0.
-- The elements which are not an entry in their row are modified according to their corresponding element in the first row (as in the Row Echelon Form algorithm).
-- It is used when computing the Triangular Form of a matrix (the Row Echelon Form).
getRestOfMatrix :: Matrix -> Matrix
getRestOfMatrix a = [drop 1 (subtractScalarRow (a !! 0) (a !! i) ((a !! i !! 0) / firstElement)) | i <- [1..n - 1]]
                  where firstElement = a !! 0 !! 0
                        n = length a

-- findNonZero takes a matrix as input and returns True if there is at least one row in matrix a which has a nonzero entry.
-- It is used when computing the Triangular Form of a Matrix.
findNonZero :: Matrix -> Bool
findNonZero a = (length [i | i <- [0..(length a) - 1], (a !! i !! 0) /= 0]) > 0

-- swapFirstLine takes a matrix as input and swaps the first line of a matrix when it has a zero entry with the closest line which has a nonzero entry.
-- It also multiplies the first entry with (-1) in order to deal with the number of swaps which is involved in the formula for the determinant (-1)^k * a[i][i], where k is the number of swaps occured in the process of reaching the Row Echelon Form.
-- It is used when computing the Triangular Form of a matrix (the Row Echelon Form).
swapFirstLine :: Matrix -> Matrix
swapFirstLine a = [((-1) * (a !! r !! 0)) : (drop 1 (a !! r))] ++ [(a !! i) | i <- [1..r - 1]] ++ [a !! 0] ++ [(a !! i) | i <- [r + 1..n - 1]]
                where n = length a
                      r = list !! 0
                      list = [i | i <- [0..n - 1], (a !! i !! 0) /= 0]
                                   
-- borderWithZero takes a matrix as input and adds a first row formed only by 0 and a first column also formed only by 0.
-- It is used when computing the Triangular form of a matrix (the Row Echelon Form).
borderWithZero :: Matrix -> Matrix
borderWithZero a = (replicate (n + 1) 0) : [0 : (a !! i) | i <- [0..n - 1]]
                 where n = length a

-- cutFirstrowFirstColumn takes a matrix as input and cuts the first row and the first column of a matrix.
-- It is used when computing the Triangular form of a matrix (Row Echelon Form).
cutFirstRowFirstColumn :: Matrix -> Matrix
cutFirstRowFirstColumn a = [drop 1 (a !! i) | i <- [1..n - 1]]
                         where n = length a

-- getTriangularForm takes a matrix as an input and outputs the Triangular Form of that matrix (the Row Echelon Form).
getTriangularForm :: Matrix -> Matrix
getTriangularForm [] = []
getTriangularForm a
    | firstElem == 0 = 
          if (findNonZero a) then getTriangularForm(swapFirstLine a) 
          else borderWithZero(getTriangularForm(cutFirstRowFirstColumn a))
    | otherwise = (a !! 0) : [0 : (restOfMatrix !! i) | i <- [0..(length restOfMatrix) - 1]]
    where restOfMatrix = getTriangularForm(getRestOfMatrix a)
          firstElem = (a !! 0) !! 0

-- determinant takes a matrix as input and outputs the determinant of that matrix.
determinant :: Matrix -> Double
determinant a = product diagonal
              where diagonal = [(tf !! i !! i) | i <- [0..n - 1]]
                    n = length a
                    tf = getTriangularForm a

-- eliminateRowCol takes a matrix, an index for a row and and index for a column as input and outputs the matrix without the given row and column.
-- It is used when computing the adjugate matrix.
eliminateRowCol :: Matrix -> Int -> Int -> Matrix
eliminateRowCol a r c = [[a !! i !! j | j <- [0..n - 1], j /= c] | i <- [0..n - 1], i /= r]
                      where n = length a

-- getC takes a matrix, an index for a row and and index for a column as input and outputs the corresponding item of the adjugate matrix.
-- It is used when computing the adjugate matrix.
getC :: Matrix -> Int -> Int -> Double
getC a r c = (-1)^(r + c) * determinant(eliminateRowCol a r c)

-- getAdjugateMatrix takes a matrix as input and outputs the adjugate matrix of that matrix.
getAdjugateMatrix :: Matrix -> Matrix
getAdjugateMatrix a = transpose([[getC a i j | j <- [0..n - 1]] | i <- [0..n- 1]])
                    where n = length a

-- inverseMatrix takes a matrix as input and outputs its inverse.
inverseMatrix :: Matrix -> Matrix
inverseMatrix a
    | not (isSquare a) = error "Invalid input"
    | delta == 0 = []
    | otherwise = map(map((1 / delta) *)) (getAdjugateMatrix a)
                where delta = determinant a

-- multRowColumn takes two matrices, an index for a row and an index for a column as input and multiplies the given row from the first matrix with the given column from the second matrix.
-- It is used when multiplying two matrices.
multRowColumn :: Matrix -> Matrix -> Int -> Int -> Double
multRowColumn a b r c = sum(zipWith (*) row column)
                      where row = (a !! r)
                            column = ((transpose b) !! c)

-- timesM takes two matrices as input and outputs their product.
timesM :: Matrix -> Matrix -> Matrix
timesM a b
    | columnsA /= rowsB = error "Invalid input"
    | otherwise = [[multRowColumn a b r c | c <- [0..columnsB - 1]] | r <- [0..rowsA - 1]]
    where rowsA = length a
          columnsA = if((length a) > 0) then length (a !! 0) else -1
          rowsB = length b
          columnsB = if((length b) > 0) then length (b !! 0) else -2
 
-- getIdentityMatrix takes an integer n as input and outputs the identity matrix with n rows and n columns.
getIdentityMatrix :: Int -> Matrix
getIdentityMatrix 0 = []
getIdentityMatrix n = [(replicate (i - 1) 0) ++ [1] ++ (replicate (n - i) 0) | i <- [1..n]]

-- prop_inverseMatrix checks on random tests if inverse function works fine.
prop_inverseMatrix :: Matrix -> Bool
prop_inverseMatrix a
    | not (isSquare a) = True
    | (determinant a) == 0 = (inverse == [])
    | otherwise = (timesM a inverse) == (getIdentityMatrix n)
-- why does this line have a compilation error?     | otherwise = (timesM (a inverse)) == (getIdentityMatrix n) 
    where inverse = inverseMatrix a
          n = length a

-- uniform takes a list of Ints as input and outputs True if all the elements in the list have the same value and False otherwise.
uniform :: [Int] -> Bool
uniform [] = True
uniform v = all (\x -> (x == (v !! 0))) v

-- isSquare takes a matrix as input and outputs True if it is a square matrix or False otherwise.
isSquare :: Matrix -> Bool
isSquare a = (uniform lenRows) && (n > 0) && (m > 0) && (n == m)
    where lenRows = [length r | r <- a]
          n = length a
          m = if(length a > 0) then length(a !! 0) else -1
