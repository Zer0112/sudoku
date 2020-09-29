{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module GameField
    ( Digit (..),
      SudokuField (..),
      Sudoku,
      validAll,
      entry,
      nrOfElem,
      nrBox,

    )
where

import           Control.Lens (Lens', getConst, makeLenses, (^.))

-- | type for the digit in the sudoku game
data Digit = EmptyField | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Eq, Ord, Enum, Bounded)

instance Show Digit where
    show EmptyField = show 0
    show One        = show 1
    show Two        = show 2
    show Three      = show 3
    show Four       = show 4
    show Five       = show 5
    show Six        = show 6
    show Seven      = show 7
    show Eight      = show 8
    show Nine       = show 9

-- for creating sudokus with arbitrary numbers
-- not implemented yet
newtype DigitFelxible = DigitFlex Integer deriving (Show, Eq, Ord)

-- | returns for how many numbers the sudoku is made - 9 for normal sudoku
-- intended for easier extention to more numbers if i want to play around with it later
-- >>> nrOfElem == fromEnum (maxBound :: Digit)
-- True
nrOfElem :: Int
nrOfElem = 4

-- | nr of boxes in one row/col
-- for now it is just fixed to 3 but i may extend it later
nrBox :: Int
nrBox = 2

-- | data type to represent the sudoku game as whole
-- It is a list of all Sudokufields
type Sudoku = [SudokuField]

-- | data type to represent the sudoku game
-- col and row starts at 1 and goes to nrOfElem
data SudokuField = SudokuField
    { _col   :: Int,
      _row   :: Int,
      _entry :: Digit
    }

makeLenses ''SudokuField




-- just proof of concept to define own Lens
lCol :: Lens' SudokuField Int
lCol f (SudokuField c r e) = (\x->SudokuField x r e) <$> (f c)

lRol :: Lens' SudokuField Int
lRol f (SudokuField c r e) = (\x->SudokuField c x e) <$> (f r)

lEntry :: Lens' SudokuField Digit
lEntry f (SudokuField c r e) = (\x->SudokuField c r x) <$> (f e)

view1 lens s = getConst $ lens s

set1 lens s = over1 lens (const s)

over1 l m = runIdentity . l (Identity . m)

newtype Identity a = Identity {runIdentity::a}

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    Identity a <*> Identity b =Identity $ a b

instance Monad Identity where
    Identity x >>=m = m x






instance Show SudokuField where
    show field =
        show (field ^. entry)

instance Eq SudokuField where
    (==) (SudokuField col1 row1 _) (SudokuField col2 row2 _)
        | col1 == col2 && row1 == row2 = True
        | otherwise = False
instance Ord SudokuField where
    compare s1 s2 = compare (func s1) (func s2)  where func s = s^.col + (nrOfElem+1)*s^.row


-- legacy section that maybe useful for view in case of later expansion
-- not really useful for the current state of the project, but showcase of len application
-- mainly getters


-- | gives True if both entries are in the same row
-- >>> rowFilter (SudokuField 1 1 One) (SudokuField 1 1 One)
-- True
rowFilter :: SudokuField -> SudokuField -> Bool
rowFilter sudField1 sudField2 = sudField1 ^. row == sudField2 ^. row

-- | give True if both entries are in the same col
colFilter :: SudokuField -> SudokuField -> Bool
colFilter sudF1 sudF2 = sudF1 ^. col == sudF2 ^. col

-- | gives True if both entries are in the same box
boxFilter :: SudokuField -> SudokuField -> Bool
boxFilter sudF1 sudF2 =
    sudF1 ^. col
        `div` nrBox
        == sudF2 ^. col
        `div` nrBox
        && sudF1 ^. row
        `div` nrBox
        == sudF2 ^. row
        `div` nrBox

-- | filter sudokufield to be in same row and col and box but not the same
allFilter :: SudokuField -> SudokuField -> Bool
allFilter =
    ( \sud x ->
          ((boxFilter sud x) || (colFilter sud x) || boxFilter sud x)
              && not ((sud ^. col == x ^. col) && (sud ^. col == x ^. col))
    )

-- | basic condition for a valid entry check
validOne :: SudokuField -> SudokuField -> Bool
validOne f1 f2
    | f1 ^. entry == EmptyField = False -- field not empty
    | otherwise = f1 ^. entry /= f2 ^. entry -- entry different

-- | List of all to check entries for one entry
listOfTestValid :: SudokuField -> Sudoku -> [SudokuField]
listOfTestValid field sudo = filter (allFilter field) sudo

-- | checks if entry is valid
validEntry :: SudokuField -> Sudoku -> Bool
validEntry field sudoku = and filterEntry
    where
        filterEntry = (map (\x -> (validOne field x)) sudoku)

-- | checks the whole sudoku
validAll :: Sudoku -> Bool
validAll sudoku = all (`validEntry` sudoku) sudoku

