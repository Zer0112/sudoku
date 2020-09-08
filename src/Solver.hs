module Solver
    ()
where
import qualified Data.Vector as V
import           GameField

-- | Digit implemented as a bit array for better performance
data FDigits = FDigits {-# UNPACK #-} !Bool {-# UNPACK #-} !Bool {-# UNPACK #-} !Bool
                        {-# UNPACK #-} !Bool {-# UNPACK #-} !Bool {-# UNPACK #-} !Bool
                        {-# UNPACK #-} !Bool {-# UNPACK #-} !Bool  {-# UNPACK #-} !Bool


-- | makeing the sudoku a vector
fsudoku :: V.Vector  FDigits
fsudoku = V.fromList allEntries

-- a big sorted list with all entries
-- i would have used a array but it seems vector package is more popular
allEntries :: [FDigits]
allEntries = undefined

