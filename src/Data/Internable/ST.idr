module Data.Internable.ST

import Data.Internable.Core
import Data.Hashable
import Data.SortedMap
import Control.Monad.ST

%default total

export
empty : ST s InternedStringTable
empty = pure SortedMap.empty

export
--Interns the string into the table
intern : String -> STRef s InternedStringTable -> ST s InternedString
intern str refTable = do
  table <- readSTRef refTable
  let h = hash str
  case lookup h table of
       Nothing => do 
         let newTable = insert h str table
         writeSTRef refTable newTable
         pure $ MkInternedString str h
       (Just s) => pure $ MkInternedString s h

--Removes an interned string from a table
export
delete : InternedString -> STRef s InternedStringTable -> ST s ()
delete (MkInternedString str h) refTable = do
  table <- readSTRef refTable
  let newTable = delete h table
  writeSTRef refTable newTable

--Removes all interned strings from a table
export
clear : STRef s InternedStringTable -> ST s ()
clear refTable = do
  writeSTRef refTable Data.Internable.ST.empty

--Returns the number of interned strings in a table
export
size : STRef s InternedStringTable -> ST s Nat
size refTable = do
  table <- readSTRef refTable
  pure $ length $ keys table

--Returns the interned string associated with a hash
export
lookup : Hash -> STRef s InternedStringTable -> ST s (Maybe InternedString)
lookup h refTable = do
  table <- readSTRef refTable
  case lookup h table of
       Nothing => pure Nothing
       (Just s) => pure $ Just $ MkInternedString s h

