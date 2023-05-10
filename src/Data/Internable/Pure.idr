module Data.Internable.Pure

import Data.Internable.Core
import Data.Hashable
import Data.SortedMap
import Control.Monad.ST


export
--Interns the string into the table
intern : String -> InternedStringTable -> (InternedString, InternedStringTable)
intern str table = let h = hash str in
  case lookup h table of
       Nothing => (MkInternedString str h, insert h str table)
       (Just s) => (MkInternedString s h, table)

--Removes an interned string from a table
export
delete : InternedString -> InternedStringTable -> InternedStringTable
delete (MkInternedString str h) table = delete h table
  

--Removes all interned strings from a table
export
clear : InternedStringTable -> InternedStringTable
clear _  = Data.Internable.Core.empty

--Returns the number of interned strings in a table
export
size : InternedStringTable -> Nat
size table = length $ keys table

--Returns the interned string associated with a hash
export
lookup : Hash -> InternedStringTable -> (Maybe InternedString)
lookup h table = lookup h table

