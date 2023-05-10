module Data.Internable.Core

import Data.SortedMap
import Data.Hashable
import Data.Ref

public export
Hash : Type
Hash = Bits64

public export
record InternedString where
  constructor MkInternedString
  str : String
  hash : Hash

public export
InternedStringTable : Type
InternedStringTable = SortedMap Hash String

public export
empty : InternedStringTable
empty = SortedMap.empty
