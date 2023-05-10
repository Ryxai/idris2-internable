module Data.Internable.IO

import Data.Internable.Core
import Data.Hashable
import Data.SortedMap
import Data.IORef

%default total


export
--Interns the string into the table
intern : String -> IORef InternedStringTable -> IO InternedString
intern str refTable = do
  table <- readIORef refTable
  let h = hash str
  case lookup h table of
       Nothing => do 
         let newTable = insert h str table
         writeIORef refTable newTable
         pure $ MkInternedString str h
       (Just s) => pure $ MkInternedString s h

--Removes an interned string from a table
export
delete : InternedString -> IORef InternedStringTable -> IO ()
delete (MkInternedString str h) refTable = do
  table <- readIORef refTable
  let newTable = delete h table
  writeIORef refTable newTable

--Removes all interned strings from a table
export
clear : IORef InternedStringTable -> IO ()
clear refTable = do
  writeIORef refTable Data.Internable.Core.empty

--Returns the number of interned strings in a table
export
size : IORef InternedStringTable -> IO Nat
size refTable = do
  table <- readIORef refTable
  pure $ length $ keys table

--Returns the interned string associated with a hash
export
lookup : Hash -> IORef InternedStringTable -> IO (Maybe InternedString)
lookup h refTable = do
  table <- readIORef refTable
  case lookup h table of
       Nothing => pure Nothing
       (Just s) => pure $ Just $ MkInternedString s h

