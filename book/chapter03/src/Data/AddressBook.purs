module Data.AddressBook where

import Prelude
import Data.List
import Data.Maybe
import Control.Plus (empty)

type Address =
  { street :: String
  , city :: String
  , province :: String
  }

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress address =
  address.street ++ ", " ++ address.city ++ ", " ++ address.province

showEntry :: Entry -> String
showEntry entry =
  entry.lastName ++ ", " ++ entry.firstName ++ ": " ++ (showAddress entry.address)

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

containsEntry :: String -> String -> AddressBook -> Boolean
containsEntry first last =
  isJust <<< (findEntry first last)

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry e = e.firstName == firstName && e.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates =
  Data.List.nubBy eq
  where
    eq :: Entry -> Entry -> Boolean
    eq a b = (a.firstName == b.firstName)
             && (a.lastName == b.lastName)
