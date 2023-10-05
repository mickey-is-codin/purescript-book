module Test.MySolutions (findEntryByStreet) where

import Prelude

import Data.Maybe (Maybe)
import Data.List (head, filter)
import Data.AddressBook (AddressBook, Entry)

filterEntryByStreet :: String -> Entry -> Boolean
filterEntryByStreet street entry = 
  _.address.street entry == street

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = 
  (head <<< filter (filterEntryByStreet street)) book