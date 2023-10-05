module Test.MySolutions (findEntryByStreet) where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (head, filter)
import Data.Maybe (Maybe)

filterEntryByStreet :: String -> Entry -> Boolean
filterEntryByStreet street entry = 
  _.address.street entry == street

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = 
  (head <<< filter (filterEntryByStreet street)) book

findEntryByStreet' :: String -> AddressBook -> Maybe Entry
findEntryByStreet' street = head <<< filter (eq street <<< _.address.street)