module Test.MySolutions 
  ( findEntryByStreet
  , findEntryByStreet'
  , isInBook) where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (head, filter, null)
import Data.Maybe (Maybe)

filterEntryByStreet :: String -> Entry -> Boolean
filterEntryByStreet street entry = 
  entry.address.street == street

filterEntryByName :: String -> String -> Entry -> Boolean
filterEntryByName first last entry = 
  entry.firstName == first && entry.lastName == last

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = 
  (head <<< filter (filterEntryByStreet street)) book

findEntryByStreet' :: String -> AddressBook -> Maybe Entry
findEntryByStreet' street = head <<< filter (eq street <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook first last book = not null (filter (filterEntryByName first last) book)