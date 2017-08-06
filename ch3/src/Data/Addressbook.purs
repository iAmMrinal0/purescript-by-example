module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe)

type Address = {
  street :: String,
  city   :: String,
  state  :: String
  }

type Entry = {
  firstName :: String,
  lastName :: String,
  address :: Address
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


findEntryByStreet :: Address -> AddressBook -> Maybe Entry
findEntryByStreet address = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == address.street

hasName :: String -> String -> AddressBook -> Boolean
hasName firstName lastName = not <<< null <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: String -> String -> AddressBook -> AddressBook
removeDuplicates firstName lastName = nubBy checkMatch
  where
    checkMatch :: Entry -> Entry -> Boolean
    checkMatch e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
