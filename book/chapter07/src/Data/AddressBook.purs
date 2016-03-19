module Data.AddressBook (Address, address)
       where

import Prelude (class Show, show, (++))

newtype Address = Address
  { street :: String
  , city :: String
  , state :: String
  }

addressToJson :: Address -> String
addressToJson (Address a) = "{street: " ++ (show a.street) ++
  ", city: " ++ (show a.city) ++
  ", state: " ++ (show a.state) ++ "}"

instance showAddress :: Show Address where
  show a = "Data.AddressBook.Address " ++ (addressToJson a)

address :: String -> String -> String -> Address
address street city state = Address
  { street: street
  , city: city
  , state: state
  }
