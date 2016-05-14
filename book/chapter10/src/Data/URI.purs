module Data.URI (encodeUriComponent
                ,decodeUriComponent) where

foreign import encodeUriComponent :: String -> String

foreign import decodeUriComponent :: String -> String
