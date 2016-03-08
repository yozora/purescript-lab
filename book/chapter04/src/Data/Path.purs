module Data.Path
  ( Path()
  , root
  , ls
  , filename
  , size
  , isDirectory
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))

data Path
  = Directory String (Array Path)
  | File String Int

instance showPath :: Show Path where
  show = filename

root :: Path
root =
  Directory "/"
    [ Directory "/bin/"
      [ File "/bin/cp" 24800
      , File "/bin/ls" 34700
      , File "/bin/mv" 20200
      ]
    , Directory "/etc/"
      [ File "/etc/hosts" 300
      ]
    , Directory "/home"
      [ Directory "/home/user/"
        [ File "/home/user/todo.txt" 1020
        , Directory "/home/user/code/"
          [ Directory "/home/user/code/js/"
            [ File "/home/user/code/js/test.js" 40000
            ]
          , Directory "/home/user/code/haskell"
            [ File "/home/user/code/haskell/test.hs" 5000
            ]
          ]
        ]
      ]
    ]

ls :: Path -> Array Path
ls (Directory _ xs) = xs
ls _ = []

filename :: Path -> String
filename (Directory name _) = name
filename (File name _) = name

size :: Path -> Maybe Int
size (File _ size) = Just size
size _ = Nothing

isDirectory :: Path -> Boolean
isDirectory (Directory _ _) = true
isDirectory _ = false
