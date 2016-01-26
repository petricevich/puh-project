{-# LANGUAGE TemplateHaskell #-}
module Role where

import Database.Persist.TH

data Role = Student    Integer -- Academic year shorthand (2015 for 2015/2016)
          | TA         Integer Integer -- AY shorthand range (inclusive)
          | Professor
            deriving (Eq,Ord,Show,Read)
derivePersistField "Role"
