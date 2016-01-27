module Assignments where

import Data.Time
import System.Directory

-- | Academic year shorthand (e.g. 2015 for 2015/16)
type Year = Integer

-- | An assignment type
data AType = Homework | Exam | Project deriving (Show, Read)

-- | A an assignment configuration data structure
-- | Uses Data.Time.UTCTime
-- | If files is an empty list, ANY number of files are OK
data Configuration = Configuration { 
	published 	 :: UTCTime, -- When to publish
	deadline 	 :: UTCTime, -- Submission deadline
	lateDeadline :: UTCTime, -- Late submission deadline
	files 		 :: [String], -- File names to expect
	minScore 	 :: Double, -- Minimum achievable
	maxScore 	 :: Double,	-- Maximum achievable
	required 	 :: Double	-- Score req to pass
} deriving (Show, Read)

type UserIdentifier = String

-- | An assignment descriptor
data Assignment = Assignment { 
	year    :: Year, 
	aType   :: AType,
	number  :: Int
--	userIDs :: [UserIdentifier]
} deriving (Show, Read)

