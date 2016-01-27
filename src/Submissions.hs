module Submissions (
	createAssignment,
	getFileName,
	getAssignmentDir,
	readConfiguration,
	getConfiguration,
	listFiles
) where

import Assignments
import System.Directory
import Data.List.Split

type UserIdentifier = String

data Submission = Submission {
	assignment	:: Assignment, 
	userID 		:: UserIdentifier,
	solutions	:: [FilePath]
} deriving (Show, Read)

assignmentHome = "/home/cizl/Assignments/"

-- | Lists the user identifiers for submissions made for an assignment
--listSubmissions :: Assignment -> IO [UserIdentifier]
--listSubmissions assignment = 

-- | Views a single submission in detail
--getSubmission :: Assignment -> UserIdentifer -> IO Submission

-- | Creates a new assignment from Assignment, configuration and PDF file
-- | The PDF file should be copied, moved or symlinked so that it is
-- | accessible from the assignment directory.
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment a c fp = do 
	createDirectoryIfMissing True $ getAssignmentDir a
	copyFile fp ((getAssignmentDir a) ++ "/" ++ (getFileName fp))
	writeFile (getAssignmentDir a ++ ".config") $ show c

-- |* Gets the file name of a file path
getFileName :: FilePath -> String
getFileName fp = last $ splitOn "/" fp

-- |* Gets the file path of an assignment
getAssignmentDir :: Assignment -> FilePath
getAssignmentDir a = assignmentHome ++ (show $ year a) ++ "/" ++ 
	(show $ aType a) ++ "/" ++ (show $ number a) ++ "/"

-- | Gets the configuration object for an assignment
getConfiguration :: Assignment -> IO Configuration
getConfiguration a = do
	conf <- readConfiguration a
	return (read conf :: Configuration)

-- |* Reads the contents of the configuration file
readConfiguration :: Assignment -> IO String
readConfiguration a = readFile $ getAssignmentDir a ++ ".config"


-- | Given a solution file body, adds a solution directory/file to the
-- | directory structure of an assignment. It will indicate an error
-- | (using Maybe, exceptions, or some other mechanism) if the file is
-- | not in a defined permitted list. It will override already made
-- | submissions.
-- | Assignment -> File Body -> File Name -> Error indication (?)
--upload :: Assignment -> Text -> String -> IO (Maybe Submission)

-- | Lists the files contained in a submission
listFiles :: Submission -> IO [FilePath]
listFiles s = return $ solutions s 

-- | Computes a file path for a submission
--getSubmissionPath :: Submission -> FilePath

