module Submissions where 

import Assignments
import System.Directory
import Data.List.Split
import Data.Text hiding (last, splitOn)


data Submission = Submission {
	userID 		:: UserIdentifier,
	assignment	:: Assignment, 
	solutions	:: [FilePath]
} deriving (Show, Read)

assignmentHome = "/tmp/Assignments/"

-- | Lists the user identifiers for submissions made for an assignment
listSubmissions :: Assignment -> IO [UserIdentifier]
listSubmissions a = listDirectory $ getAssignmentPath a
	

-- | Views a single submission in detail
getSubmission :: Assignment -> UserIdentifier -> IO Submission
getSubmission a id = do
	sol <- listDirectory $ getSubmissionPath $ Submission id a []
	return $ Submission id a sol

-- | Creates a new assignment from Assignment, configuration and PDF file
-- | The PDF file should be copied, moved or symlinked so that it is
-- | accessible from the assignment directory.
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment a c fp = do 
	createDirectoryIfMissing True $ getAssignmentPath a
	copyFile fp ((getAssignmentPath a) ++ "/" ++ (getFileName fp))
	writeFile (getAssignmentPath a ++ ".config") $ show c

-- |* Gets file name of a file path
getFileName :: FilePath -> String
getFileName fp = last $ splitOn "/" fp

-- |* Computes a file path for an assignment
getAssignmentPath :: Assignment -> FilePath
getAssignmentPath a = assignmentHome ++ (show $ year a) ++ "/" ++ 
	(show $ aType a) ++ "/" ++ (show $ number a) ++ "/"

-- | Gets the configuration object for an assignment
getConfiguration :: Assignment -> IO Configuration
getConfiguration a = do
	conf <- readConfiguration a
	return (read conf :: Configuration)

-- |* Reads the contents of the configuration file
readConfiguration :: Assignment -> IO String
readConfiguration a = readFile $ getAssignmentPath a ++ ".config"


-- | Given a solution file body, adds a solution directory/file to the
-- | directory structure of an assignment. It will indicate an error
-- | (using Maybe, exceptions, or some other mechanism) if the file is
-- | not in a defined permitted list. It will override already made
-- | submissions.
-- | Assignment -> File Body -> File Name -> Error indication (?)
--upload :: Assignment -> Text -> String -> IO (Maybe Submission)
{-}
upload a fb fn = do
	conf <- getConfiguration a
	let path = getAssignmentPath a ++ userID a ++ "/"
	if (fn `elem` files conf) 
        then do
        	createDirectoryIfMissing True path
        	writeFile (path ++ fn) $ unpack fb
        	return $ Just Submission (userID a)
        else 
            return Nothing
-}

submit :: Assignment -> UserIdentifier -> FilePath -> IO ()
submit a uId fp = do
	copyFile fp (getAssignmentPath a ++ uId)


-- | Lists the files contained in a submission
listFiles :: Submission -> IO [FilePath]
listFiles s = return $ solutions s 

-- | Computes a file path for a submission
getSubmissionPath :: Submission -> FilePath
getSubmissionPath s = (getAssignmentPath $ assignment s) ++ (userID $ s) ++ "/"

