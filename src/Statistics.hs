module Statistics where

import Data.List
import Control.Monad
import System.Directory
import Submissions
import Users
import Assignments



data Bucket = Bucket
              { rangeMin :: Double
              , rangeMax :: Double
              , count    :: Int
              } deriving Show


data Statistics = Statistics
                  { minPossible :: Double
                  , maxPossible :: Double
                  , mean        :: Double
                  , median      :: Double
                  , minAchieved :: Double
                  , maxAchieved :: Double
                  , histogram   :: [Bucket]
                  } deriving Show


data Score = Score
            { points :: Double
            , pass   :: Bool
            } deriving (Eq,Show,Read)


instance Ord Score where
    x `compare` y | pass x == pass y = points x `compare` points y
                  | otherwise = pass x `compare` pass y


data UserScore = UserScore
                 { identifier :: UserIdentifier
                 , score      :: Score
                 } deriving (Eq,Show,Read)


instance Ord UserScore where
    x `compare` y = score y `compare` score x

                
atypes = [Homework, Exam, Project]

--test data
s1 = Score 4.5 False
s2 = Score 4.4 True
assign = Assignment 2015 Homework 5

-- for testing purposes
writeScore :: Assignment -> UserIdentifier -> Score -> IO ()
writeScore a uId score = do
    let path = getAssignmentPath a ++ uId
    createDirectoryIfMissing True path
    writeFile (path ++ "/review") $ show score



readScore :: String -> IO Score
readScore path = do
    exists <- doesFileExist path
    if exists
        then do
            score <- (readFile path) 
            return (read score :: Score)
        else 
            return $ Score 0 False


getScoreFromDir :: String -> String -> IO Score
getScoreFromDir uId dir = do
    let path = dir ++ "/" ++ uId ++ "/review"
    readScore path


processScores :: [Score] -> Score
processScores scores = Score (sum $ map (points) scores) (all pass scores)


typeScore :: Integer -> AType -> UserIdentifier -> IO Score
typeScore year atype uId = do
    let rootPath = assignmentHome ++ show year ++ "/" ++ show atype
    dirs <- listDirectory rootPath
    let fullPathDirs = map (\x -> rootPath ++ "/" ++ x) dirs
    scores <- mapM (getScoreFromDir uId) fullPathDirs
    return $ processScores scores


assignmentScore :: Assignment -> UserIdentifier -> IO Score
assignmentScore a uId = do
    let path = getAssignmentPath a ++ uId ++ "/review"
    readScore path


yearScore :: Integer -> UserIdentifier -> IO Score
yearScore year uId = do
    atypeScores <- mapM ((flip . typeScore) year uId) atypes
    return $ processScores atypeScores


parseFullPath :: (String, [String]) -> [String] 
parseFullPath pair = map (\x -> (fst pair) ++ "/" ++ x ++ "/") (snd pair)


typeListUsers :: Integer -> AType -> IO [UserIdentifier]
typeListUsers year atype = do
    let rootFolder = assignmentHome ++ show year ++ "/" ++ show atype ++ "/"
    problemFolders <- listDirectory rootFolder
    let fullProblemFolders = map (\x -> rootFolder ++ x ++ "/") problemFolders
    users <- mapM (listDirectory) fullProblemFolders
    return $ nub $ concat users


yearListUsers :: Integer -> IO [UserIdentifier]
yearListUsers year = do
    let rootFolder = assignmentHome ++ show year ++ "/"
    users <- mapM (typeListUsers year) atypes
    return $ nub $ concat users


ranked :: Integer -> IO [UserScore]
ranked year = do
    users <- yearListUsers year
    scores <- mapM (yearScore year) users
    let userScores = map (\x -> uncurry UserScore x) $ zip users scores
    return $ sort userScores


typeRanked :: Integer -> AType -> IO [UserScore]
typeRanked year atype = do
    users <- typeListUsers year atype
    scores <- mapM (yearScore year) users
    let userScores = map (\x -> uncurry UserScore x) $ zip users scores
    return $ sort userScores


assignmentRanked :: Assignment -> IO [UserScore]
assignmentRanked a = do
    let assignmentPath = getAssignmentPath a
    users <- listDirectory assignmentPath
    scores <- mapM (`getScoreFromDir` assignmentPath) users
    let userScores = map (\x -> uncurry UserScore x) $ zip users scores
    return $ sort userScores