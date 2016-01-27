import Assignments
import Submissions
import Data.Time


config = Configuration 
	(read "2016-01-26 23:49:46.861565 UTC" :: UTCTime)
	(read "2016-01-26 23:49:46.861565 UTC" :: UTCTime)
	(read "2016-01-26 23:49:46.861565 UTC" :: UTCTime)
	["Assignment.hs", "Homework.hs", "Exercises.hs"]
	5 10 7

assign= Assignment 2015 Homework 3

sub = Submission "Marin" assign ["Exercises.hs", "Homework.hs"]

main :: IO ()
main = do
	putStrLn "da"
