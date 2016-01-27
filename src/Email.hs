{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Email where

import Network.Mail.SMTP
import Data.Text
import Data.List
import Data.Yaml
import GHC.Generics
import Data.Map
import Control.Monad.Error


data User = User {
    jmbag    :: String,
    lastname :: String,
    firstname:: String,
    age      :: Int,
    email    :: String,
    role     :: String,
    location :: String } deriving (Show, Read, Ord, Eq)





recipientInfo = ["username", "location", "mailAdress", "name", "role"]
recipient1 = ["msaric", "Zagreb, Croatia", "msaric@hotmail.com", "Mario Saric", "proffessor"]
recipient2 = ["mpetricevic", "Zagreb, Croatia", "mpetricevic@fer.hr", "Marin Petricevic", "TA"]
recipient3 = ["dlozic", "Karlovac, Croatia", "dlozic@fer.hr", "David Lozic", "student"]
recipient4 = ["dsaric", "Varazdin, Croatia", "dsaric@fer.hr", "Doria Saric", "student"]


user1 = createMap recipient1
user2 = createMap recipient2
user3 = createMap recipient3
user4 = createMap recipient4


-- | An alias for the template contents

type Template = Text



-- | Some configuration object mirroring a file.
-- | Define your own structure and use Maybe X for
-- | optional fields.

data Configuration = Configuration { host :: String,
                                     port :: String,
                                     senderAdress :: String,
                                     username :: String,
                                     password :: String
                                   } deriving (Show, Generic)

instance FromJSON Configuration


-- | Reads the e-mail configuration object from a file, using some
-- | default path to config file.

readConfig :: IO Configuration
readConfig = do
    configFile <- decodeFile "./src/info.yml" :: IO (Maybe Configuration)
    return $ case configFile of Just x -> x
                                Nothing -> error "Error reading configuration file"




-- | Parses an expression and returns either a result or an error
-- | message, if the parsing fails. If a variable is undefined, you
-- | can either return an error or replace it with an empty string.
-- | You can use a more elaborate type than String for the Map values,
-- | e.g. something that differentiates between Strings and Bools.


compileTemplate :: Template -> Map String String -> Text
compileTemplate message varsMap | patternFields == [] = message
                                | otherwise           = "error"
                                    where patternFields = toBeReplaced message



handlePattern :: Text -> Map String String -> Text
handlePattern p varsMap
    | isExpression p = evaluateExpression p varsMap
    | otherwise      = case Data.Map.lookup (unpack p) varsMap of Nothing -> pack ""
                                                                  Just x  -> pack x


evaluateExpression :: Text -> Map String String -> Text
evaluateExpression expr mapx = if ((tellMeFunction $ extractCondition ifx) mapx) then ((handlePattern $ extractIfAction ifx) mapx)
                               else ((handlePattern $ extractElseAction elsex) mapx)
    where ifx   = (ifthenelse expr) !! 0
          elsex = (ifthenelse expr) !! 1




createMap :: [String] -> Map String String
createMap maildata = fromList $ Data.List.zip recipientInfo maildata



-- IF-THEN-ELSE for example

isExpression :: Text -> Bool
isExpression txt
    | Data.Text.head txt == '@' = True
    | otherwise                 = False




-- | Sends an e-mail with given text to list of e-mail addresses
-- | using given configuration. Throws appropriate error upon failure.


--sendMail :: Configuration -> Text -> [String] -> IO ()





-- {% @if (condition) action -> condition
extractCondition :: Text -> Text
extractCondition txt = Data.Text.tail $ Data.Text.dropWhile (/= '(') (Data.Text.takeWhile (/= ')') txt)



-- {% @if (condition) action -> action
extractIfAction :: Text -> Text
extractIfAction txt = strip $ Data.Text.tail $ Data.Text.dropWhile (/= ')') txt


extractElseAction :: Text -> Text
extractElseAction expr = strip $ Data.Text.unwords $ Data.List.tail $ Data.Text.words expr


ifthenelse :: Text -> [Text]
ifthenelse expr = Data.List.init $ Data.List.tail $ Data.Text.splitOn "@" expr

tellMeFunction arg
    | arg == "isProf" = isProf
    | arg == "isStudent" = isStudent
    | arg == "isTA" = isTA



whatRole :: Map String String -> String
whatRole user = case Data.Map.lookup "role" user of Nothing -> error "No info about the role provided for this user."
                                                    Just x  -> x


isProf :: Map String String -> Bool
isProf user = if (whatRole user == "proffessor") then True else False


isTA :: Map String String -> Bool
isTA user = if (whatRole user == "TA") then True else False

isStudent :: Map String String -> Bool
isStudent user = if (whatRole user == "student") then True else False



-- Helper function that finds all the expressions between {% and %}
-- extracts parts of the template that need to be replaced... or have if-then-else conditions
toBeReplaced :: Text -> [Text]
toBeReplaced message = Data.List.tail $ Data.List.map (\x -> strip $ (splitOn closeSign x) !! 0) (splitOn openingSign message)
    where openingSign = pack "{%"
          closeSign   = pack "%}"