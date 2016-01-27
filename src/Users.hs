{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Users ( createUser
             , updateUser
             , deleteUser
             , listUsers
             , listUsersInRole
             , getUser
             , isRoleInYear
             ) where


import Database.Persist.Sqlite
import Database.Persist.TH
import Crypto.PasswordStore
import Data.ByteString.Char8 hiding (map)
import Role


type UserIdentifier = String


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    identifier UserIdentifier
    email   String
    pwdHash String
    role    Role
    UniqueIdentifier identifier
    deriving Show
|]


database = "db.sqlite"


createUser :: UserIdentifier -> String -> String -> Role -> IO User
createUser ident email pass role = runSqlite database $ do
    runMigration migrateAll
    --passHash <- makePassword (pack pass) 17
    let newUser = User ident email pass role
    _ <- insert newUser
    return newUser


updateUser :: User -> IO ()
updateUser user = runSqlite database $ do
    runMigration migrateAll

    userToBeUpdated <- getBy $ UniqueIdentifier (userIdentifier user)
    case userToBeUpdated of 
        Just (Entity uId _) -> replace uId user
        Nothing -> error $ "User " ++ show (userIdentifier user) ++ " could not be found in the database"
    

deleteUser :: UserIdentifier -> IO ()
deleteUser ident = runSqlite database $ do
    userToBeDeleted <- getBy $ UniqueIdentifier ident
    case userToBeDeleted of 
        Just (Entity uId _) -> delete uId
        Nothing             -> error $ "User with the identifier " ++ show ident ++ " is not in the database"


listUsers :: IO [User]
listUsers = runSqlite database $ do
    users <- selectList ([] :: [Filter User]) []
    return $ map (\ (Entity _ u) -> u) users


-- filters according to full role, so Student 2014 and Sudent 2015 insteado of just Student
-- may be a bit awkward to use with TAs as the ranges have to match perfectly, 
-- and you can't just query for all TAs who worked in 2015
listUsersInRole :: Role -> IO [User]
listUsersInRole role = runSqlite database $ do
    users <- selectList [UserRole ==. role] []
    return $ map (\ (Entity _ u) -> u) users


getUser :: UserIdentifier -> IO User
getUser ident = runSqlite database $ do
    user <- getBy $ UniqueIdentifier ident
    case user of 
        Just (Entity _ u) -> return u
        Nothing           -> error $ "User with the identifier " ++ show ident ++ " is not in the database"


isRole :: User -> Role -> Bool
isRole (User {userRole = Professor})   Professor   = True
isRole (User {userRole = (TA _ _)})    (TA _ _)    = True
isRole (User {userRole = (Student _)}) (Student _) = True
isRole _                               _           = False


isInYear :: User -> Integer -> Bool
isInYear (User {userRole = Professor})          _ = True
isInYear (User {userRole = (TA startTA endTA)}) x = startTA <= x && x <= endTA
isInYear (User {userRole = (Student year)})     x = year == x


isRoleInYear :: User -> Role -> Integer -> Bool
isRoleInYear user role x = isRole user role && isInYear user x
