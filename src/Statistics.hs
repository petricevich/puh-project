module Statistics where


data Bucket = { rangeMin :: Double
              , rangeMin :: Double
              , count    :: Int
              } deriving Show


data Statistics = { minPossible :: Double
                  , maxPossible :: Double
                  , mean        :: Double
                  , median      :: Double
                  , minAchieved :: Double
                  , maxAchieved :: Double
                  , histogram   :: [Bucket]
                  } deriving Show


data Score ={ points :: Double
            , pass   :: Bool
            } deriving (Eq,Ord,Show)


data UserScore = { identifier :: UserIdentifier
                 , score      :: Score
                 } deriving (Eq,Ord,Show)
