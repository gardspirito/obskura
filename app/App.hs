{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, GADTs, DerivingStrategies, GeneralisedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
import Yesod
import Database.Persist
import Database.Persist.TH
import Database.Persist.MongoDB
import Database.MongoDB.Connection
import Language.Haskell.TH (Type(..))

share
    [mkPersist (mkPersistSettings (ConT ''MongoContext))]
    [persistLowerCase|
|]

data Servil = Servil ConnectionPool

mkYesod "Servil" [parseRoutes|
/ HomeR GET
|]

instance Yesod Servil

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello world!|]

main :: IO ()
main = do
    mongo <- createMongoDBPool
        "obskura"
        "10.233.2.2"
        defaultPort
        Nothing
        defaultPoolStripes
        defaultStripeConnections
        defaultConnectionIdleTime
    print mongo
    warp 3000 $ Servil mongo