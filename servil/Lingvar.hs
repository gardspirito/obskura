module Lingvar where

import Control.Monad.Logger
import Data.Biapplicative
import Datum
import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.Partial
import RIO.Text as T (pack, unpack)
import Text.JSON
import Yesod.Core

legMankojn :: IO LingvMankoj
legMankojn =
  HashMap.fromList .
  map (bimap pack HashSet.fromList) . fromJSObject . RIO.Partial.fromJust <$>
  runStdoutLoggingT (jsLeg "lingvar/mank.json")

data TradukPet
  = PetCxio
  | PetNur (HashSet.HashSet Text)

tradukDos :: LingvMankoj -> [Text] -> TradukPet -> [FilePath]
tradukDos mankMap tutLin tutPet =
  lingvDos <$>
  tradukDos'
    [(l, mankoj) | l <- tutLin, Just mankoj <- pure $ HashMap.lookup l mankMap]
    tutPet
  where
    lingvDos :: Text -> FilePath
    lingvDos lin = "lingvar/" ++ unpack lin ++ ".json"
    tradukDos' :: [(Text, HashSet Text)] -> TradukPet -> [Text]
    tradukDos' _ (PetNur (HashSet.null -> True)) = []
    tradukDos' [] _ = ["eo"]
    tradukDos' ((l, mankoj):ls) PetCxio = l : tradukDos' ls (PetNur mankoj)
    tradukDos' ((l, mankoj):ls) (PetNur peto) =
      let resto = mankoj `HashSet.intersection` peto
          havisEfikon = HashSet.size resto < HashSet.size peto
       in [l | havisEfikon] ++ tradukDos' ls (PetNur resto)

ordKun :: Ord a => [(a, v)] -> [(a, v)] -> [(a, v)]
ordKun (l:ls) (d:ds) =
  case fst l `compare` fst d of
    LT -> l : ordKun ls (d : ds)
    EQ -> l : ordKun ls ds
    GT -> d : ordKun (l : ls) ds
ordKun ls ds = ls <|> ds

lingvar :: Traktil TypedContent
lingvar = do
  mankMap <- akirLingvMank <$> getYesod
  lingvoj <- languages
  case tradukDos mankMap lingvoj PetCxio of
    [] -> error "Translation not found"
    [unu] -> sendFile typeJson unu
    multe -> do
      dosj <- sequence (jsLeg <$> multe)
      let kun =
            foldr ordKun [] $ fromJSObject <$> catMaybes dosj :: [( String
                                                                  , JSValue)]
      respond typeJson $ encode $ toJSObject kun
