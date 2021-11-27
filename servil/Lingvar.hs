{-# LANGUAGE OverloadedStrings #-}

module Lingvar where

import Control.Applicative
import Data.Biapplicative
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Datum
import Text.JSON
import Text.Printf
import Yesod.Core

jsLeg :: JSON a => FilePath -> IO (Maybe a)
jsLeg dosNomo = do
  d <- readFile dosNomo
  case decode d of
    Ok x -> pure $ Just x
    Error er -> do
      putStrLn $ printf "Eraro dum mafermado de %s: %s" dosNomo er
      pure Nothing

legMankojn :: IO LingvMankoj
legMankojn =
  Map.fromList . map (bimap pack Set.fromList) . fromJSObject . fromJust <$>
  jsLeg "lingvar/mank.json"

data TradukPet
  = PetCxio
  | PetNur (Set.Set Text)

tradukDos :: LingvMankoj -> [Text] -> TradukPet -> [FilePath]
tradukDos mankMap lingvoj tutaPeto =
  let filtr = pet' (flip Map.lookup mankMap <$> lingvoj) tutaPeto
   in [lingvDos l | (l, f) <- zip (lingvoj ++ ["eo"]) filtr, f]
  where
    pet' :: [Maybe (Set.Set Text)] -> TradukPet -> [Bool]
    pet' _ (PetNur nul)
      | null nul = []
    pet' [] _ = [True] -- Uzu aprioran lingvon
    pet' (Nothing:ls) peto = False : pet' ls peto
    pet' ((Just l):ls) PetCxio = True : pet' ls (PetNur l)
    pet' ((Just l):ls) (PetNur peto) =
      let komun = l `Set.intersection` peto
       in (Set.size komun < Set.size peto) : pet' ls (PetNur komun)
    lingvDos lin = "lingvar/" ++ unpack lin ++ ".json"

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
    [] -> error "Ne estas traduko"
    [unu] -> sendFile typeJson unu
    multe -> do
      dosj <- liftIO $ sequence (jsLeg <$> multe)
      let kun =
            foldr ordKun [] $ fromJSObject . fromJust <$> dosj :: [( String
                                                                   , JSValue)]
      respond typeJson $ encode $ toJSObject kun
