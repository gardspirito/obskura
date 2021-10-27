{-# LANGUAGE OverloadedStrings #-}

module Lingvar where

import Data.Biapplicative
import Data.Bifunctor
import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Datum
import Text.JSON
import Text.JSON.Types
import Text.Printf
import Yesod.Core
import Yesod.Core.Content (typeJson)

jsLeg :: JSON a => String -> IO (Maybe a)
jsLeg dosNomo = do
  d <- readFile dosNomo
  case decode d of
    Ok x -> pure $ Just x
    Error er -> do
      putStrLn $ printf "Eraro dum mafermado de %s: %s" dosNomo er
      pure Nothing

legMankojn :: IO Mankoj
legMankojn =
  Map.fromList . (map $ second Set.fromList). fromJSObject . fromJust <$> jsLeg "lingvar/mank.json"

lingvar :: Traktil TypedContent
lingvar = do
  lingvoj <- languages
  mankoj <- lingvMankoj <$> getYesod
  kombinu mankoj (aldonEo lingvoj) Nothing
  where
    aldonEo :: [Text] -> [Text]
    aldonEo [] = ["eo"]
    aldonEo ls@("eo":_) = ls
    aldonEo (x:xs) = x : aldonEo xs
    lingvDos lin = "lingvar/" ++ lin ++ ".json"
    legLingv :: Text -> IO (Map.Map String Text)
    legLingv lin =
      Map.fromList . fromJSObject . fromJust <$>
      jsLeg (lingvDos $ unpack lin)
    kombinu mankMap = kombinu'
      where
        kombinu' ::
             [Text]
          -> Maybe (Set.Set Text, Map.Map String Text)
          -> Traktil TypedContent
        kombinu' _ (Just (nulMankas, trdList))
          | Set.null nulMankas =
            respond typeJson $
            encode $
            toJSObject $ Map.toList trdList
        kombinu' (x:xs) trad
          | Just nunMank <- Map.lookup (unpack x) mankMap =
            case trad of
              Nothing
                | Set.null nunMank -> sendFile typeJson $ lingvDos $ unpack x
              _ -> do
                novTrdList <- liftIO $ legLingv x
                kombinu' xs $
                  Just $
                  foldr
                    (\old gxisdat ->
                       bimap Set.intersection Map.union old <<*>> gxisdat)
                    (nunMank, novTrdList)
                    trad
          | otherwise = kombinu' xs trad
        kombinu' _ _ = error "Ne trovis tradukon."
