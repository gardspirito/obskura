module Lingvar where

import Control.Monad.Logger
import Data.Biapplicative
import Datum
import RIO
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS
import qualified RIO.Partial as P
import qualified RIO.Partial
import qualified RIO.Text as T
import Text.JSON
import Yesod.Core

legMankojn :: IO LingvMankoj
legMankojn =
  HM.fromList .
  map (bimap T.pack HS.fromList) . fromJSObject . RIO.Partial.fromJust <$>
  runStdoutLoggingT (jsLeg "lingvar/mank.json")

data TradukPet
  = PetCxio
  | PetNur (HashSet Text)

tradukDos ::
     TradukPet -> [Lingvo] -> LingvMankoj -> [(Maybe (HashSet Text), FilePath)]
tradukDos tutPet tutLin mankMap =
  (lingvDos <$>) <$>
  tradukDos'
    [(l, mankoj) | l <- tutLin, Just mankoj <- pure $ HM.lookup l mankMap]
    tutPet
  where
    lingvDos :: Text -> FilePath
    lingvDos lin = "lingvar/" ++ T.unpack lin ++ ".json"
    tradukDos' ::
         [(Lingvo, HashSet Text)]
      -> TradukPet
      -> [(Maybe (HashSet Text), Lingvo)]
    tradukDos' _ (PetNur (HS.null -> True)) = []
    tradukDos' [] rest = [(Just $ rest' rest, "eo")]
      where
        rest' PetCxio = HS.empty
        rest' (PetNur l) = l
    tradukDos' ((l, mankoj):ls) PetCxio =
      (Nothing, l) : tradukDos' ls (PetNur mankoj)
    tradukDos' ((l, mankoj):ls) (PetNur peto) =
      let resto = mankoj `HS.intersection` peto
          havisEfikon = HS.size resto < HS.size peto
       in [(Just $ mankoj `HS.difference` peto, l) | havisEfikon] ++
          tradukDos' ls (PetNur resto)

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
  case snd <$> tradukDos PetCxio lingvoj mankMap of
    [] -> error "Translation not found"
    [unu] -> sendFile typeJson unu
    multe -> do
      dosj <- sequence (jsLeg <$> multe)
      let kun =
            foldr ordKun [] $ fromJSObject . P.fromJust <$> dosj :: [( String
                                                                     , JSValue)]
      respond typeJson $ encode $ toJSObject kun

krudTradukoj ::
     MonadIO m
  => HashSet Text
  -> [Lingvo]
  -> LingvMankoj
  -> m (HashMap Text Text)
krudTradukoj = krudTradukoj'
  where
    krudTradukoj' pet lingvoj =
      procDos HM.empty . tradukDos (PetNur pet) lingvoj
    procDos rezMap [] = pure rezMap
    procDos rezMap ((P.fromJust -> tutLegendaj, dosnomo):xs) = do
      linoj <- fromJSObject . P.fromJust <$> runStdoutLoggingT (jsLeg dosnomo)
      procDos (foldl' (&) rezMap $ procLin tutLegendaj linoj) xs
      where
        procLin (HS.null -> True) _ = []
        procLin legendaj ((T.pack -> nom, val):linoj)
          | nom `HS.member` legendaj =
            HM.insert nom val : procLin (HS.delete nom legendaj) linoj
          | otherwise = procLin legendaj linoj
        procLin _ _ = error "Translation not found"



data Interlingv a where
  IPet :: HashSet Text -> (HashMap Text Text -> a) -> Interlingv a
  IPur :: a -> Interlingv a
  IApl :: Interlingv (a -> b) -> Interlingv a -> Interlingv b

instance Functor Interlingv where
  fmap f (IPet pet akir) = IPet pet $ f . akir
  fmap f (IPur a) = IPur $ f a
  fmap f (IApl prevf val) = IApl (fmap (f .) prevf) val
  
instance Applicative Interlingv where
  pure = IPur
  (<*>) = IApl

(<.>) :: Text -> Text -> Text
(<.>) a b = a <> "." <> b

infixr 5 <.>

-- Peti traduko por unu simbolo.
tpet :: Text -> Interlingv Text
tpet nom = IPet (HS.singleton nom) (P.fromJust . HM.lookup nom)

-- Peti tradukon por listo de simboloj
tpetList :: [Text] -> Interlingv [Text]
tpetList list = IPet (HS.fromList list) (\m -> P.fromJust <$> (HM.lookup <$> list <*> [m]))

-- Peti tradukon por kelkaj partoj en sama spaco.
-- Ekz. tpetPart "abc.def" 3 = tpetList ["abc.def.1", "abc.def.2", "abc.def.3"] 
tpetPart :: Text -> Int -> Interlingv [Text]
tpetPart spac nom = tpetList $ (spac <.>) . tshow <$> [1..nom] 

kajtpet :: Interlingv a -> Interlingv b -> Interlingv (a, b)
kajtpet = liftA2 (,)

traduki :: Interlingv a -> Traktil a
traduki tut = do
    lingvoj <- languages
    mankoj <- akirLingvMank <$> getYesod
    tradukoj <- krudTradukoj tutBezon lingvoj mankoj
    pure $ traduki' tradukoj
  where
    bezonate :: Interlingv a -> HashSet Text
    bezonate (IPet pet _) = pet
    bezonate (IPur _) = HS.empty
    bezonate (IApl a b) = bezonate a <> bezonate b
    tutBezon = bezonate tut 
    traduki' tradukoj = traduki'' tut where
      traduki'' :: Interlingv a -> a
      traduki'' (IPet _ f) = f tradukoj
      traduki'' (IPur x) = x
      traduki'' (IApl a b) = traduki'' a (traduki'' b) 