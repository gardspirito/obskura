module Lingvar where

import Control.Monad.Logger as Log (logError, logWarn)
import Data.Aeson (decode, decode')
import Data.Type.Nat (SNatI)
import qualified Data.Vec.DataFamily.SpineStrict as Vec
import Datum
  ( ApiRespond
  , LingvDatum(..)
  , LingvMankoj
  , Lingvo
  , Servil(akirLingvDat)
  , Traktil
  , servilErar
  , sukc, unsafeKrudaRespond
  )
import RIO
  ( Alternative((<|>))
  , Applicative((<*>), liftA2, pure)
  , Bool(True)
  , FilePath
  , Foldable(foldl')
  , Functor(fmap)
  , HashMap
  , HashSet
  , IO
  , Integral(toInteger)
  , Maybe(..)
  , Monad((>>=))
  , Ord((<))
  , Semigroup((<>))
  , Text
  , Traversable(sequence)
  , ($)
  , (&)
  , (++)
  , (.)
  , (<$>)
  , (<&>)
  , catMaybes
  , error
  , fromMaybe
  , otherwise
  , snd
  , tshow
  )
import RIO.ByteString.Lazy (readFile)
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS
import qualified RIO.Text as T
import Yesod.Core (MonadIO(..), MonadLogger, getYesod, languages)

legLingv :: IO LingvDatum
legLingv = do
  mankoj <- fromJust . decode' <$> readFile "lingvar/mank.json"
  simboloj <-
    fromJust . decode <$> readFile "lingvar/eo.json" :: IO (HashMap Text Text)
  pure $ LingvDatum mankoj (HM.keys simboloj)
  where
    fromJust (Just x) = x
    fromJust Nothing = error "Failed to load `mank.json` & `eo.json`."

class Monad m =>
      RaportMank m
  where
  raportMank :: Text -> m ()

instance RaportMank Traktil where
  raportMank t =
    languages >>= \l ->
      $(Log.logWarn) $
      t <> " in languages " <> tshow l <> ", fallback to Esperanto"

tradukDos ::
     (RaportMank m)
  => LingvMankoj
  -> [Lingvo]
  -> HashSet Text
  -> m [(HashSet Text, FilePath)]
tradukDos mankMap = tradukDos'
  where
    tradukDos' [] (HS.null -> True) = pure []
    tradukDos' [] p = do
      raportMank $ "Translation(s) not found for " <> tshow p
      pure [(p, lingvDos "eo")]
    tradukDos' (l:ls) peto
      | Just mankoj <- HM.lookup l mankMap =
        let restPeto = peto `HS.intersection` mankoj
            havisEfikon = HS.size restPeto < HS.size peto
            uzataj = peto `HS.difference` mankoj -- Pigra elkalkulo
         in ([(uzataj, lingvDos l) | havisEfikon] ++) <$> tradukDos' ls restPeto
      | otherwise = tradukDos' ls peto

lingvDos :: Text -> FilePath
lingvDos lin = "lingvar/" ++ T.unpack lin ++ ".json"

cxioTradukDos :: RaportMank m => LingvMankoj -> [Lingvo] -> m [FilePath]
cxioTradukDos mankMap = cxio'
  where
    cxio' [] = do
      raportMank "No translation(s) at all found"
      pure [lingvDos "eo"]
    cxio' (l:ls)
      | Just mankoj <- HM.lookup l mankMap =
        ([lingvDos l] ++) . (snd <$>) <$> tradukDos mankMap ls mankoj
      | otherwise = cxioTradukDos mankMap ls

-- Ne elsendu "servil."
getLingvar :: Traktil (ApiRespond (HashMap Text Text))
getLingvar = do
  lingvDatum <- akirLingvDat <$> getYesod
  lingvoj <- languages
  traduk <- cxioTradukDos (lingvMankoj lingvDatum) lingvoj
  case traduk of
    [] -> do
      servilErar
    [unu] -> do
      dosEnhavo <- readFile unu
      unsafeKrudaRespond dosEnhavo
    multe -> do
      dosjEnhavo <- sequence (readFile <$> multe)
      let dosjAnalizitaj =
            catMaybes $ decode <$> dosjEnhavo :: [HashMap Text Text]
      sukc $
        HM.fromList $
        lingvSimboloj lingvDatum <&> \simbol ->
          ( simbol
          , fromMaybe "[...]" $
            foldl'
              (\ak lin -> ak <|> HM.lookup simbol lin)
              Nothing
              dosjAnalizitaj)

krudTradukoj ::
     (MonadLogger m, RaportMank m, MonadIO m)
  => LingvMankoj
  -> [Lingvo]
  -> HashSet Text
  -> m (HashMap Text Text)
krudTradukoj = krudTradukoj'
  where
    krudTradukoj' mankoj lingvoj pet = do
      tradukDos mankoj lingvoj pet >>= procDos HM.empty
    procDos rezMap [] = pure rezMap
    procDos rezMap ((tutLegendaj, dosnomo):xs) = do
      mlinDos <- liftIO $ decode' <$> readFile dosnomo
      case mlinDos of
        Just linDos ->
          let bezonataParto =
                HS.toMap tutLegendaj &
                HM.mapWithKey (\k () -> fromMaybe "[...]" $ HM.lookup k linDos)
           in procDos (rezMap <> bezonataParto) xs
        Nothing -> do
          $(Log.logError) $ "Failed to decode file " <> T.pack dosnomo
          procDos rezMap xs

data Interlingv a where
  IPur :: a -> Interlingv a
  IPet :: HashSet Text -> ((Text -> Maybe Text) -> a) -> Interlingv a
  IApl :: Interlingv (a -> b) -> Interlingv a -> Interlingv b

instance Functor Interlingv where
  fmap f (IPur a) = IPur $ f a
  fmap f (IPet pet akir) = IPet pet $ f . akir
  fmap f (IApl prevf val) = IApl (fmap (f .) prevf) val

instance Applicative Interlingv where
  pure = IPur
  (<*>) = IApl

-- 
tKuntDe :: Text -> Interlingv a -> Interlingv a
tKuntDe kunt = tKuntDe'
  where
    tKuntDe' :: Interlingv a -> Interlingv a
    tKuntDe' i@(IPur _) = i
    tKuntDe' (IPet pet kreilo) =
      IPet (HS.map alKuntSpaco pet) (\vrt -> kreilo $ vrt . alKuntSpaco)
    tKuntDe' (IApl a b) = IApl (tKuntDe' a) (tKuntDe' b)
    alKuntSpaco = ((kunt <> ".") <>)

akirVrt :: Text -> (Text -> Maybe Text) -> Text
akirVrt nom vrt = fromMaybe ("[" <> nom <> "]") $ vrt nom

-- | Peti traduko por unu simbolo.
tpet :: Text -> Interlingv Text
tpet nom = IPet (HS.singleton nom) (akirVrt nom)

-- | Peti tradukon por listo de simboloj.
tpetList :: SNatI n => Vec.Vec n Text -> Interlingv (Vec.Vec n Text)
tpetList list =
  IPet (HS.fromList $ Vec.toList list) (\vrt -> (`akirVrt` vrt) <$> list)

-- | Krei liston de tradukoj por elnumeritaj partoj de mesaĝo.
-- Oni prenu nur komenca parto de ĉi tiu listo.
-- @tpetPartoj = tpetList ["1", "2", "3", "4", "5", ...]@
-- @tKunDe "abc" tpetPartoj = ["abc.1", "abc.2", "abc.3", "abc.4", "abc.5", ...]@
tpetPartoj :: SNatI n => Interlingv (Vec.Vec n Text)
tpetPartoj = tpetList $ Vec.tabulate (tshow . toInteger)

-- | Kombini du petojn
kajtpet :: Interlingv a -> Interlingv b -> Interlingv (a, b)
kajtpet = liftA2 (,)

traduki :: Interlingv a -> Traktil a
traduki tut = do
  lingvoj <- languages
  mankoj <- lingvMankoj . akirLingvDat <$> getYesod
  tradukoj <- krudTradukoj mankoj lingvoj tutBezon
  pure $ traduki' tradukoj
  where
    bezonate :: Interlingv a -> HashSet Text
    bezonate (IPur _) = HS.empty
    bezonate (IPet pet _) = pet
    bezonate (IApl a b) = bezonate a <> bezonate b
    tutBezon = bezonate tut
    traduki' tradukoj = traduki'' tut
      where
        traduki'' :: Interlingv a -> a
        traduki'' (IPur x) = x
        traduki'' (IPet _ f) = f (`HM.lookup` tradukoj)
        traduki'' (IApl a b) = traduki'' a (traduki'' b)
