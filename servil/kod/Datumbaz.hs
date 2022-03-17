module Datumbaz where

import Data.Kind (Type)
import Data.Time.Clock (UTCTime)
import Database.MongoDB
import GHC.Generics as GHC
import MongoKod
import RIO
import qualified RIO.HashSet as HS
import qualified RIO.Text as T
import qualified RIO.Partial as P
import qualified GHC.TypeLits as GHC
import qualified Patience as Dif
import qualified Data.Text.ICU as ICU
import RIO.List (stripPrefix)
import Data.Text.IO (putStrLn)

-- FARENDE: Forigi KunKonservita modelo? ... kaj KolDe

---- Filtro
class Filtr a where
  type En a
  type El a
  kontroli :: En a -> Maybe (El a)

data Filtrita a where
  UnsafeFrFiltrita :: Filtr a => El a -> Filtrita a

instance (Filtr a, MongoKod (El a)) => MongoKod (Filtrita a) where
  kodig (UnsafeFrFiltrita x) = kodig @(El a) x
instance (Filtr a, MongoMalkod (El a)) => MongoMalkod (Filtrita a) where
  malkodig x = UnsafeFrFiltrita <$> malkodig @(El a) x

instance forall a. Filtr a => Filtr (Filtrita a) where
  type En (Filtrita a) = En a
  type El (Filtrita a) = Filtrita a
  kontroli x = UnsafeFrFiltrita <$> kontroli @a x

---- Aŭtomata kreado de malplenaj strukturoj.
class GMalplena f where
  gmalplena :: f a

class Malplena a where
  malplena :: a
  default malplena :: (Generic a, GMalplena (Rep a)) => a
  malplena = GHC.to gmalplena

instance GMalplena f => GMalplena (M1 _a _b f) where
  gmalplena = M1 gmalplena

instance (GMalplena fa, GMalplena fb) => GMalplena (fa :*: fb) where
  gmalplena = gmalplena :*: gmalplena

instance GMalplena U1 where
  gmalplena = U1

instance Malplena a => GMalplena (Rec0 a) where
  gmalplena = K1 malplena

instance Malplena (Maybe a) where
  malplena = Nothing
instance Malplena (Preter a) where
  malplena = Preterlas
instance Malplena [a] where
  malplena = []
instance Malplena Int where
  malplena = 0

m_ :: (Generic a, GMalplena (Rep a)) => a
m_ = GHC.to gmalplena

---- https://blog.poisson.chat/posts/2018-08-06-one-type-family.html
type Esprim a = a -> Type

type family Plenumi (e :: Esprim a) :: a

data Konst :: a -> b -> Esprim a
type instance Plenumi (Konst x _) = x

data Curryigi :: (Type -> a) -> Type -> Esprim a
type instance Plenumi (Curryigi f a) = f a

---- Aliaj helpantaj tipfamilioj
type EnList :: Type -> [Type] -> Bool
type family EnList a list where
  EnList a '[] = 'False
  EnList a (a ': xs) = 'True
  EnList a (_ ': xs) = EnList a xs

type family Xor a b where
  Xor 'True 'False = 'True
  Xor 'False 'True = 'True
  Xor _ _ = 'False

class BoolVal (b :: Bool) where
  boolVal :: Bool
instance BoolVal 'True where
  boolVal = True
instance BoolVal 'False where
  boolVal = False

type family Egal a b where
  Egal a a = 'True
  Egal _ _ = 'False

type family Se cond a b where
  Se 'True  a _ = a
  Se 'False _ b = b

type family Kaj a b where
  Kaj 'True 'True = 'True
  Kaj _ _ = 'False

------ HKD

---- Argumentoj (`A` prefikso)
data AId

---- Kuntekstoj
data Enmet
data Gxis
data ArRigard
data Malpusx
data Elekt
data Leg

class Enhavas a b

instance Enhavas a a
instance Enhavas Gxis Malpusx

-- Cxelo de strukturo, por kiu estis elektita specifa kunteksto.
type Cxelo :: Type -> (Type -> Esprim Type) -> [Type] -> Type
type family Cxelo var hkd arg
type instance Cxelo Enmet hkd _ = Plenumi (hkd Enmet)
type instance Cxelo Gxis hkd _ = Preter (Plenumi (hkd Gxis))
type instance Cxelo Malpusx hkd _ = Plenumi (hkd Elekt)
type instance Cxelo Leg hkd _ = Plenumi (hkd Leg)
type instance Cxelo Elekt hkd _ = Preter (Plenumi (hkd Elekt))

type HE :: Type -> (Type -> Esprim Type) -> [Type] -> Type
data HE var1 hkd arg where
  HE :: forall var2 var1 hkd arg. (Enhavas var1 var2, MongoKod (Cxelo var2 hkd arg))
    => !(Cxelo var2 hkd arg) -> HE var1 hkd arg

instance (BoolVal (EnList AId arg)) => MongoKod (HE a hkd arg) where
  kodig (HE x) = kodig x
  kodigCxel nom v =
    Just $
      if boolVal @(EnList AId arg) then
        "_id" := kval
      else
        nom := kval
    where
      kval = kodig v
instance (BoolVal (EnList AId arg), MongoMalkod (Cxelo var hkd arg))
    => MongoMalkod (HE var hkd arg) where
  malkodig x = HE @var <$> malkodig x
  malkodigCxel krudaNom = do
      x <- ?mk nom
      maybe mzero pure (malkodig x)
    where
      nom = if (boolVal @(EnList AId arg)) then "_id" else krudaNom
instance (MongoKod (Cxelo var hkd arg), Malplena (Cxelo var hkd arg))
    => Malplena (HE var hkd arg) where
  malplena = HE @var malplena

data HKDList :: (Type -> Type) -> Type -> Esprim Type
type instance Plenumi (HKDList hkd var) = [hkd var]

type H' var hkd arg = HE var (Curryigi hkd) arg

---- Operacioj kun simplaj tipoj

data LevSimpl :: Type -> Type -> Esprim Type
-- | Levado de simplaj tipoj al HKD.
type H var nehkd arg = HE var (LevSimpl nehkd) arg

type instance Plenumi (LevSimpl ObjectId x) = ObjectId
type instance Plenumi (LevSimpl Binary x) = Binary
type instance Plenumi (LevSimpl (Filtrita filtr) x) = El filtr
type instance Plenumi (LevSimpl Text x) = Text

data MongoKomp' a = MongoKomp' {
    kompMalpli :: !Bool,
    kompEg :: !Bool
  }
data MongoKomp a = KNeEgal (MongoKomp' a) a | KEgal a
instance MongoKod a => MongoKod (MongoKomp a) where
  kodig (KEgal x) = kodig x
  kodig (KNeEgal (MongoKomp' {kompMalpli, kompEg}) x) =
      Doc [ nom := kodig x]
    where
      nom = "$" <> (if kompMalpli then "l" else "g") <> "t" <> (if kompEg then "e" else "")

type MKompareblas a = (MongoKod a, Plenumi (LevSimpl a Elekt) ~ MongoKomp a)
type Komparil a arg = MKompareblas a => a -> H Elekt a arg
komparil' :: MKompareblas a => MongoKomp a -> H Elekt a arg
komparil' = HE @Elekt . Inkluziv

(<.) :: Komparil a arg
(<.)  = komparil' . KNeEgal (MongoKomp' True False)
(<=.) :: Komparil a arg
(<=.) = komparil' . KNeEgal (MongoKomp' True True)
(=.) :: Komparil a arg
(=.) = komparil' . KEgal
(>=.) :: Komparil a arg
(>=.) = komparil' . KNeEgal (MongoKomp' False True)
(>.) :: Komparil a arg
(>.) = komparil' .KNeEgal (MongoKomp' False False)

type instance Plenumi (LevSimpl UTCTime var) = Se (Egal Elekt var) (MongoKomp UTCTime) UTCTime

---- Konservado

-- | Sekura kontrolilo, certiganta ekzistadon de id alfiksita al objekto.
class HavasUnuId obj elRikord where
  -- | Akiri ID, kiun oni alfiksu al rikordo antaŭ sendado al datumbazo.
  alfiksendaId :: Maybe Value

type family GHavasUnuId rep where
  GHavasUnuId (M1 _ _ f) = GHavasUnuId f
  GHavasUnuId (a :*: b) = Xor (GHavasUnuId a) (GHavasUnuId b)
  GHavasUnuId (Rec0 (HE _ tip arg)) = EnList AId arg
  GHavasUnuId _ = 'False

instance (Generic (a Enmet), GHavasUnuId (Rep (a Enmet)) ~ 'True) => HavasUnuId a 'True where
  alfiksendaId = Nothing

-- | La rikordo estas kunkonservita en komunuma konservejo kaj havas specifan ID.
class GHavasUnuId (Rep (a Enmet)) ~ 'False => KunKonservita a where
  kunId :: Value

instance (KunKonservita a, Generic (a Enmet), GHavasUnuId (Rep (a Enmet)) ~ 'False)
    => HavasUnuId a 'False where
  alfiksendaId = Just $ kunId @a

class Konservita a where
  konservejo :: Text

data KolEl :: GHC.Symbol -> (Type -> Type) -> Type -> Type where
  KolEl :: hkd var -> KolEl s hkd var

instance MongoKod (hkd var) => MongoKod (KolEl s hkd var) where
  kodig (KolEl x) = kodig x
instance GHC.KnownSymbol s => Konservita (KolEl  s a) where
  konservejo = T.pack $ GHC.symbolVal @s Proxy

-- Helpiloj por `enmet`
class GKreiElekt rep where
  gkreiElekt :: Value -> Maybe (rep a)

instance GKreiElekt rep => GKreiElekt (M1 _a _b rep) where
  gkreiElekt v = M1 <$> gkreiElekt v

instance (GKreiElekt a, GKreiElekt b) => GKreiElekt (a :*: b) where
  gkreiElekt v = (:*:) <$> gkreiElekt v <*> gkreiElekt v

class GKreiElektCxel tip jes where
  gkreiElektCxel :: Value -> Maybe tip

instance (MongoMalkod (Plenumi (hkd Elekt)), EnList AId arg ~ 'True) => GKreiElektCxel (HE Elekt hkd arg) 'True where
  gkreiElektCxel v = HE @Elekt . Inkluziv <$> malkodig v
instance (MongoKod (Plenumi (hkd Elekt)), EnList AId arg ~ 'False) => GKreiElektCxel (HE Elekt hkd arg) 'False where
  gkreiElektCxel _ = Just $ HE @Elekt Preterlas

instance GKreiElektCxel (HE Elekt hkd arg) (EnList AId arg) => GKreiElekt (Rec0 (HE Elekt hkd arg)) where
  gkreiElekt v = K1 <$> gkreiElektCxel @_ @(EnList AId arg) v

--- SPECIALA MONGO KOD REGULO POR AId ĉelo, POR KE ĜI KODIĜU KIEL `_id`
-- ... samo por Mongo malkod!

-- Funkcioj
en :: MongoKod (Plenumi (LevSimpl a Enmet)) => Plenumi (LevSimpl a Enmet) -> H Enmet a arg
en = HE @Enmet

en' :: MongoKod (hkd Enmet) => hkd Enmet -> H' Enmet hkd arg
en' = HE @Enmet

enmet :: forall a. (MongoKod (a Enmet), Konservita a,
      Generic (a Enmet), HavasUnuId a (GHavasUnuId (Rep (a Enmet))),
      Generic (a Elekt), GKreiElekt (Rep (a Elekt)))
    => a Enmet -> Action IO (a Elekt)
enmet x = do
  GHC.to . P.fromJust . gkreiElekt <$> insert (konservejo @a) (kodigDok x <> alfiksi)
  where
    alfiksi = ("_id" =:) <$> catMaybes [alfiksendaId @a @(GHavasUnuId (Rep (a Enmet)))]

forig' :: forall a. (MongoKod (a Elekt), Konservita a) => Bool -> a Elekt -> Action IO ()
forig' unu v = void $ deleteMany (konservejo @a) [(kodigDok v, [SingleRemove | unu])]

forigUnu :: (MongoKod (a Elekt), Konservita a) => a Elekt -> Action IO ()
forigUnu = forig' True

forigCxiuj :: (MongoKod (a Elekt), Konservita a) => a Elekt -> Action IO ()
forigCxiuj = forig' False

-- Diferenco

data DensDifRez a = DifPreter Int | DifNov [a] | DifMalnov [a] deriving Show

densigiDif :: [Dif.Item a] -> [DensDifRez a]
densigiDif orEn = reinv <$> redukt invFand (alDens <$> orEn) where
  alDens (Dif.New a) = DifNov [a]
  alDens (Dif.Old a) = DifMalnov [a]
  alDens (Dif.Both _ _) = DifPreter 1

  redukt :: (a -> a -> Maybe a) -> [a] -> [a]
  redukt f = redukt' where
    redukt' (a:b:xs)
      | Just r <- f a b = redukt' (r:xs)
      | otherwise = a:redukt' (b:xs)
    redukt' xs = xs

  -- Kunfandi DensDifRez inverse. [DifNov [a], DifNov [b], DifNov [c]] -> [DifNov [c, b, a]]
  invFand :: DensDifRez a -> DensDifRez a -> Maybe (DensDifRez a)
  invFand (DifPreter n1) (DifPreter n2) = Just $ DifPreter (n1 + n2)
  invFand (DifNov x1)    (DifNov x2)    = Just $ DifNov (x2 <> x1)
  invFand (DifMalnov x1) (DifMalnov x2) = Just $ DifMalnov (x2 <> x1)
  invFand _ _                           = Nothing

  reinv :: DensDifRez a -> DensDifRez a
  reinv orp@(DifPreter _) = orp
  reinv (DifNov x) = DifNov $ reverse x
  reinv (DifMalnov x) = DifMalnov $ reverse x

class Difebla a where
  type DifRez a
  type DifUnuoj a
  alUnuoj :: a -> DifUnuoj a
  elUnuoj :: DifUnuoj a -> a
  dif :: DifUnuoj a -> DifUnuoj a -> DifRez a
  maldif :: DifUnuoj a -> DifRez a -> DifUnuoj a

newtype AfisxTekst = AfisxTekst Text deriving (MongoKod)
instance MongoMalkod AfisxTekst where
  malkodig v = AfisxTekst <$> malkodig v

erarSxan :: String
erarSxan = "Impossible to revert specified change of the object: the object and the change are unrelated."

instance Difebla AfisxTekst where
  type DifRez AfisxTekst = [DensDifRez Text]
  type DifUnuoj AfisxTekst = [Text]
  alUnuoj (AfisxTekst x) = ICU.brkBreak <$> ICU.breaks (ICU.breakWord "ru_RU") x
  elUnuoj = AfisxTekst . T.concat
  dif malnov nov
    = densigiDif $ Dif.diff malnov nov
  maldif orXs ((DifPreter orN):difs) = preter orXs orN where
    -- | Ni preterlasu `n` sekvaj elementoj
    preter xs 0 = maldif @AfisxTekst xs difs
    preter (x:xs) n = x : preter xs (n - 1)
    preter _ _ = error erarSxan
  maldif xs ((DifNov novoj):difs)
    | Just rest <- stripPrefix novoj xs = maldif @AfisxTekst rest difs
    | otherwise = error erarSxan
  maldif xs ((DifMalnov malnov):difs) = malnov <> maldif @AfisxTekst xs difs
  maldif xs [] = xs

debugMontrDif :: AfisxTekst -> AfisxTekst -> IO ()
debugMontrDif un du = sequence_ $ montr' <$> dif @AfisxTekst (alUnuoj un) (alUnuoj du) where
  montr' (DifPreter n) = putStrLn ("... preterlasis " <> tshow n <> " samajn vortojn")
  montr' (DifNov nov) = montrKunSign "+" nov
  montr' (DifMalnov malnov) = montrKunSign "-" malnov
  montrKunSign sign kion = putStrLn $ sign <> " " <> T.concat kion

data Dif
type instance Cxelo Dif hkd _ = Preter (Plenumi (hkd Dif))

data Difil a var where
  DifilKrud :: EnList var '[Enmet, Leg] ~ 'True => a -> Difil a var
  DifilGxis :: DensDifRez a -> Difil a Dif
instance (MongoKod a, MongoKod (DensDifRez a)) => MongoKod (Difil a var) where
  kodig (DifilKrud x) = kodig x
  kodig (DifilGxis x) = kodig x
instance (MongoMalkod a, MongoKod (DensDifRez a)) => MongoMalkod (Difil a Enmet) where
  malkodig v = DifilKrud <$> malkodig v
instance (MongoMalkod a, MongoKod (DensDifRez a)) => MongoMalkod (Difil a Leg) where
  malkodig v = DifilKrud <$> malkodig v
instance (MongoMalkod a, MongoMalkod (DensDifRez a)) => MongoMalkod (Difil a Dif) where
  malkodig v = DifilGxis <$> malkodig v

data Retposxt

retposxtPerm :: HashSet Char
retposxtPerm = HS.fromList $ "abcdefghijklmnopqrstuvwxyz0123456789.-_"

instance Filtr Retposxt where
  type En Retposxt = Text
  type El Retposxt = (Text, Text)
  kontroli (T.toLower -> minusk) = do
    case T.split (== '@') minusk of
      partoj@[un, du] -> do
        sequence_ $ guard . cxuPerm <$> partoj
        Just (un, du)
      _ -> Nothing
    where
      cxuPerm x = all (`HS.member` retposxtPerm) $ T.unpack x

rw :: MonadIO m => Pipe -> Action IO a -> m a
rw p = liftIO . access p master "obskurativ"


{-
updateMany [(
    [#id "ensalutoj"],
    Sesia {
      konf: pull (gte nun),
      nekonf: pull (gte nun)
    }
  )]

updateMany [Sesia {
    id: "ensalutoj",
    konf: pull (gte nun),
    nekonf: pull (gte nun)
  }]

updateMany :: [( Elektil, Kunt GxisK )] -> Action m _

data Kunt a where
    Kunt :: Value -> Kunt a

pull :
-}
