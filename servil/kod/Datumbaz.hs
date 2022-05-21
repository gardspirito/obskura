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
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, MonadError (throwError))

-- FARENDE: Forigi KunKonservita modelo? ... kaj KolDe

---- Filtro
class Filtr a where
  type En a
  type El a
  kontroli :: En a -> Maybe (El a)

data Filtrita a where
  UnsafeFrFiltrita :: Filtr a => El a -> Filtrita a

instance (Filtr a, MongoKodil (El a)) => MongoKodil (Filtrita a) where
  kodil = kodMapLiv (\((UnsafeFrFiltrita x)) -> x) UnsafeFrFiltrita $ kodil

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

data Aplik :: (Type -> a) -> Type -> Esprim a
type instance Plenumi (Aplik f a) = f a

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

------ HKD

---- Argumentoj (`A` prefikso)
data AId

---- Kuntekstoj
data EnmetKunt
data GxisKunt
data ArRigard
data MalpusxKunt
data ElektKunt
data LegKunt

class Enhavas a b

instance Enhavas a a
instance Enhavas GxisKunt MalpusxKunt

-- Cxelo de strukturo, por kiu estis elektita specifa kunteksto.
type Cxelo :: Type -> (Type -> Esprim Type) -> [Type] -> Type
type family Cxelo var hkd arg where
  Cxelo EnmetKunt hkd _ = Plenumi (hkd EnmetKunt)
  Cxelo GxisKunt hkd _ = Preter (Gxis (Plenumi (hkd GxisKunt)))
  Cxelo MalpusxKunt hkd _ = Plenumi (hkd ElektKunt)
  Cxelo LegKunt hkd _ = Plenumi (hkd LegKunt)
  Cxelo ElektKunt hkd _ = Preter (Plenumi (hkd ElektKunt))

type HE :: Type -> (Type -> Esprim Type) -> [Type] -> Type
data HE var1 hkd arg where
  HE :: forall var2 var1 hkd arg. (Enhavas var1 var2, MongoKodil (Cxelo var2 hkd arg))
    => !(Cxelo var2 hkd arg) -> HE var1 hkd arg

instance (BoolVal (EnList AId arg)) => MongoKodil (HE a hkd arg) where
  kodil = kodMapLiv (\(HE x) -> x) HE $ kodil
  kodilCxel nom =
    KodEn
      (if boolVal @(EnList AId arg) then
        "_id"
      else
        nom)
      kodil

data HKDList :: (Type -> Type) -> Type -> Esprim Type
type instance Plenumi (HKDList hkd var) = [hkd var]

type H' var hkd arg = HE var (Aplik hkd) arg

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
instance MongoKodil a => MongoKodil (MongoKomp a) where
  kodig (KEgal x) = kodig x
  kodig (KNeEgal (MongoKomp' {kompMalpli, kompEg}) x) =
      Doc [ nom := kodig x]
    where
      nom = "$" <> (if kompMalpli then "l" else "g") <> "t" <> (if kompEg then "e" else "")

type MKompareblas a = (MongoKodil a, Plenumi (LevSimpl a ElektKunt) ~ MongoKomp a)
type Komparil a arg = MKompareblas a => a -> H ElektKunt a arg
komparil' :: MKompareblas a => MongoKomp a -> H ElektKunt a arg
komparil' = HE @ElektKunt . Inkluziv

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

type instance Plenumi (LevSimpl UTCTime var) = Se (Egal ElektKunt var) (MongoKomp UTCTime) UTCTime

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

instance (Generic (a EnmetKunt), GHavasUnuId (Rep (a EnmetKunt)) ~ 'True) => HavasUnuId a 'True where
  alfiksendaId = Nothing

-- | La rikordo estas kunkonservita en komunuma konservejo kaj havas specifan ID.
class GHavasUnuId (Rep (a EnmetKunt)) ~ 'False => KunKonservita a where
  kunId :: Value

instance (KunKonservita a, Generic (a EnmetKunt), GHavasUnuId (Rep (a EnmetKunt)) ~ 'False)
    => HavasUnuId a 'False where
  alfiksendaId = Just $ kunId @a

class Konservita a where
  konservejo :: Text

data KolEl :: GHC.Symbol -> (Type -> Type) -> Type -> Type where
  KolEl :: hkd var -> KolEl s hkd var

instance MongoKodil (hkd var) => MongoKodil (KolEl s hkd var) where
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

instance (MongoMalkod (Plenumi (hkd ElektKunt)), EnList AId arg ~ 'True) => GKreiElektCxel (HE ElektKunt hkd arg) 'True where
  gkreiElektCxel v = HE @ElektKunt . Inkluziv <$> malkodig v
instance (MongoKodil (Plenumi (hkd ElektKunt)), EnList AId arg ~ 'False) => GKreiElektCxel (HE ElektKunt hkd arg) 'False where
  gkreiElektCxel _ = Just $ HE @ElektKunt Preterlas

instance GKreiElektCxel (HE ElektKunt hkd arg) (EnList AId arg) => GKreiElekt (Rec0 (HE ElektKunt hkd arg)) where
  gkreiElekt v = K1 <$> gkreiElektCxel @_ @(EnList AId arg) v

-- Funkcioj
en :: MongoKodil (Plenumi (LevSimpl a EnmetKunt)) => Plenumi (LevSimpl a EnmetKunt) -> H EnmetKunt a arg
en = HE @EnmetKunt

en' :: MongoKodil (hkd EnmetKunt) => hkd EnmetKunt -> H' EnmetKunt hkd arg
en' = HE @EnmetKunt

enmet :: forall a. (MongoKodil (a EnmetKunt), Konservita a,
      Generic (a EnmetKunt), HavasUnuId a (GHavasUnuId (Rep (a EnmetKunt))),
      Generic (a ElektKunt), GKreiElekt (Rep (a ElektKunt)))
    => a EnmetKunt -> Action IO (a ElektKunt)
enmet x = do
  GHC.to . P.fromJust . gkreiElekt <$> insert (konservejo @a) (kodigDok x <> alfiksi)
  where
    alfiksi = ("_id" =:) <$> catMaybes [alfiksendaId @a @(GHavasUnuId (Rep (a EnmetKunt)))]

forig' :: forall a. (MongoKodil (a ElektKunt), Konservita a) => Bool -> a ElektKunt -> Action IO ()
forig' unu v = void $ deleteMany (konservejo @a) [(kodigDok v, [SingleRemove | unu])]

forigUnu :: (MongoKodil (a ElektKunt), Konservita a) => a ElektKunt -> Action IO ()
forigUnu = forig' True

forigCxiuj :: (MongoKodil (a ElektKunt), Konservita a) => a ElektKunt -> Action IO ()
forigCxiuj = forig' False

-- Efektiva eluzo

data Retposxt

retposxtPerm :: HashSet Char
retposxtPerm = HS.fromList "abcdefghijklmnopqrstuvwxyz0123456789.-_"

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
