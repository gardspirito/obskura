module MongoKod where

import Data.Bson
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics as GHC
import GHC.TypeLits as GHC (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T
import Data.Kind
import Control.Monad.State
import qualified Language.Haskell.TH as TH

-- Bedaŭrinde, mi ne estis sufiĉe saĝa por ĝuste pritrakti kaĉon el tipoj, kiu okazis
-- en ĉi tiu modulo. Estus tre bone se iu (ekzemple mi mem) povus reviziti ĉi tiun
-- dosieron kaj forigi `unsafeCoerce`-ojn.

-- | Kvanto de kampoj por kodita objekto. Uzatas por optimumigoj.
data KodilKv = Kv0 | Kv1 | KvPlur

type family SumKv kv1 kv2 where
  SumKv 'Kv0 x = x
  SumKv x 'Kv0 = x
  SumKv 'Kv1 'Kv1 = 'KvPlur
  SumKv 'Kv1 'KvPlur = 'KvPlur
  SumKv 'KvPlur _ = 'KvPlur

data MalkodErar = FormatErar | NulErar

-- | Preterlasebla kampo, kiu ne necese estu en kodigrezulto.
data Preter a = Inkluziv !a | Preterlas deriving Generic

type ValKodil a kv = Either (a -> Value, Value -> Maybe a) (Kodil a kv 'True)

-- | Kodilo, uzota nur por malkodado de datumo.
-- | Ĉi tiu limigo ebligas nur-une direkta mapado de kodilo.
type MalkodKodil a = (forall b kv br. Kodil b kv br -> Either MalkodErar b) -> Either MalkodErar a

-- | Ĝenerala priskribo de kodigo kaj malkodigo de objektoj.
-- | @Kodil a@ konsistas ambaŭ kodigon kaj malkodigon.
-- | Kiam ambaŭ aperas en sama konstruilo samtempe, kodilo aperas antaŭ malkodilo.
-- | @kv@ priskribas nombro de kampoj de kodigata objekto. Depende de tio okazas optimumigoj.
-- | @br@ certigas, ke KodAu estas ĉiam suprnivela. Alio ne estas permesata pro Mongo kunteksto.
data Kodil a (kv :: KodilKv) (br :: Bool) where
  KodSum :: !(Kodil a kv1 'False) -> !(Kodil b kv2 'False) 
              -> Kodil (a, b) (SumKv kv1 kv2) br
  KodMap :: !(b -> a) -> !(Either MalkodErar a -> Either MalkodErar b)
              -> !(Kodil a kv br) -> Kodil b kv br
  KodPur :: !a -> Kodil a kv br
  KodEn :: DatumInterfac kv => !Text -> ValKodil a kv -> Kodil a 'Kv1 br
  KodAu :: DatumInterfac (SumKv 'Kv1 kv) => !Text -> !(KodVar a kv) -> !(HashMap Text (MalkodKodil a))
              -> Kodil a (SumKv 'Kv1 kv) 'True
  KodPreter :: !(Kodil a kv 'False) -> Kodil (Preter a) kv br

-- | Priskribo de plurvarianta strukturo.
data KodVar a kv where
  KodVarFolio :: !Text -> !(Kodil a kv 'False) -> KodVar a kv
  KodVarAu :: !(KodVar l kv1) -> !(KodVar r kv2)
    -> !(a -> Either l r) -> !(l -> a) -> !(r -> a) -> KodVar a (SumKv kv1 kv2)

kodAu :: DatumInterfac (SumKv 'Kv1 kv) => Text -> KodVar a kv -> Kodil a (SumKv 'Kv1 kv) 'True
kodAu t var = KodAu t var (kalkulVar var) where
  kalkulVar :: KodVar b kv -> HashMap Text (MalkodKodil b)
  kalkulVar (KodVarFolio n v) = HM.singleton n (\f -> f v) -- ($) ne amikas kun impredicative polymorphism.
  kalkulVar (KodVarAu l d _ lm dm) =
      aplikiAl l lm <> aplikiAl d dm
    where
      aplikiAl :: KodVar a kv -> (a -> b) -> HashMap Text (MalkodKodil b)
      aplikiAl variant levil = (\nelevita -> \legil -> levil <$> (nelevita legil)) <$> kalkulVar variant

kodMap :: (b -> a) -> (Either MalkodErar a -> Either MalkodErar b) -> Kodil a kv br -> Kodil b kv br
kodMap f1 mf1 (KodMap f2 mf2 x) = KodMap (f2 . f1) (mf1 . mf2) x
kodMap f mf x = KodMap f mf x

kodMapLiv :: (b -> a) -> (a -> b) -> Kodil a kv br -> Kodil b kv br
kodMapLiv f mf = kodMap f (mf <$>)

type Legil = Text -> Either MalkodErar Value

-- | Diversaj interfacoj al efektiva datumo depende de KodilKv.
-- | La klaso difinas funkcioj por legado kaj skribado de datumo pri objekto.
class DatumInterfac (kv :: KodilKv) where
  interfacLeg :: Value -> Either MalkodErar Legil
  unsafeSkr :: [(Text, Value)] -> Value

class DatumInterfac (MKodilKv a) => MongoKodil a where
  type MKodilKv a :: KodilKv
  type MKodilKv a = 'KvPlur
  type MKodilCxelKv a :: KodilKv
  type MKodilCxelKv a = 'Kv1
  kodil :: ValKodil a (MKodilKv a)
  kodilCxel :: Text -> Kodil a (MKodilCxelKv a) br

  default kodil :: (Generic a, GMongoKodil (Rep a), MKodilKv a ~ GMKodilKv (Rep a)) 
      => ValKodil a (MKodilKv a)
  kodil = Right $ kodMapLiv GHC.from GHC.to $ gkodil @(Rep a)
  default kodilCxel :: MKodilCxelKv a ~ 'Kv1 
      => Text -> Kodil a (MKodilCxelKv a) br
  kodilCxel nom = KodEn nom kodil

class GMongoKodil rep where
  type GMKodilKv (rep :: Type -> Type) :: KodilKv
  gkodil :: Kodil (rep a) (GMKodilKv rep) 'True

class GMongoKodilAu rep where
  type GMKodilAuKv (rep :: Type -> Type) :: KodilKv
  type GMKodilAuEtikedKv (rep :: Type -> Type) :: KodilKv
  gkodilAu :: KodVar (rep a) (GMKodilAuKv rep)

class GMongoKodilStr rep where
  type GMKodilStrKv (rep :: Type -> Type) :: KodilKv
  gkodilStr :: State Int (Kodil (rep a) (GMKodilStrKv rep) 'False)

instance (GMongoKodilAu a, DatumInterfac (SumKv 'Kv1 (GMKodilAuKv a))) => GMongoKodil (D1 m a) where
  type GMKodilKv (D1 m a) = (SumKv (GMKodilAuEtikedKv a) (GMKodilAuKv a))
  gkodil = kodMapLiv unM1 M1 $ case gkodilAu @a of
    KodVarFolio _ k -> unsafeCoerce k
    au -> unsafeCoerce $ kodAu "_et" au

instance (GMongoKodilAu a, GMongoKodilAu b) => GMongoKodilAu (a :+: b) where
  type GMKodilAuKv (a :+: b) = SumKv (GMKodilAuKv a) (GMKodilAuKv b)
  type GMKodilAuEtikedKv (a :+: b) = 'Kv1
  gkodilAu = KodVarAu (gkodilAu @a) (gkodilAu @b) (\case
      L1 x -> Left x
      R1 x -> Right x
    ) L1 R1

instance (KnownSymbol n, GMongoKodilStr a) => GMongoKodilAu (C1 ('MetaCons n m1 m2) a) where
  type GMKodilAuKv (C1 ('MetaCons n m1 m2) a) = GMKodilStrKv a
  type GMKodilAuEtikedKv (C1 ('MetaCons n m1 m2) a) = 'Kv0
  gkodilAu =
    KodVarFolio
      (T.pack $ symbolVal @n Proxy)
      (kodMapLiv unM1 M1 $
        evalState (gkodilStr @a) 0)

instance (GMongoKodilStr a, GMongoKodilStr b) => GMongoKodilStr (a :*: b) where
  type GMKodilStrKv (a :*: b) = SumKv (GMKodilStrKv a) (GMKodilStrKv b)
  gkodilStr =
    let sm = KodSum <$> gkodilStr @a <*> gkodilStr @b in
      kodMapLiv (\(a :*: b) -> (a, b)) (uncurry (:*:)) <$> sm

instance (MongoKodil v, KnownSymbol s)
  => GMongoKodilStr (S1 ('MetaSel
        ('Just s)
        m1 m2 m3)
      (Rec0 v)) where
  type GMKodilStrKv (S1 ('MetaSel ('Just s) m1 m2 m3) (Rec0 v)) = MKodilCxelKv v
  gkodilStr =
    pure $ kodMapLiv (unK1 . unM1) (M1 . K1) $ kodilCxel @v (T.pack $ symbolVal @s Proxy)

instance MongoKodil v
  => GMongoKodilStr (S1 ('MetaSel
        'Nothing
        m1 m2 m3)
      (Rec0 v)) where
  type GMKodilStrKv (S1 ('MetaSel 'Nothing m1 m2 m3) (Rec0 v)) = MKodilCxelKv v
  gkodilStr = do
    n <- get
    modify (+1)
    pure $ kodMapLiv (unK1 . unM1) (M1 . K1) $ kodilCxel @v ("_" <> tshow n)

instance GMongoKodilStr U1 where
  type GMKodilStrKv U1 = 'Kv0
  gkodilStr = pure $ KodPur U1

$(join <$> traverse 
  (\tip -> 
    [d| instance MongoKodil $(pure $ TH.ConT tip) where 
          kodil = Left (val, cast') 
    |]) 
  [ ''Bool, ''Char, ''Double, ''Float, ''Int, ''Int32
  , ''Int64, ''Integer, ''Text, ''UTCTime, ''POSIXTime
  , ''ObjectId, ''MinMaxKey, ''MongoStamp, ''Data.Bson.Symbol
  , ''Javascript, ''Regex, ''UserDefined, ''UUID, ''Function
  , ''Binary, ''Value, ''Field])

legMapAkir :: HashMap Text b -> Text -> Either MalkodErar b
legMapAkir font pet = case HM.lookup pet font of
  Just r -> Right r
  Nothing -> Left NulErar
  
instance DatumInterfac 'Kv0 where
  interfacLeg _ = Right $ const $ Left NulErar
  unsafeSkr _ = Null

instance DatumInterfac 'Kv1 where
  interfacLeg v = Right $ const (Right v)
  unsafeSkr [(_, x)] = x
  unsafeSkr _ = undefined

instance DatumInterfac 'KvPlur where
  interfacLeg (Doc d) = Right $ legMapAkir $ HM.fromList ((\(a := b) -> (a, b)) <$> d)
  interfacLeg _ = Left FormatErar
  unsafeSkr = Doc . fmap (uncurry (:=))

leg :: MongoKodil a
    => Value -> Either MalkodErar a
leg = leg' kodil
  where
    leg' :: forall a kv. DatumInterfac kv => ValKodil a kv -> Value -> Either MalkodErar a
    leg' valK v = case valK of
      Left (_, mf) -> case mf v of
        Just r -> Right r
        Nothing -> Left FormatErar
      Right k -> do
        petf <- interfacLeg @kv v
        perPetf' petf k

    perPetf' :: Legil -> Kodil b kv br -> Either MalkodErar b
    perPetf' petf = leg'' where
      leg'' :: Kodil b kv br -> Either MalkodErar b
      leg'' (KodSum a b) = (,) <$> leg'' a <*> leg'' b
      leg'' (KodMap _ mf k) = mf $ leg'' k
      leg'' (KodPur x) = Right x
      leg'' (KodEn nom k) = petf nom >>= leg' k
      leg'' (KodAu etikedCxel _ legVar) =
        petf etikedCxel >>= leg @Text >>= legMapAkir legVar >>= ($ leg'')
      leg'' (KodPreter k) = case leg'' k of
        Right x -> Right $ Inkluziv x
        Left NulErar -> Right Preterlas
        Left FormatErar -> Left FormatErar

skr :: MongoKodil a => a -> Value
skr = skr' kodil where
    skr' :: forall a kv. DatumInterfac kv => ValKodil a kv -> a -> Value
    skr' valK v = case valK of
      Left (f, _) -> f v
      Right k -> unsafeSkr @kv $ skr'' k v

    skr'' :: Kodil b kv br -> b -> [(Text, Value)]
    skr'' (KodSum k1 k2) (a, b) = skr'' k1 a <> skr'' k2 b
    skr'' (KodMap f _ k) v = skr'' k $ f v
    skr'' (KodPur _) _ = []
    skr'' (KodEn nom k) v = [(nom, skr' k v)]
    skr'' (KodAu etikedCxel kodVar _) orV = skrVar kodVar orV where
      skrVar :: KodVar b kv -> b -> [(Text, Value)]
      skrVar (KodVarFolio n k) v = [(etikedCxel, skr n)] <> skr'' k v
      skrVar (KodVarAu lVar dVar kazAnalizil _ _) v = case kazAnalizil v of
        Left l -> skrVar lVar l
        Right d -> skrVar dVar d
    skr'' (KodPreter k) v = case v of
      Inkluziv v' -> skr'' k v'
      Preterlas -> []

kodigDok :: MongoKodil a => a -> Document
kodigDok e = case skr e of
  Doc x -> x
  x -> ["_" := x]

malkodigDok :: MongoKodil a => Document -> Maybe a
malkodigDok v = hush $ leg $ case v of
    ["_" := x] -> x
    x -> Doc x
  where
    hush :: Either a b -> Maybe b
    hush (Left _)  = Nothing
    hush (Right x) = Just x

instance (MongoKodil a, DatumInterfac ((SumKv 'Kv1 (MKodilCxelKv a))))
     => MongoKodil (Preter a) where
  type MKodilKv (Preter a) = SumKv 'Kv1 (MKodilCxelKv a)
  type MKodilCxelKv (Preter a) = MKodilCxelKv a
  kodilCxel = KodPreter . kodilCxel @a

newtype Gxis a = Gxis a deriving Generic

instance (MongoKodil a, DatumInterfac (MKodilCxelKv a), DatumInterfac (SumKv 'Kv1 (MKodilCxelKv a))) => MongoKodil (Gxis a) where
  type MKodilKv (Gxis a) = MKodilCxelKv a
  type MKodilCxelKv (Gxis a) = MKodilKv a
  kodilCxel kat = 
      kodMapLiv (\(Gxis x) -> x) Gxis $ case kodil @a of
        Left k -> unsafeCoerce $ KodEn @'Kv1 kat $ Left k -- Eble ni evitu ĉi tiun uzon de coerce.
        Right k -> p k
    where
      p :: Kodil b kv br1 -> Kodil b kv br2
      p (KodSum k1 k2) = KodSum (p k1) (p k2) 
      p (KodMap f mf k) = KodMap f mf $ p k
      p (KodPur x) = KodPur x
      p (KodEn nom v) = KodEn ((kat <> ".") <> nom) v
      p (KodPreter k) = KodPreter $ p k
      p (KodAu et a1 a2) = unsafeCoerce $ KodEn kat $ Right $ KodAu et a1 a2