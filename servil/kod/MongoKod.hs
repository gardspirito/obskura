module MongoKod where--(MongoKod(..), AutoMongoKod, kodigDok, malkodigDok) where

import Data.Bson
  ( Binary(..)
  , Field((:=))
  , Function(..)
  , Javascript
  , MD5(..)
  , MinMaxKey
  , MongoStamp(..)
  , ObjectId
  , Regex
  , Symbol(..)
  , UUID(..)
  , UserDefined(..)
  , Val(cast', val)
  , Value(..)
  , (=:), Document
  )
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics as GHC
  ( C1
  , D1
  , Generic(..)
  , K1(K1)
  , M1(M1)
  , Meta(MetaCons, MetaSel)
  , Rec0
  , S1,type  (:*:) ((:*:)), type (:+:) (L1, R1), U1 (U1)
  )
import GHC.TypeLits as GHC (KnownSymbol, symbolVal)
import RIO
  ( Applicative(pure)
  , Bool (True, False)
  , Char
  , Double
  , Either (Left, Right)
  , Eq((==))
  , Float
  , Int
  , Int32
  , Int64
  , Integer
  , Maybe(..)
  , Num((+))
  , Proxy(Proxy)
  , Semigroup((<>))
  , Text
  , ($)
  , (.)
  , (<$>)
  , fst
  , tshow
  , (<|>), (>>>), id, otherwise, Void, absurd, maybeToList, maybe
  )
import qualified RIO.Partial as P
import qualified RIO.HashMap as HM
import qualified RIO.Text as T
import qualified RIO.List as L
import Data.Kind
import Control.Monad.Except

class GMongoKod f (dok :: Bool) where
  gkodig :: f a -> Value
class GMongoMalkod f (dok :: Bool) where
  gmalkodig :: Value -> Maybe (f a)

type family Au a b where
  Au 'True _ = 'True
  Au 'False b = b

type family DokStr (rep :: Type -> Type) where
  DokStr (D1 _ x) = DokStr x 
  DokStr (a :+: b) = Au (DokStr a) (DokStr b)
  DokStr (C1 _ U1) = 'False
  DokStr (C1 _ _) = 'True

alAkiril :: [Field] -> Text -> Maybe Value
alAkiril d x = HM.lookup x (HM.fromList $ alTup <$> d)
  where
    alTup (a := b) = (a, b)

-- Ne estas branĉoj, do ni kodigas strukturon ne konservante etikedon.
instance GEnhavKod e [Field] => GMongoKod (D1 m1 (C1 m2 e)) dokStr where
  gkodig (M1 (M1 x)) = Doc $ fst $ kodigEn x ([], 0)
instance GEnhavMalkod e Text => GMongoMalkod (D1 m1 (C1 m2 e)) dokStr where
  gmalkodig (Doc d) =
    let ?mk = alAkiril d in
      M1 . M1 . fst <$> malkodigEn 0
  gmalkodig _ = Nothing

class GEtikedKod f k where
  kodigEt :: f a -> k -> Maybe (Text, k)
class GEtikedMalkod f sxlos where
  malkodigEt :: MonadPlus m => (?mk :: sxlos -> m Value) => Text -> m (f a)

-- Kodigi kun etikedo
instance GEtikedKod (a :+: b) [Field]
    => GMongoKod (D1 m (a :+: b)) 'True where
  gkodig (M1 x) =
    let (et, rest) = P.fromJust $ kodigEt x [] in
      Doc $ ("_et" =: et):rest
instance GEtikedMalkod (a :+: b) Text
    => GMongoMalkod (D1 m (a :+: b)) 'True where
  gmalkodig (Doc d) =
    let ?mk = alAkiril d in do
      String et <- ?mk "_et"
      M1 <$> malkodigEt et
  gmalkodig _ = Nothing

-- Konservi nur etikedon (DokStr rep ~ 'False => la strukturo ne enhavas
-- datumon krom etikedo)
instance GEtikedKod (a :+: b) () => GMongoKod (D1 m (a :+: b)) 'False where
  gkodig (M1 x) = String $ fst $ P.fromJust $ kodigEt x ()
instance GEtikedMalkod (a :+: b) Void => GMongoMalkod (D1 m (a :+: b)) 'False where
  gmalkodig (String x) = let ?mk = absurd in M1 <$> malkodigEt @_ @_ @Maybe x 
  gmalkodig _ = Nothing

instance (GEtikedKod a k, GEtikedKod b k)
    => GEtikedKod (a :+: b) k where
  kodigEt (L1 x) = kodigEt x
  kodigEt (R1 x) = kodigEt x
instance (GEtikedMalkod a sxlos, GEtikedMalkod b sxlos)
    => GEtikedMalkod (a :+: b) sxlos where
  malkodigEt n = (L1 <$> malkodigEt @a n) <|> (R1 <$> malkodigEt @b n)

instance (KnownSymbol nom, GEnhavKod e k) 
    => GEtikedKod (C1 ('MetaCons nom m1 m2) e) k where
  kodigEt (M1 x) k = Just (T.pack $ symbolVal @nom Proxy, fst $ kodigEn x (k, 0))
instance (KnownSymbol nom, GEnhavMalkod e sxlos) 
    => GEtikedMalkod (C1 ('MetaCons nom m1 m2) e) sxlos where
  malkodigEt nom =
    if T.pack (symbolVal @nom Proxy) == nom then
      M1 . fst <$> malkodigEn 0
    else
      mzero

class GEnhavKod f k where
  kodigEn :: f a -> (k, Int) -> (k, Int)
class GEnhavMalkod f sxlos where
  malkodigEn :: MonadPlus m => (?mk :: sxlos -> m Value) => Int -> m (f a, Int)

instance forall a b k. (GEnhavKod a k, GEnhavKod b k) => GEnhavKod (a :*: b) k where
  kodigEn (a :*: b) = kodigEn a >>> kodigEn b
instance forall a b mk. (GEnhavMalkod a mk, GEnhavMalkod b mk) 
    => GEnhavMalkod (a :*: b) mk where
  malkodigEn n1 = do
    (a, n2) <- malkodigEn @a n1
    (b, n3) <- malkodigEn @b n2
    pure (a :*: b, n3)

{-
kodigCxel :: forall a. MongoKod a => Text -> a -> [Field] -> [Field]
kodigCxel nom v k = do
  let kodigita = kodig v
  if not (kodigNul @a) && kodigita == Null then
    k
  else
    (nom =: kodigita):k

malkodigCxel :: forall a. (MongoMalkod a, (?mk :: GAkiril)) => Text -> Maybe a
malkodigCxel nom = malkodig v
  where
      v = fromMaybe Null $ ?mk nom
-}

instance MongoKod a => GEnhavKod (S1 ('MetaSel 'Nothing m1 m2 m3) (Rec0 a)) [Field] where
  kodigEn (M1 (K1 x)) (k, n) = 
    (maybeToList (kodigCxel ("_"<>tshow n) x) <> k, n+1)
instance MongoMalkod a => GEnhavMalkod (S1 ('MetaSel 'Nothing m1 m2 m3) (Rec0 a)) Text where
  malkodigEn n = do
    v <- malkodigCxel ("_"<>tshow n)
    pure (M1 $ K1 v, n+1)

instance forall a m1 m2 m3 nom. (KnownSymbol nom, MongoKod a)
    => GEnhavKod (S1 ('MetaSel ('Just nom) m1 m2 m3) (Rec0 a)) [Field] where
  kodigEn (M1 (K1 x)) (k, n) =
    (maybeToList (kodigCxel (T.pack $ symbolVal @nom Proxy) x) <> k, n)
instance forall a m1 m2 m3 nom. (KnownSymbol nom, MongoMalkod a)
    => GEnhavMalkod (S1 ('MetaSel ('Just nom) m1 m2 m3) (Rec0 a)) Text where
  malkodigEn n = do
    v <- malkodigCxel (T.pack $ symbolVal @nom Proxy)
    pure (M1 $ K1 v, n)

instance GEnhavKod U1 k where
  kodigEn U1 = id
instance GEnhavMalkod U1 sxlos where
  malkodigEn n = pure (U1, n)

class MongoKod a where
  kodig :: a -> Value
  kodigCxel :: Text -> a -> Maybe Field
  
  default kodig :: (r ~ Rep a, m ~ DokStr r, Generic a, GMongoKod r m) => a -> Value
  kodig x = gkodig @_ @(DokStr (Rep a)) (GHC.from x)

  kodigCxel nom v = Just (nom := kodig v) 


class MongoKod a => MongoMalkod a where
  malkodig :: Value -> Maybe a
  default malkodig :: (r ~ Rep a, m ~ DokStr r, Generic a, GMongoMalkod r m) => Value -> Maybe a
  malkodig x = GHC.to <$> gmalkodig @_ @(DokStr (Rep a)) x

  malkodigCxel :: MonadPlus m => (?mk :: Text -> m Value) => Text -> m a
  malkodigCxel nom = do
    v <- ?mk nom
    maybe mzero pure (malkodig v)

newtype Kovr a = Kovr a

instance Val a => MongoKod (Kovr a) where
  kodig (Kovr x) = val x
instance Val a => MongoMalkod (Kovr a) where
  malkodig x = Kovr <$> cast' x

malkovr :: Val a => Value -> Maybe a
malkovr x = malkovr' <$> malkodig x where
  malkovr' (Kovr a) = a

deriving via Kovr Bool instance MongoKod Bool
instance MongoMalkod Bool where
  malkodig = malkovr

deriving via Kovr Char instance MongoKod Char
instance MongoMalkod Char where
  malkodig = malkovr

deriving via Kovr Double instance MongoKod Double
instance MongoMalkod Double where
  malkodig = malkovr

deriving via Kovr Float instance MongoKod Float
instance MongoMalkod Float where
  malkodig = malkovr

deriving via Kovr Int instance MongoKod Int
instance MongoMalkod Int where
  malkodig = malkovr

deriving via Kovr Int32 instance MongoKod Int32
instance MongoMalkod Int32 where
  malkodig = malkovr

deriving via Kovr Int64 instance MongoKod Int64
instance MongoMalkod Int64 where
  malkodig = malkovr

deriving via Kovr Integer instance MongoKod Integer
instance MongoMalkod Integer where
  malkodig = malkovr

deriving via Kovr Text instance MongoKod Text
instance MongoMalkod Text where
  malkodig = malkovr

deriving via Kovr UTCTime instance MongoKod UTCTime
instance MongoMalkod UTCTime where
  malkodig = malkovr

deriving via Kovr POSIXTime instance MongoKod POSIXTime
instance MongoMalkod POSIXTime where
  malkodig = malkovr

deriving via Kovr ObjectId instance MongoKod ObjectId
instance MongoMalkod ObjectId where
  malkodig = malkovr

deriving via Kovr MinMaxKey instance MongoKod MinMaxKey
instance MongoMalkod MinMaxKey where
  malkodig = malkovr

deriving via Kovr MongoStamp instance MongoKod MongoStamp
instance MongoMalkod MongoStamp where
  malkodig = malkovr

deriving via Kovr Data.Bson.Symbol instance
         MongoKod Data.Bson.Symbol
instance MongoMalkod Data.Bson.Symbol where
  malkodig = malkovr

deriving via Kovr Javascript instance MongoKod Javascript
instance MongoMalkod Javascript where
  malkodig = malkovr

deriving via Kovr Regex instance MongoKod Regex
instance MongoMalkod Regex where
  malkodig = malkovr

deriving via Kovr UserDefined instance MongoKod UserDefined
instance MongoMalkod UserDefined where
  malkodig = malkovr

deriving via Kovr MD5 instance MongoKod MD5
instance MongoMalkod MD5 where
  malkodig = malkovr

deriving via Kovr UUID instance MongoKod UUID
instance MongoMalkod UUID where
  malkodig = malkovr

deriving via Kovr Function instance MongoKod Function
instance MongoMalkod Function where
  malkodig = malkovr

deriving via Kovr Binary instance MongoKod Binary
instance MongoMalkod Binary where
  malkodig = malkovr

deriving via Kovr Value instance MongoKod Value
instance MongoMalkod Value where
  malkodig = malkovr

deriving via Kovr Field instance MongoKod Field
instance MongoMalkod Field where
  malkodig = malkovr

instance MongoKod Void where
  kodig = absurd

instance MongoKod a => MongoKod [a] where
  kodig x = Array $ kodig <$> x
instance MongoMalkod a => MongoMalkod [a] where
  malkodig (Array x) = sequence $ malkodig @a <$> x
  malkodig _ = Nothing

instance MongoKod a => MongoKod (Maybe a) where
  kodig (Just x) = kodig x
  kodig Nothing = Null
instance MongoMalkod a => MongoMalkod (Maybe a) where
  malkodig Null = Just Nothing
  malkodig x = Just <$> malkodig @a x

instance (MongoKod a, MongoKod b) => MongoKod (a, b)
instance (MongoMalkod a, MongoMalkod b) => MongoMalkod (a, b)

instance (MongoKod a, MongoKod b, MongoKod c) => MongoKod (a, b, c)
instance (MongoMalkod a, MongoMalkod b, MongoMalkod c) => MongoMalkod (a, b, c)

instance (MongoKod a, MongoKod b, MongoKod c, MongoKod d) => MongoKod (a, b, c, d)
instance (MongoMalkod a, MongoMalkod b, MongoMalkod c, MongoMalkod d) => MongoMalkod (a, b, c, d)

instance (MongoKod a, MongoKod b, MongoKod c, MongoKod d, MongoKod e) => MongoKod (a, b, c, d, e)
instance (MongoMalkod a, MongoMalkod b, MongoMalkod c, MongoMalkod d, MongoMalkod e) => MongoMalkod (a, b, c, d, e)

instance (MongoKod a, MongoKod b, MongoKod c, MongoKod d, MongoKod e, MongoKod f) => MongoKod (a, b, c, d, e, f)
instance (MongoMalkod a, MongoMalkod b, MongoMalkod c, MongoMalkod d, MongoMalkod e, MongoMalkod f) => MongoMalkod (a, b, c, d, e, f)

kodigDok :: MongoKod a => a -> Document
kodigDok e = case kodig e of
  Doc x -> x
  x -> ["_" =: x]

malkodigDok :: MongoMalkod a => Document -> Maybe a
malkodigDok x
  | Just ("_" := y) <- L.find f x
    = malkodig y
  | otherwise
    = malkodig (Doc x)
  where
    f ("_" := _) = True
    f _ = False

data Preter a = Inkluziv !a | Preterlas

instance MongoKod a => MongoKod (Preter a) where
  kodig (Inkluziv x) = kodig x
  kodig Preterlas = Null

  kodigCxel nom (Inkluziv x) = kodigCxel nom x
  kodigCxel _ Preterlas = Nothing

type NeTrovitaCxelo = ()

instance MongoMalkod a => MongoMalkod (Preter a) where
  malkodig Null = Just Preterlas
  malkodig x = Inkluziv <$> malkodig x

  malkodigCxel nom =
    let oldmk = ?mk in
      let ?mk = \x -> ExceptT (Right <$> oldmk x) <|> throwError () in do
        v <- runExceptT $ malkodigCxel nom
        pure $ case v of
          Left () -> Preterlas
          Right x -> Inkluziv x