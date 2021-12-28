module Datum
  ( Lingvo
  )
  where

import Prelude (type (~>), (<#>), (>>>))
import Data.HashMap (HashMap, lookup)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.HalogenQ (HalogenQ(..))
import Halogen.Query.HalogenM (HalogenM(..), HalogenF(..), HalogenAp(..))
import Control.Applicative.Free (hoistFreeAp)
import Data.Newtype (over)
import Data.Maybe (fromMaybe)
import Control.Monad.Free (hoistFree)

type Lingvo
  = HashMap String String

{-
traduk :: String -> Lingvo -> String
traduk p = lookup p >>> fromMaybe "[...]"

htraduk :: ∀ w i. String -> Lingvo -> HH.HTML w i
htraduk p = traduk p >>> HH.text

type LinKomp p en
  = H.Component p ({ lin :: Lingvo, ene :: en })

type KrudaKomp stat p ago fakoj en el m
  = { eval :: (HalogenQ p ago en) ~> (HalogenM stat ago fakoj el m)
    , initialState :: en -> stat
    , render :: Lingvo -> stat -> H.ComponentHTML ago fakoj m
    }

mkLinKomp :: ∀ stat p ago fakoj en el m. KrudaKomp stat p ago fakoj en el m -> LinKomp p en el m
mkLinKomp kruda =
  H.mkComponent
    { render: \stat -> kruda.render stat.lin stat.ene
    , initialState: \en -> { ene: kruda.initialState en.ene, lin: en.lin }
    , eval:
        \p ->
          etendStat
            ( case p of
                Receive { ene } r -> kruda.eval (Receive ene r)
                Initialize r -> kruda.eval (Initialize r)
                Finalize r -> kruda.eval (Finalize r)
                Action ago r -> kruda.eval (Action ago r)
                Query a b -> kruda.eval (Query a b)
            )
    }

-- https://github.com/purescript-halogen/purescript-halogen/blob/v6.1.2/src/Halogen/Query/HalogenM.purs#L229-L234
etendStat ::
  ∀ enaStat restStat ago fakoj el m.
  HalogenM enaStat ago fakoj el m
    ~> HalogenM { ene :: enaStat | restStat } ago fakoj el m
etendStat (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: H.HalogenF enaStat ago fakoj el m ~> H.HalogenF { ene :: enaStat | restStat } ago fakoj el m
  go = case _ of
    State fs -> State (\stat -> fs stat.ene <#> (\r -> stat { ene = r }))
    Subscribe fes k -> Subscribe fes k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift q -> Lift q
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp etendStat) p)
    Fork hmu k -> Fork (etendStat hmu) k
    Kill fid a -> Kill fid a
    GetRef p k -> GetRef p k
-}