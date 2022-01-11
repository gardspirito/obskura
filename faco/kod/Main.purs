module Main
  ( erarList
  , main
  ) where

import Data.Array as Arr
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map as HM
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Datum (Erar(..), Tradukenda(..), Tradukil, petKern, priskribiKlientErar, priskribiServilErar)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object (toArrayWithKey)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (type (<>), fork, useLifecycleEffect)
import Halogen.Hooks as HK
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, discard, map, pure, unit, ($), (+), (<#>), (<>), (>=))
import Safe.Coerce (coerce)
import UzantMenu as UzMenu

main :: Effect Unit
main =
  HA.runHalogenAff do
    pagx <- HA.awaitBody
    runUI komp unit pagx

komp :: ∀ p en el m. MonadAff m => H.Component p en el m
komp =
  HK.component \_ _ -> HK.do
    linMap /\ linId <- HK.useState HM.empty
    let trd = (\p -> fromMaybe "[...]" (HM.lookup (coerce p) linMap)) :: Tradukil
    erarilo /\ erarH <- erarList trd
    useLifecycleEffect do
      respond <- petKern (Left GET) "lingvar" unit
      case respond of
        Left erar -> erarilo erar
        Right nlin -> HK.put linId $ HM.fromFoldable (toArrayWithKey Tuple nlin)
      pure Nothing

    menuH <- UzMenu.komp trd
    HK.pure
      $ HH.div
          [ HP.id "supr"
          ]
          [ HH.div
              [ HP.id "uzant"
              ]
              [ menuH
              ]
          , erarH
          ]

type UzErarList
  = HK.UseState (M.Map Int ErarElem) <> HK.UseState Int <> HK.Pure

type ErarElem
  = { elem :: Erar, morto :: Ref.Ref (Maybe Int) }

erarList :: ∀ m w. MonadAff m => Tradukil -> HK.Hook m UzErarList ((Erar -> HK.HookM m Unit) /\ (HH.HTML w (HK.HookM m Unit)))
erarList trd = HK.do
  stat /\ statId <- HK.useState M.empty
  _ /\ nombril <- HK.useState 0
  HK.pure
    $ ( \erar -> do
          nombro <- HK.modify nombril (_ + 1)
          ref <- liftEffect $ Ref.new (Just 0)
          HK.modify_ statId (M.insert nombro { elem: erar, morto: ref })
          _ <- fork do
            liftAff $ atendMorton ref
            HK.modify_ statId (M.delete nombro)
          pure unit
      )
    /\ HH.div
        [ HP.id "erar-list" ]
        (Arr.fromFoldable $ M.values stat <#> \erarEl ->
          let titol /\ info = tekstigi erarEl.elem in
            HH.div (revivigiMsg erarEl.morto) $ [
              HH.h1_ [HH.text titol],
              HH.hr_
            ] <> info
        )
  where
  tekstigi :: ∀ i. Erar -> String /\ Array (HH.HTML w i)
  tekstigi = case _ of
    ServilErar erar -> trd "erar.servil" /\ priskribiServilErar trd erar
    KlientErar erar -> trd "erar.klient" /\ [HH.text $ priskribiKlientErar trd erar]
  atendMorton ref = cikl
    where
    morttempo = 5

    cikl = do
      delay (Milliseconds 1000.0)
      val <- liftEffect $ Ref.modify (map (_ + 1)) ref
      if fromMaybe 0 val >= morttempo then
        pure unit
      else
        cikl
  revivigiMsg ref = [
      HE.onMouseEnter \_ -> liftEffect $ Ref.write Nothing ref,
      HE.onMouseLeave \_ -> liftEffect $ Ref.write (Just 0) ref
    ]
