module UzantMenu
  ( komp
  , Stat
  ) where

import Prelude (Unit, bind, const, pure, unit, ($), (&&), (<), (<#>), (<>), (==), (>=), (>=>), (>>>))

import DOM.HTML.Indexed.InputType (InputType(InputText))
import Data.Array (filter, length, all)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits as S
import Data.String.Common (split, toLower)
import Data.Tuple.Nested ((/\))
import Datum (Erar(..), HHTML, KlientErar(..), Tradukil, Eraril, fapl, fdevas, fen, fperm, petKern, setigi, skrKErar, striktAlfabet)
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (type (<>))
import Halogen.Hooks as HK
import Stil as Stil
import Web.UIEvent.KeyboardEvent as KeyboardEvent

data Stat
  = Aux { retposxt :: String, erar :: Maybe (Tradukil -> String) }
  | Sukc { celloko :: String }

type UzMenu
  = HK.UseState Stat <> HK.Pure

komp ∷ ∀ m. MonadAff m => Eraril -> Tradukil -> HHTML m UzMenu
komp eraril trd = HK.do
  stat /\ statId <- HK.useState $ Aux { retposxt: "", erar: Nothing }
  HK.pure
    $ HH.div
        [ HP.id "uzant-menu"
        ] case stat of
        Aux orstat@{ retposxt } ->
          let
            msendi = verigiAdr retposxt <#> \analizita -> do
              resp <- petKern (Left POST) "ensaluti" { retposxt: retposxt }
              case resp of
                Left (KlientErar erar)
                  | erar `elem` [ MalgxustaRetposxtErar, DomajnoNeEkzistasErar ] -> HK.put statId $ Aux $ orstat { erar = Just $ skrKErar erar }
                Left n -> eraril n
                Right (_ :: Array Unit) -> HK.put statId $ Sukc analizita
          in
            [ HH.text $ trd "aux.auxtentigxo"
            , HH.input
                [ HP.type_ InputText
                , HP.placeholder (trd "aux.retposxt")
                , fen
                    (fdevas (kalkDe '@' >>> (_ < 2)) >=> fapl toLower >=> fperm (striktAlfabet <> setigi "@"))
                    (\adr -> HK.put statId $ Aux $ orstat { retposxt = adr })
                , HE.onKeyDown
                    $ \event -> case KeyboardEvent.code event of
                      "Enter" | Just sendi <- msendi -> sendi
                      _ -> pure unit
                ]
            , HH.button
                (
                  case msendi of
                    Just sendi -> [ HE.onClick $ const sendi ]
                    Nothing -> [ HP.disabled true ]
                )
                [ HH.text $ trd "aux.ensalutu" ]
            ]
        Sukc { celloko } -> [
          HH.text $ trd "aux.sukces.1"
        , HH.br_
        , HH.text $ trd "aux.sukces.2"
        , HH.br_
        , Stil.sukcSign
        , HH.br_
        , HH.text $ trd "aux.vizitu"
        , HH.text " "
        , HH.a [ HP.href ("https://" <> celloko )] [ HH.text $ celloko ]
        ]
  where
  kalkDe liter = S.toCharArray >>> filter (_ == liter) >>> length

  verigiAdr adr
    | [ un, du ] <- split (Pattern "@") adr = 
        if S.length un >= 1 && kalkDe '.' du >= 1 && verigiDu du
          then Just $ { celloko: du }
          else Nothing
      where
      verigiDu = split (Pattern ".") >>> all (\domajnpart -> S.length domajnpart >= 2)

  verigiAdr _ = Nothing