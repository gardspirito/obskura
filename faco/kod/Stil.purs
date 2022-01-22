module Stil
  ( etaTekst
  , sukcSign
  )
  where

import Halogen.HTML (HTML)
import Halogen.Svg.Attributes (viewBox)
import Halogen.Svg.Elements (svg, path)
import Web.HTML.Common (ClassName(..), AttrName(..))
import Halogen.HTML.Properties (IProp, id, attr)

etaTekst :: ClassName
etaTekst = ClassName "eta-tekst"

d :: ∀ r i. String -> IProp r i
d = attr (AttrName "d")

sukcSign :: ∀ w i. HTML w i
sukcSign =
    svg [ viewBox 4.0 4.0 8.5 8.5, id "sukc-mark" ]
        [ path [ d "M10.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.267.267 0 0 1 .02-.022z" ]
        ]
