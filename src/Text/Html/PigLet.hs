{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Html.PigLet
    ( defTemplate
    , composeTemplate
    , genTemplate
    , var
    , pass
    , setContent
    , embedContent
    , updateAttr
    , addAttr
    , maybeAttr
    , (>@<)
    , Selector(..))
where

import Text.Html.PigLet.Th1 ( defTemplate, composeTemplate, genTemplate, var
                            , addAttr, maybeAttr)
import Text.Html.PigLet.HtmlMod
    (pass, setContent, (>@<), embedContent, Selector(..), updateAttr)
