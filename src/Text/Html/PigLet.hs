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
    , maybeContent
    , maybeVal
    , (>@<)
    , Selector(..))
where

import Text.Html.PigLet.Th1 ( defTemplate, composeTemplate, genTemplate, var
                            , addAttr, maybeAttr, maybeContent, maybeVal)
import Text.Html.PigLet.HtmlMod
    (pass, setContent, (>@<), embedContent, Selector(..), updateAttr)
