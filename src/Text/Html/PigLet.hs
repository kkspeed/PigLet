{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Html.PigLet
    ( makeTemplate
    , pass
    , setContent
    , embedContent
    , addAttrs
    , addAttr
    , (>@<)
    , Selector(..))
where

import Text.Html.PigLet.Th1 (makeTemplate)
import Text.Html.PigLet.HtmlMod
    (pass, setContent, addAttrs, addAttr, (>@<), embedContent, Selector(..))
