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
    , (>@<)
    , Selector(..))
where

import Text.Html.PigLet.Th1 ( defTemplate, composeTemplate, genTemplate, var
                            , addAttr)
import Text.Html.PigLet.HtmlMod
    (pass, setContent, (>@<), embedContent, Selector(..), updateAttr)
