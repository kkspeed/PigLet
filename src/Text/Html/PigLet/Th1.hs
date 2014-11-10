{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Html.PigLet.Th1
    ( makeTemplate
    , setContent
    , embedContent
    , addAttr
    , pass
    , Selector (..))
where

import           Text.HTML.TagSoup
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Language.Haskell.TH
import           Data.Monoid
import           Data.Maybe (fromJust)
import           Data.String.Utils (join)
import qualified Data.Set                    as S

import GHC.Exts (IsString (..))

import Util.BlazeFromHtml           hiding (main)
import Util.GenerateHtmlCombinators hiding (main)

import Text.Html.PigLet.Html5Defs
import Text.Html.PigLet.HtmlMod

-- TODO: R
-- 1. All html5 tags
-- 2. Better selector

makeTemplate :: FilePath -> [(HtmlMod -> HtmlMod)] -> ExpQ
makeTemplate file trans = runIO (readFile file) >>= transformHtml trans

transformHtml :: [(HtmlMod -> HtmlMod)] -> String -> ExpQ
transformHtml trans htmlStr = genCode $ foldr ($) hm trans
    where hm = html2HtmlMod $ htmlTree html5 htmlStr

htmlTree :: HtmlVariant -> String -> Html
htmlTree variant = removeEmptyText . fst . makeTree variant False [] .
                   parseTagsOptions parseOptions { optTagPosition = True }

genCode :: HtmlMod -> ExpQ
genCode (HtmlText str) = [| H.toHtml (str :: String) |]
genCode (HtmlParent tag attrs children modn) =
    genParent tag attrs children modn
genCode (HtmlLeaf tag attrs modn)            =
    genLeaf tag attrs modn
genCode (HtmlBlock htmls)                    =
    [| $(foldr genHtmls [| mempty |] htmls) |]

genHtmls :: HtmlMod -> ExpQ -> ExpQ
genHtmls html code = [| $(genCode html) <> $code |]

genParent :: String -> Attrs -> HtmlMod -> ModNode -> ExpQ
genParent _ _ _ (ModNode _ (SetContent expr))                           = expr
genParent tag attrs _ (ModNode attrMods (EmbedContent expr))            =
    [| $(getHtmlParent tag) H.! $(genAttrMods attrMods attrs) $ $expr |]
genParent tag attrs child (ModNode attrMods NotTouched)                 =
    [| $(getHtmlParent tag) H.! $(genAttrMods attrMods attrs) $ $(genCode child) |]

genLeaf   :: String -> Attrs -> ModNode -> ExpQ
genLeaf _ _ (ModNode _ (SetContent expr))                     = expr
genLeaf tag attrs (ModNode attrMods _)                        =
    [| $(getHtmlLeaf tag) H.! $(genAttrMods attrMods attrs) |]

genAttrs :: Attrs -> ExpQ
genAttrs  = foldr genAttr [| mempty |]
    where genAttr (k, vals) code = let attrVal = join " " $ S.toList vals
                                   in [| $(getHtmlAttr k) attrVal <> $code |]

genAttrMods :: AttrModify -> Attrs -> ExpQ
genAttrMods NoAttr           attrs = genAttrs attrs
genAttrMods (AddAttr aattrs) attrs = genAttrs (mergeAttr aattrs attrs)

makeAttrs :: Attributes -> H.Attribute
makeAttrs = mconcat .
            map (\(n, v) -> fromJust (lookup n html5Attr1) $ fromString v)
