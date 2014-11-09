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

import Util.BlazeFromHtml hiding (main)
import Util.GenerateHtmlCombinators hiding (main)
import Text.HTML.TagSoup
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Language.Haskell.TH
import Data.Monoid
import Data.Maybe (fromJust)
import GHC.Exts (IsString (..))

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

-- genCode (HtmlParent _ _ _ (SetContent expr)) = expr
-- genCode (HtmlParent tag attrs _ (EmbedContent expr)) =
--    [| $(getHtmlParent tag) H.! $(genAttrs attrs) $ $expr |]

-- genCode _ = error $ "Undefined nodes"

genParent :: String -> Attrs -> HtmlMod -> ModNode -> ExpQ
genParent = undefined

genLeaf   :: String -> Attrs -> ModNode -> ExpQ
genLeaf   = undefined

-- genParent :: String -> Attributes -> HtmlMod -> Maybe (Attribute String) -> ExpQ
-- genParent tag attrs (HtmlBlock []) Nothing = [| $(getHtmlParent tag) H.!
--                                                 $(genAttrs attrs)
--                                                 $ mempty |]
-- genParent tag attrs child Nothing = [| $(getHtmlParent tag) H.! $(genAttrs attrs)
--                                        $ $(genCode child) |]
-- genParent tag attrs (HtmlBlock []) (Just attr) =
--     [| $(getHtmlParent tag) H.! makeAttrs (mergeAttr attr attrs) $ mempty |]
-- genParent tag attrs child (Just attr) =
--     [| $(getHtmlParent tag) H.! makeAttrs (mergeAttr attr attrs) $
--        $(genCode child) |]

-- genLeaf :: String -> Attributes -> Maybe (Attribute String) -> ExpQ
-- genLeaf tag attrs Nothing = [| $(getHtmlLeaf tag) H.! $(genAttrs attrs) |]
-- genLeaf tag attrs (Just attr) =
--     [| $(getHtmlLeaf tag) H.! makeAttrs (mergeAttr attr attrs) |]

makeAttrs :: Attributes -> H.Attribute
makeAttrs = mconcat .
            map (\(n, v) -> fromJust (lookup n html5Attr1) $ fromString v)

mergeAttr :: Attribute String -> Attributes -> Attributes
mergeAttr (name, value) attrs =
    case lookup name attrs of
      Just _  -> map (\(n, v) -> if n == name
                                 then (n, value ++ " " ++ v)
                                 else (n, v)) attrs
      Nothing -> (name, value) : attrs

genAttrs :: Attributes -> ExpQ
genAttrs  = foldr genAttr [| mempty |]
    where genAttr (attr, val) code = [| $(getHtmlAttr attr) val <> $code |]
