{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Html.PigLet.Th
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

-- TODO: R
-- 1. All html5 tags
-- 2. Better selector

data Modify = SetContent ExpQ
            | EmbedContent ExpQ
            | AddAttr (Attribute String)
            | NotTouched

data HtmlMod = HtmlParent String Attributes HtmlMod Modify
             | HtmlBlock [HtmlMod]
             | HtmlText String
             | HtmlComment String
             | HtmlDoctype

data Selector = Dom String
              | Attr (Attribute String)
                deriving (Show)

makeTemplate :: FilePath -> [(HtmlMod -> HtmlMod)] -> ExpQ
makeTemplate file trans = runIO (readFile file) >>= transformHtml trans

transformHtml :: [(HtmlMod -> HtmlMod)] -> String -> ExpQ
transformHtml trans htmlStr = genCode $ foldr ($) hm trans
    where hm = html2HtmlMod $ htmlTree html5 htmlStr

pass :: HtmlMod -> HtmlMod
pass = id

setContent :: Selector -> ExpQ -> HtmlMod -> HtmlMod
setContent selector expr = attachModify selector (SetContent expr)

embedContent :: Selector -> ExpQ -> HtmlMod -> HtmlMod
embedContent selector expr = attachModify selector (EmbedContent expr)

addAttr :: Selector -> Attribute String -> HtmlMod -> HtmlMod
addAttr selector attr = attachModify selector (AddAttr attr)

(##) :: Selector -> Modify -> HtmlMod -> HtmlMod
selector ## modi = attachModify selector modi

selected :: Selector -> String -> Attributes -> Bool
selected (Dom tag') tag _ = tag == tag'
selected (Attr attr) _ attrs = elem attr attrs

attachModify :: Selector -> Modify -> HtmlMod -> HtmlMod
attachModify selector modi (HtmlParent tag attrs child modi') =
    if selected selector tag attrs
    then HtmlParent tag attrs (attachModify selector modi child) modi
    else HtmlParent tag attrs (attachModify selector modi child) modi'
attachModify _ _ (HtmlText t) = HtmlText t
attachModify selector modi (HtmlBlock htmls) =
    HtmlBlock $ map (attachModify selector modi) htmls
attachModify _ _ _ = error "blk Undefined"

html2HtmlMod :: Html -> HtmlMod
html2HtmlMod (Parent tag attrs child) =
    HtmlParent tag attrs (html2HtmlMod child) NotTouched
html2HtmlMod (Text t) = HtmlText t
html2HtmlMod (Block htmls) = HtmlBlock $ map html2HtmlMod htmls
html2HtmlMod _ = error "Cannot support doctype and comment"

htmlTree :: HtmlVariant -> String -> Html
htmlTree variant = removeEmptyText . fst . makeTree variant False [] .
                   parseTagsOptions parseOptions { optTagPosition = True }

genCode :: HtmlMod -> ExpQ
genCode (HtmlText str) = [| H.toHtml (str :: String) |]
genCode (HtmlParent tag attrs children NotTouched)
    | isParent tag = genParent tag attrs children Nothing
    | otherwise = genLeaf tag attrs Nothing
genCode (HtmlParent tag attrs children (AddAttr attr))
    | isParent tag = genParent tag attrs children (Just attr)
    | otherwise = genLeaf tag attrs (Just attr)
genCode (HtmlParent _ _ _ (SetContent expr)) = expr
genCode (HtmlParent tag attrs _ (EmbedContent expr)) =
    [| $(getHtmlParent tag) H.! $(genAttrs attrs) $ $expr |]
genCode (HtmlBlock htmls) = [| $(foldr genHtmls [| mempty |] htmls) |]
genCode _ = error $ "Undefined nodes"

genHtmls :: HtmlMod -> ExpQ -> ExpQ
genHtmls html code = [| $(genCode html) <> $code |]

genParent :: String -> Attributes -> HtmlMod -> Maybe (Attribute String) -> ExpQ
genParent tag attrs (HtmlBlock []) Nothing = [| $(getHtmlParent tag) H.!
                                                $(genAttrs attrs)
                                                $ mempty |]
genParent tag attrs child Nothing = [| $(getHtmlParent tag) H.! $(genAttrs attrs)
                                       $ $(genCode child) |]
genParent tag attrs (HtmlBlock []) (Just attr) =
    [| $(getHtmlParent tag) H.! makeAttrs (mergeAttr attr attrs) $ mempty |]
genParent tag attrs child (Just attr) =
    [| $(getHtmlParent tag) H.! makeAttrs (mergeAttr attr attrs) $
       $(genCode child) |]

genLeaf :: String -> Attributes -> Maybe (Attribute String) -> ExpQ
genLeaf tag attrs Nothing = [| $(getHtmlLeaf tag) H.! $(genAttrs attrs) |]
genLeaf tag attrs (Just attr) =
    [| $(getHtmlLeaf tag) H.! makeAttrs (mergeAttr attr attrs) |]

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
