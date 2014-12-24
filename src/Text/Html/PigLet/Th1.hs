{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Html.PigLet.Th1
    ( defTemplate
    , composeTemplate
    , genTemplate
    , addAttr
    , maybeAttr
    , maybeContent
    , maybeVal
    , var
    )
where

import           Text.HTML.TagSoup
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Language.Haskell.TH
import           Data.Monoid
import           Data.String.Utils (join)
import qualified Data.Set                    as S
import Control.Applicative

import GHC.Exts (IsString (..))

import Util.BlazeFromHtml           hiding (main)
import Util.GenerateHtmlCombinators hiding (main)

import Text.Html.PigLet.Html5Defs
import Text.Html.PigLet.HtmlMod

-- TODO: R
-- 1. All html5 tags
-- 2. Better selector

type Args = [String]

type HtmlTemplate = Q Template

data Template = Template Args HtmlMod

readTemplate :: [String] -> FilePath -> Q Template
readTemplate params file =  (Template params . makeHtmlMod) <$>
                            runIO (readFile file)

defTemplate :: [String] -> FilePath -> [HtmlMod -> HtmlMod]
            -> Q Template
defTemplate params file trans = transformTemplate trans <$>
                                readTemplate params file

composeTemplate :: Selector -> HtmlTemplate -> HtmlTemplate -> HtmlTemplate
composeTemplate selector = liftA2 (mergeTemplate selector)

defSnippet :: [String]
           -> FilePath
           -> HtmlTemplate
           -> Selector
           -> [HtmlMod -> HtmlMod]
           -> HtmlTemplate
defSnippet params file parent selector trans = composeTemplate selector parent
  $ transformTemplate trans <$> readTemplate params file

mergeTemplate :: Selector -> Template -> Template -> Template
mergeTemplate selector (Template pa pm) (Template ca cm) =
    Template (pa <> ca) (mergeTree selector cm pm)

transformTemplate :: [HtmlMod -> HtmlMod] -> Template -> Template
transformTemplate trans (Template args hm) =
    Template args (foldr1 (.) trans $ hm)

genTemplate :: HtmlTemplate -> ExpQ
genTemplate t = do
  Template args mods <- t
  lamE (map (varP . mkName) args) [| $(genCode mods) |]

var :: String -> ExpQ
var = varE . mkName

-- makeTemplate :: FilePath -> [String] -> [(HtmlMod -> HtmlMod)] -> ExpQ
-- makeTemplate file args trans = runIO (readFile file) >>=
--                                transformHtml trans args

-- transformHtml :: [(HtmlMod -> HtmlMod)] -> [String] -> String -> ExpQ
-- transformHtml trans args htmlStr =
--     lamE (map (varP . mkName) args) [| $(genCode $ foldr ($) hm trans) |]
--     where hm = html2HtmlMod $ htmlTree html5 htmlStr

makeHtmlMod :: String -> HtmlMod
makeHtmlMod = html2HtmlMod . htmlTree html5

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
    [| $(getHtmlParent tag) H.! genAttrs $attrMods attrTuples
       $ $expr |]
     where attrTuples = map (\ (k, vs) -> (k, S.toList vs)) attrs
genParent tag attrs child (ModNode attrMods NotTouched)                 =
    [| $(getHtmlParent tag) H.! genAttrs $attrMods attrTuples $
       $(genCode child) |]
    where attrTuples = map (\ (k, vs) -> (k, S.toList vs)) attrs

genLeaf   :: String -> Attrs -> ModNode -> ExpQ
genLeaf _ _ (ModNode _ (SetContent expr))                     = expr
genLeaf tag attrs (ModNode attrMods _)                        =
    [| $(getHtmlLeaf tag) H.! genAttrs $attrMods attrTuples |]
    where attrTuples = map (\ (k, vs) -> (k, S.toList vs)) attrs

type AttrT = (String, [String])
type AttrsT = [AttrT]

genAttrs :: (AttrsT -> AttrsT) -> AttrsT -> H.Attribute
genAttrs trans =
    mconcat . map (\(n, v) -> case lookup n html5Attr1 of
                               Nothing -> error $ "Looking up " ++ n
                               Just f -> f
                   $ fromString $ join " " v) . trans

addAttr :: (String, String) -> AttrsT -> AttrsT
addAttr (k, v) attrs = maybe ((k, [v]):attrs)
                       (\ vs -> if v `elem` vs
                         then (k, vs) : delKey k attrs
                         else (k, v:vs) : delKey k attrs)
                       (lookup k attrs)
    where delKey key kvs = filter (not . (== key) . fst) kvs

maybeAttr :: Maybe a -> (String, String) -> AttrsT -> AttrsT
maybeAttr m kv = maybe id (const $ addAttr kv) m

maybeVal :: (Show a) => Maybe a -> String -> AttrsT -> AttrsT
maybeVal v k = maybe id (\x -> addAttr (k, show x)) v

maybeContent :: (H.ToMarkup a) => Maybe a -> H.Html
maybeContent = maybe mempty H.toHtml
