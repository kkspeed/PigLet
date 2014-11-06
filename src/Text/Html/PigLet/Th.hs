{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Html.PigLet.Th
    ( makeTemplate
    , setContent
    , embedContent 
    , Selector(..) )
where

import Util.BlazeFromHtml hiding (main)
import Util.GenerateHtmlCombinators hiding (main)
import Text.HTML.TagSoup
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Language.Haskell.TH
import Data.Monoid
import Data.Maybe (isJust)

-- TODO: R
-- 1. All html5 tags
-- 2. Better selector

data Modify = SetContent ExpQ
            | EmbedContent ExpQ
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

setContent :: Selector -> ExpQ -> HtmlMod -> HtmlMod
setContent selector expr = attachModify selector (SetContent expr)

embedContent :: Selector -> ExpQ -> HtmlMod -> HtmlMod
embedContent selector expr = attachModify selector (EmbedContent expr)

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
    | isParent tag = genParent tag attrs children
    | otherwise = genLeaf tag attrs
genCode (HtmlParent _ _ _ (SetContent expr)) = expr
genCode (HtmlParent tag attrs _ (EmbedContent expr)) =
    [| $(getHtmlParent tag) H.! $(genAttrs attrs) $ $expr |]
genCode (HtmlBlock htmls) = [| $(foldr genHtmls [| mempty |] htmls) |]
genCode _ = error $ "Undefined nodes"

genHtmls :: HtmlMod -> ExpQ -> ExpQ
genHtmls html code = [| $(genCode html) <> $code |]

genParent :: String -> Attributes -> HtmlMod -> ExpQ
genParent tag attrs (HtmlBlock []) = [| $(getHtmlParent tag) H.!
                                        $(genAttrs attrs)
                                        $ mempty |]
genParent tag attrs child = [| $(getHtmlParent tag) H.! $(genAttrs attrs)
                               $ $(genCode child) |]

genLeaf :: String -> Attributes -> ExpQ
genLeaf tag attrs = [| $(getHtmlLeaf tag) H.! $(genAttrs attrs) |]

genAttrs :: Attributes -> ExpQ
genAttrs  = foldr genAttr [| mempty |]
    where genAttr (attr, val) code = [| $(getHtmlAttr attr) val <> $code |]

type Parent = H.Html -> H.Html
type Leaf = H.Html

getHtmlParent :: String -> ExpQ
getHtmlParent tag = maybe (error $ "Invalid parent tag " ++ tag)
                          id
                          (lookup tag html5Parent)

getHtmlLeaf :: String -> ExpQ
getHtmlLeaf tag = maybe (error $ "Invalid leaf tag " ++ tag)
                        id
                        (lookup tag html5Leaf)

getHtmlAttr :: String -> ExpQ
getHtmlAttr attr = maybe (error $ "Invalid attr " ++ attr)
                         id
                         (lookup attr html5Attr)

isParent :: String -> Bool
isParent = isJust . flip lookup html5Parent

html5Parent :: [(String, ExpQ)]
html5Parent = [ ("html"       , [| H.docTypeHtml |])
              , ("head"       , [| H.head |])
              , ("title"      , [| H.title |])
              , ("p"          , [| H.p |])
              , ("div"        , [| H.div |])
              , ("a"          , [| H.a |])
              , ("abbr"       , [| H.abbr |])
              , ("address"    , [| H.address |])
              , ("article"    , [| H.article |])
              , ("aside"      , [| H.aside |])
              , ("audio"      , [| H.audio |])
              , ("b"          , [| H.b |])
              , ("base"       , [| H.base |])
              , ("bdo"        , [| H.bdo |])
              , ("blockquote" , [| H.blockquote |])
              , ("body"       , [| H.body |])
              , ("button"     , [| H.button |])
              , ("canvas"     , [| H.canvas |])
              , ("caption"    , [| H.caption |])
              , ("cite"       , [| H.cite |])
              , ("code"       , [| H.code |])
              , ("colgroup"   , [| H.colgroup |])
              , ("command"    , [| H.command |])
              , ("datalist"   , [| H.datalist |])
              , ("dd"         , [| H.dd |])
              , ("script"     , [| H.script |])
              , ("nav"        , [| H.nav |])
              , ("form"       , [| H.form |])
              , ("label"      , [| H.label |])
              ]

html5Leaf :: [(String , ExpQ)]
html5Leaf = [ ("area" , [| H.area |])
            , ("br"   , [| H.br  |])
            , ("col"  , [| H.col |])
            , ("embed", [| H.embed |])
            , ("hr"   , [| H.hr |])
            , ("img"  , [| H.img |])
            , ("input", [| H.input |])
            , ("meta" , [| H.meta |])
            , ("link" , [| H.link |])
            , ("param", [| H.param |]) ]

html5Attr :: [(String, ExpQ)]
html5Attr = [ ("class"       , [| HA.class_ |])
            , ("cite"        , [| HA.cite |])
            , ("src"         , [| HA.src |])
            , ("id"          , [| HA.id |])
            , ("rel"         , [| HA.rel |])
            , ("href"        , [| HA.href |])
            , ("type"        , [| HA.type_ |])
            , ("for"         , [| HA.for |])
            , ("placeholder" , [| HA.placeholder |])
            ]
