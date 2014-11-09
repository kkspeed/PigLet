{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Html.PigLet.HtmlMod where


import qualified Data.Set as S
import           Data.String.Utils (splitWs)
import           Data.List
import           Data.Monoid
import           Language.Haskell.TH

import           Util.BlazeFromHtml hiding (main)
import           Text.Html.PigLet.Html5Defs

pass :: ModNode
pass = ModNode False NotTouched

setContent :: ExpQ -> ModNode
setContent = ModNode True . SetContent

embedContent :: ExpQ -> ModNode
embedContent = ModNode True . EmbedContent

addAttr :: Attr -> ModNode
addAttr = addAttrs . (:[])

addAttrs :: Attrs -> ModNode
addAttrs = ModNode False . AddAttr

type Attr = (String, S.Set String)
type Attrs = [Attr]

data Modify = SetContent ExpQ
            | EmbedContent ExpQ
            | AddAttr Attrs
            | NotTouched

data ModNode = ModNode [Modify] Modify

instance Monoid ModNode where
    mappend (ModNode attrs1 NotTouched) (ModNode attrs2 m2) =
        ModNode (mergeAttrs attrs1 attrs2) m2
    mappend (ModNode False m1) (ModNode x m2)     = ModNode x (m1 ^@^ m2)
    mappend (ModNode x m1)     (ModNode False m2) = ModNode x (m2 ^@^ m1)
    mempty  = ModNode False NotTouched

data Selector = D String
              | A (String, [String])
                deriving (Show)

(>@<) :: Selector -> ModNode -> HtmlMod -> HtmlMod
(>@<) = attachModify

attachModify :: Selector -> ModNode -> HtmlMod -> HtmlMod
attachModify selector modn l@(HtmlLeaf tag attrs modn')
    | selected selector tag attrs = HtmlLeaf tag attrs (modn <> modn')
    | otherwise = l
attachModify selector modn (HtmlParent tag attrs child modn')
    | selected selector tag attrs =
        HtmlParent tag attrs (attachModify selector modn child) (modn <> modn')
    | otherwise = HtmlParent tag attrs (attachModify selector modn child) modn'
attachModify selector modn (HtmlBlock htmls) =
    HtmlBlock $ map (attachModify selector modn) htmls
attachModify _ _ t@(HtmlText _) = t

selected :: Selector -> String -> Attrs -> Bool
selected (D tag') tag _ = tag == tag'
selected (A (k, v)) _ attrs = maybe False (S.isSubsetOf (S.fromList v))
                                    (lookup k attrs)

data HtmlMod = HtmlLeaf String Attrs ModNode
             | HtmlParent String Attrs HtmlMod ModNode
             | HtmlBlock [HtmlMod]
             | HtmlText String

html2HtmlMod :: Html -> HtmlMod
html2HtmlMod (Parent tag attrs child)
    | isParent tag         =
        HtmlParent tag (makeAttrs attrs) (html2HtmlMod child) (ModNode False NotTouched)
    | isLeaf   tag         =
        HtmlLeaf tag (makeAttrs attrs) (ModNode False NotTouched)
    | otherwise            = error $ "Undefined tag: " ++ tag
html2HtmlMod (Text  t)     = HtmlText t
html2HtmlMod (Block htmls) = HtmlBlock $ map html2HtmlMod
                                       $ filter (not . isCommentDoctype) htmls
    where isCommentDoctype (Comment _) = True
          isCommentDoctype Doctype     = True
          isCommentDoctype _           = False
html2HtmlMod t             = error $ (show t) ++ " : Shouldn't happen at all"

makeAttrs :: Attributes -> Attrs
makeAttrs = map parseAttr
    where parseAttr (k, vs) = (k, S.fromList $ splitWs vs)
