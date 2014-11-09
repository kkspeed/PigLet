{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Html.PigLet.HtmlMod where

import Util.BlazeFromHtml   hiding (main)
import Data.String.Utils           (splitWs)
import Language.Haskell.TH

import Data.Monoid
import Text.Html.PigLet.Html5Defs

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

type Attr = (String, [String])
type Attrs = [Attr]

data Modify = SetContent ExpQ
            | EmbedContent ExpQ
            | AddAttr Attrs
            | Comp Modify Modify
            | NotTouched

(^@^) :: Modify -> Modify -> Modify
NotTouched ^@^ n2         = n2
n1         ^@^ NotTouched = n1
n1         ^@^ n2         = Comp n1 n2



data ModNode = ModNode Bool Modify

instance Monoid ModNode where
    mappend (ModNode True _)   (ModNode True m2)  = ModNode True m2
    mappend (ModNode False m1) (ModNode x m2)     = ModNode x (m1 ^@^ m2)
    mappend (ModNode x m1)     (ModNode False m2) = ModNode x (m1 ^@^ m2)
    mempty  = ModNode False NotTouched

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
    where parseAttr (k, vs) = (k, splitWs vs)
