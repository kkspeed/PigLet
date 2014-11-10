{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Html.PigLet.HtmlMod where

import qualified Data.Set as S
import           Data.String.Utils (splitWs)
import           Data.Monoid
import           Language.Haskell.TH

import           Util.BlazeFromHtml hiding (main)
import           Text.Html.PigLet.Html5Defs

pass :: ModNode
pass = ModNode NoAttr NotTouched

setContent :: ExpQ -> ModNode
setContent = ModNode NoAttr . SetContent

embedContent :: ExpQ -> ModNode
embedContent = ModNode (AddAttr []) . EmbedContent

addAttr :: (String, [String]) -> ModNode
addAttr (k, v) = addAttrs [(k, S.fromList v)]

addAttrs :: Attrs -> ModNode
addAttrs attrs = ModNode (AddAttr attrs) NotTouched

type Attr = (String, S.Set String)
type Attrs = [Attr]

data Modify = SetContent ExpQ
            | EmbedContent ExpQ
            | NotTouched

instance Monoid Modify where
    mappend a          NotTouched = a
    mappend NotTouched b          = b
    mappend a          _          = a
    mempty = NotTouched

data AttrModify = AddAttr Attrs
                | NoAttr

instance Monoid AttrModify where
    mappend (AddAttr attr1) (AddAttr attr2) = AddAttr (mergeAttr attr1 attr2)
    mappend NoAttr a2 = a2
    mappend a1 NoAttr = a1
    mempty = NoAttr

-- TODO: Use Map to do this
mergeAttr :: Attrs -> Attrs -> Attrs
mergeAttr attrs1 = foldr insertAttr attrs1
    where insertAttr (k, vs) attrs = maybe ((k, vs) : delKey k attrs)
                                     (\v -> (k, S.union v vs) : delKey k attrs)
                                     (lookup k attrs)
          delKey k kvs = filter (not . (== k) . fst) kvs

data ModNode = ModNode AttrModify Modify

instance Monoid ModNode where
    mappend (ModNode mattrs1 m1) (ModNode mattrs2 m2) =
        ModNode (mattrs1 <> mattrs2) (m1 <> m2)
    mempty  = ModNode NoAttr NotTouched

data Selector = D String
              | A (String, [String])
                deriving (Show)

(>@<) :: Selector -> ModNode -> HtmlMod -> HtmlMod
(>@<) = attachModify

infix 5 >@<

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
        HtmlParent tag (makeAttrs attrs) (html2HtmlMod child) pass
    | isLeaf   tag         =
        HtmlLeaf tag (makeAttrs attrs) pass
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
