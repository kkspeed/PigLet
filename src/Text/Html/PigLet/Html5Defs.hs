{-# LANGUAGE TemplateHaskell #-}

module Text.Html.PigLet.Html5Defs where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Language.Haskell.TH
import           Data.Maybe (isJust)

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

isLeaf   :: String -> Bool
isLeaf   = isJust . flip lookup html5Leaf

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
            , ("action"      , [| HA.action |])
            , ("method"      , [| HA.method |])
            , ("name"        , [| HA.name |])
            ]

html5Attr1 :: [(String, H.AttributeValue -> H.Attribute)]
html5Attr1 = [ ("class"       ,  HA.class_ )
             , ("cite"        ,  HA.cite )
             , ("src"         ,  HA.src )
             , ("id"          ,  HA.id )
             , ("rel"         ,  HA.rel )
             , ("href"        ,  HA.href )
             , ("type"        ,  HA.type_ )
             , ("for"         ,  HA.for )
             , ("placeholder" ,  HA.placeholder )
             , ("method"      ,  HA.method)
             , ("action"      ,  HA.action)
             , ("name"        ,  HA.name)
            ]
