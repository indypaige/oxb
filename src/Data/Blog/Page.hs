module Data.Blog.Page where

import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Functor.Identity
import Org.Parser.Objects
import Control.Monad
import Control.Lens
import Data.Monoid
import Lucid.Base
import Org.Parser
import Data.Maybe
import Org.Types
import Data.List
import Data.Blog
import Lucid

data Page = Page
  { _pageName :: [OrgObject]
  , _pageSect :: [OrgSection]
  , _pageBody :: [OrgElement]
  , _pageMeta :: Meta
  }
  deriving Show

data Meta = Meta
  { _metaAuthor :: Text
  , _metaDate   :: TimestampData
  , _metaPath   :: Text

  }
  deriving Show

makeLenses ''Page
makeLenses ''Meta

collectMeta :: Properties -> Maybe Meta
collectMeta props = do
  author <- Map.lookup "author" props
  date   <- Map.lookup "date"   props
  path   <- Map.lookup "path"   props

  date'  <- parseOrgMaybe
    defaultOrgOptions
    parseTimestamp
    date
 
  pure $ Meta author date' path

toPage :: OrgSection -> Maybe Page
toPage s = do
  meta <- collectMeta (sectionProperties s)
  pure $ Page (sectionTitle s) (sectionSubsections s) (sectionChildren s) meta

instance ToHtml Meta where
  toHtmlRaw           = toHtml
  toHtml (Meta a d _) = div_ [class_ "post-meta"] $ do
    span_ (toHtml a)
    br_ []
    span_ (toHtml d)

instance ToHtml Page where
  toHtmlRaw = toHtml

  toHtml p = do
    doctype_
    html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

        meta_ [charset_ "utf-8"]

        title_ pageName'

        link_ [rel_ "stylesheet", href_ "/static/styles.css"]
        link_ [rel_ "icon" , href_ "/static/favicon.ico"]

        script_ [src_ "/static/htmx.min.js"] ""
        script_ [src_ "/static/hyperscript.min.js"] ""

        meta_ [name_ "description", content_ "silly website"]

        meta_ [name_ "author", content_ "Indy Paige"]

      body_ $ do
        header_ [class_ "post-header"] $
          h1_ pageName'

        div_ [class_ "layout"] $ do
          main_ [class_ "content"] $
            article_ [class_ "box post-body"] $ do
              toHtml $ p^.pageMeta
              toHtml $ p^.pageBody
              toHtml $ p^.pageSect

          aside_
            [ class_ "sidebar"
            , makeAttribute "hx-get" "/hx/links"
            , makeAttribute "hx-trigger" "load"
            ]
            mempty

        div_
          [ makeAttribute "hx-get" "/hx/footer"
          , makeAttribute "hx-swap" "outerHTML"
          , makeAttribute "hx-trigger" "load"
          ]
          mempty

    where
      pageName' = toHtml (p^.pageName)

pages :: OrgDocument -> [Page]
pages doc = catMaybes $ map toPage (getSections doc)

getSections :: OrgDocument -> [OrgSection]
getSections = f . documentSections
  where
    f (x:xs) | "blog" `elem` sectionTags x = x:(f xs)
    f (x:xs)                               = f (sectionSubsections x ++ xs)
    f []                                   = []

