module Data.Blog.Home where

import qualified Data.Text.Lazy.IO as TIO
import Data.Text (Text, pack, unpack)
import Data.Functor.Identity
import Org.Parser.Document
import System.Directory
import Data.Blog.Page
import Control.Monad
import Control.Lens
import Data.Monoid
import Text.Printf
import Lucid.Base
import Org.Parser
import Org.Types
import Data.List
import Lucid

data Home = Home
  { _homeTitle :: [OrgObject]
  , _homePages :: [Page]
  }
  deriving Show

makeLenses ''Home

toHome :: OrgDocument -> Maybe Home
toHome doc = do
  title <- getTitle (documentChildren doc)
  pure $ Home title (pages doc)
  where
    getTitle ((OrgElement _ (Keyword "title" (ParsedKeyword kw)):_)) = Just kw
    getTitle (_:xs)                                                  = getTitle xs
    getTitle []                                                      = Nothing

instance ToHtml Home where
  toHtmlRaw = toHtml

  toHtml h = do
    doctype_
    html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        meta_ [charset_ "utf-8"]

        title_ $ toHtml (h^.homeTitle)

        link_ [rel_ "stylesheet", href_ "/static/styles.css"]
        link_ [rel_ "icon", href_ "/static/favicon.ico"]

        script_ [src_ "/static/htmx.min.js"] ""
        script_ [src_ "/static/hyperscript.min.js"] ""

        meta_
          [ name_ "description"
          , content_ "silly website"
          ]

        meta_
          [ name_ "author"
          , content_ "Indy Paige"
          ]

      body_ $ do
        header_ $
          h1_ $ toHtml (h^.homeTitle)

        div_ [class_ "layout"] $ do
          main_ [class_ "content"] $
            section_ [class_ "box"] $
              ul_ [class_ "blog-links"] $
                foldl f mempty (h^.homePages)

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
      f last current = do
        last
        li_ $
          a_
            [ class_ "blog-link"
            , href_ uri
            ]
            name
        where
          name = toHtml (current^.pageName)
          uri  = "/blog/" <> current^.pageMeta.metaPath

writeHome :: Home -> IO ()
writeHome home = do
  createDirectoryIfMissing False "blog"
  createDirectoryIfMissing False "blog/posts"
  homePage >> other
  where
    homePage = do
      TIO.writeFile "./blog/index.html" $  renderText (toHtml home)

    other    = mapM_ f (home^.homePages)
    f a      = TIO.writeFile filePath' text
      where
        filePath' = dirPath <> ".html"
        dirPath   = "./blog/posts/" <> mp

        mp        = unpack $ a^.pageMeta.metaPath
        text      = renderText (toHtml a)
