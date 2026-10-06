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
  toHtml h  = div_ $ do
    header_ $ h1_ [class_ "blog-title"] title

    main_ [class_ "blog-main"] $ do
      ul_ [class_ "blog-links"] $ foldl f mempty (h^.homePages)
    where
      title          = toHtml (h^.homeTitle)

      f last current = do
        last
        li_ $ do
          a_ [class_ "blog-link", href_ uri] name
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
