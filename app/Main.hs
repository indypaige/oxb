module Main where

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
import Data.Blog.Home
import Lucid

main :: IO ()
main = do
  str <- TIO.readFile name
  let doc = parseOrgDoc defaultOrgOptions name str
  case toHome doc of
    Nothing -> print "failed to make home page"
    Just x  -> writeHome x
  where
    name = "blog.org"
