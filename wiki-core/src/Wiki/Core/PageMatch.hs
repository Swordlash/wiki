{-# LANGUAGE DeriveAnyClass #-}

-- | 'PageMatch' is how the letters typed for a page are matched against
-- the page's title and path: the rule of the editor's list after @[[@
-- ("Wiki.App.Editor.Complete"'s 'Wiki.App.Editor.Complete.pageSource'),
-- and of the lookup of pages by title in every space the reader may view
-- ('TitleQuery', 'rankTitles'), which the list of another space's pages
-- will be filled from (issue #301). One rule for both, so that a page is
-- offered for the same letters wherever it is.
--
-- The letters and the title are compared in lower case ('T.toLower'),
-- and the path's segments with the letters made a slug ('slugify'). A
-- page matches in one of three ranks ('matchRank'): its title starts
-- with the letters; a later word of its title, or a segment of its path,
-- starts with them; its title or its path holds them anywhere else.
--
-- The lookup runs on the server, and in the demo's backend in the
-- browser. Both rank and cut the pages here, in Haskell, so that every
-- backend gives the same pages in the same order whatever its database
-- would compare.
module Wiki.Core.PageMatch
  ( -- * Matching
    Letters (..)
  , typedLetters
  , Matchable (..)
  , matchable
  , matchRank

    -- * The lookup of pages by title
  , TitleQuery (..)
  , titleQuery
  , titleQueryOf
  , TitleHit (..)
  , rankTitles
  , titleRefusal
  , looksFor
  , titleLimit
  , minLetters
  , maxLetters
  , defaultTitleHits
  , maxTitleHits
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Protolude
import Wiki.Core.Id (PageId (..), SpaceId)
import Wiki.Core.Slug (Slug, slugText, slugify)

--------------------------------------------------------------------------------
-- Matching

-- | 'Letters' are the letters typed for a page, as 'matchRank' compares
-- them: without spaces at either end and in lower case ('lower'), and
-- that made a slug ('slug'), or the same where no slug can be made of it.
data Letters = Letters
  { lower :: Text
  , slug :: Text
  }
  deriving stock (Eq, Show)

-- | The letters typed, as 'matchRank' compares them.
typedLetters :: Text -> Letters
typedLetters typed = Letters {lower, slug = maybe lower slugText (slugify lower)}
  where
    lower = T.toLower (T.strip typed)

-- | 'Matchable' is a page as 'matchRank' compares letters with it: its
-- title in lower case, the words of that after the first, its path's
-- segments, and those joined with @/@. The editor keeps these for each
-- page of the space when the pages change, not at each letter typed.
data Matchable = Matchable
  { lowerTitle :: Text
  , titleWords :: [Text]
  , segments :: [Text]
  , joined :: Text
  }
  deriving stock (Eq, Show)

-- | A page as 'matchRank' compares letters with it, from its title and its
-- path under its space's home page.
matchable :: Text -> [Slug] -> Matchable
matchable title path =
  Matchable
    { lowerTitle
    , titleWords = drop 1 (T.split (not . isAlphaNum) lowerTitle)
    , segments
    , joined = T.intercalate "/" segments
    }
  where
    lowerTitle = T.toLower title
    segments = map slugText path

-- | How well a page matches the letters, the best first: 1 when its title
-- starts with them (every page matches so when no letters are typed); 2
-- when a later word of the title starts with them, or a segment of the
-- path starts with them made a slug (@on c@ finds @on-call@); 3 when the
-- title or the path holds them anywhere else. 'Nothing' when it holds
-- them nowhere.
matchRank :: Letters -> Matchable -> Maybe Int
matchRank q p
  | T.null q.lower || q.lower `T.isPrefixOf` p.lowerTitle = Just 1
  | any (q.lower `T.isPrefixOf`) p.titleWords = Just 2
  | any (q.slug `T.isPrefixOf`) p.segments = Just 2
  | q.lower `T.isInfixOf` p.lowerTitle || q.slug `T.isInfixOf` p.joined = Just 3
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- The lookup of pages by title

-- | 'TitleQuery' asks for the pages whose title or path match some
-- letters ('matchRank'), among the pages of the spaces the reader may
-- view that the search reads ('Wiki.Core.Search.searchedChain': not an
-- encrypted space, not the front page's), templates left out.
data TitleQuery = TitleQuery
  { letters :: Text
  -- ^ The letters typed. Fewer than 'minLetters' find nothing; more than
  -- 'maxLetters' are refused ('titleRefusal').
  , except :: Maybe SpaceId
  -- ^ A space whose pages are left out: the space of the page being
  -- edited, whose pages the editor holds already. Its subspaces are
  -- other spaces, and are not left out.
  , limit :: Int
  -- ^ At most this many pages, the best first, and never more than
  -- 'maxTitleHits' ('titleLimit').
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A query for these letters in every space, with 'defaultTitleHits'
-- pages at most.
titleQuery :: Text -> TitleQuery
titleQuery typed = TitleQuery {letters = typed, except = Nothing, limit = defaultTitleHits}

-- | The query the parameters of the API's address ask for (@q@, @except@,
-- @limit@): no letters when @q@ is missing, and 'defaultTitleHits' pages
-- when @limit@ is.
titleQueryOf :: Maybe Text -> Maybe SpaceId -> Maybe Int -> TitleQuery
titleQueryOf q except limit = TitleQuery {letters = fromMaybe "" q, except, limit = fromMaybe defaultTitleHits limit}

-- | 'TitleHit' is a page the lookup found: its id, its space, the path of
-- its space from the top level, its path under its space's home page (the
-- home page's is empty), and its title.
data TitleHit = TitleHit
  { page :: PageId
  , space :: SpaceId
  , spacePath :: [Slug]
  , pagePath :: [Slug]
  , title :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The fewest letters a lookup looks for, once without spaces at either
-- end and in lower case: one letter would match most of a wiki.
minLetters :: Int
minLetters = 2

-- | The most letters a lookup takes, as typed: longer than any title a
-- reader types the start of. A longer query is refused ('titleRefusal'),
-- so that no request makes the server compare a long text with every
-- title.
maxLetters :: Int
maxLetters = 200

-- | How many pages a lookup gives when it does not say.
defaultTitleHits :: Int
defaultTitleHits = 10

-- | The most pages a lookup gives, whatever it asks for.
maxTitleHits :: Int
maxTitleHits = 20

-- | The number of pages a lookup gives at most: what it asks for, from 0
-- to 'maxTitleHits'.
titleLimit :: TitleQuery -> Int
titleLimit q = max 0 (min maxTitleHits q.limit)

-- | Why a lookup is refused, if it is: letters longer than 'maxLetters'.
-- Every backend refuses it so, with this reason, before it reads a page.
titleRefusal :: TitleQuery -> Maybe Text
titleRefusal q
  | T.length q.letters > maxLetters = Just ("A page's title is looked up by " <> show maxLetters <> " characters at most.")
  | otherwise = Nothing

-- | Whether a lookup looks for anything: at least 'minLetters' letters
-- once compared ('typedLetters'), and a limit above 0. One that does not finds
-- nothing, and a backend reads no page for it.
looksFor :: TitleQuery -> Bool
looksFor q = T.length (typedLetters q.letters).lower >= minLetters && titleLimit q > 0

-- | The pages of a lookup among the candidates: those that match its
-- letters ('matchRank'), the better rank first, then by title in lower
-- case, the title as written, the space's path, the page's path, and the
-- page's id, so that the order is the same whatever order the candidates
-- come in. At most 'titleLimit' of them, cut after the order. None for a
-- query that 'looksFor' nothing.
--
-- The candidates are the pages the backend chose: the reader may view
-- them, and they are no template and not in the space left out.
rankTitles :: TitleQuery -> [TitleHit] -> [TitleHit]
rankTitles q candidates
  | not (looksFor q) = []
  | otherwise = map snd (take (titleLimit q) (sortOn fst ranked))
  where
    typed = typedLetters q.letters
    ranked =
      [ ((r, T.toLower c.title, c.title, map slugText c.spacePath, map slugText c.pagePath, c.page.text), c)
      | c <- candidates
      , Just r <- [matchRank typed (matchable c.title c.pagePath)]
      ]
