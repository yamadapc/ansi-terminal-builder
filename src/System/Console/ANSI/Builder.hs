module System.Console.ANSI.Builder
    (
      Builder (..)
    , putBuilder
    , renderBuilder
    , fuse
    -- * SGRs
    , vBlue
    , vBlueBg
    , vRed
    , vRedBg
    -- * Re-exports
    , (<>)
    , mappend
    , mempty
    )
  where

import           Control.Monad
import           Data.Monoid
import           Data.Sequence          (Seq, index, (<|), (><))
import qualified Data.Sequence          as Sequence
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as Text (toStrict)
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.IO      as Text
import           System.Console.ANSI

-- |
-- An ANSI Builder is a 'Monoid' you can use to build
-- <https://hackage.haskell.org/package/ansi-terminal ansi-terminal> output in
-- a nicer fashion. We provide functions to easily build ANSI text and
-- efficiently output it to the terminal or to a 'Text' data-type.
newtype Builder = Builder ANSITextSeq
  deriving(Show, Eq)

type ANSITextSeq = Seq (Text.Builder, [SGR])

-- |
-- Internally our ANSI terminal builder is a 'Seq' of the Text/SGR pairs we
-- need for the output. This choice is because the concat operation for 'Seq's
-- is more efficient than for lists.
instance Monoid Builder where
    mappend (Builder bs1) (Builder bs2) = Builder (fuse bs1 bs2)
    mempty = Builder mempty

instance IsString Builder where
    fromString s = bstr (fromString s)

-- |
-- Builder from 'Text.Builder'
bstr :: Text.Builder -> Builder
bstr s = Builder (Sequence.singleton (s, mempty))

-- |
-- Builder from 'SGR'
bsgr :: [SGR] -> Builder
bsgr s = Builder (Sequence.singleton (mempty, s))

vBlue :: Builder
vBlue = bsgr [ SetColor Foreground Vivid Blue ]

vBlueBg :: Builder
vBlueBg = bsgr [ SetColor Background Vivid Blue ]

vRed :: Builder
vRed = bsgr [ SetColor Foreground Vivid Red ]

vRedBg :: Builder
vRedBg = bsgr [ SetColor Background Vivid Red ]

-- |
-- We have two sequences of 'Text' builders and 'SGR' lists. When appending
-- them, ideally, we fuse the last element of the first with the first element
-- of the second so that when rendering; there're no unnecessary 'SGR' calls.
--
-- We should use a reversed order sequence to amortize the cost of appending a
-- small sequence to a big one. Currently appending will be 'O(length seq1)'
-- because of the cost of generating the init seq1.
fuse :: ANSITextSeq -> ANSITextSeq -> ANSITextSeq
fuse seq1 seq2
    | len1 == 0 = seq2
    | len2 == 0 = seq1
    | (t2, sgr2) <- head2, (t1, sgr1) <- last1, t1 == mempty =
          fuseEdges mempty t2 (sgr1 <> sgr2)
    | (t2, []) <- head2, (t1, sgr) <- last1 =
          fuseEdges t1 t2 sgr
    | (t2, sgr2) <- head2, (t1, sgr1) <- last1, sgr2 == sgr1 =
          fuseEdges t1 t2 sgr1
    | otherwise = seq1 >< seq2
  where
    fuseEdges t1 t2 sgr = init1 >< (t1 <> t2, sgr) <| tail2

    len1 = Sequence.length seq1
    len2 = Sequence.length seq2
    last1 = seq1 `index` (len1 - 1)
    init1 = Sequence.inits seq1 `index` (len1 - 1)
    head2 = seq2 `index` 0
    tail2 = Sequence.drop 1 seq2

-- |
-- Renders an ANSI 'Builder' as a 'Text'. This uses the 'setSGRCode' function
-- and will break Windows compatibility, but can make your handling more
-- efficient and pure.
renderBuilder :: Builder -> Text
renderBuilder (Builder sq) =
    Text.toStrict $ Text.toLazyText $
    foldr (\(t, _) m -> t <> m) mempty sq

-- |
-- Renders an ANSI 'Builder' to the terminal. Tries to not waste calls to
-- 'setSGR' or 'Text.putStr' by fusing whatever it can.
putBuilder :: Builder -> IO ()
putBuilder (Builder sq) = do
    forM_ sq $ \(t, sgr) -> do
        setSGR sgr
        Text.putStr (Text.toLazyText t)
    setSGR [Reset]
