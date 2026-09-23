-- | System prompt assembly: hilda's instructions, the working-directory
-- context and any AGENTS.md files, in that order.
module Hilda.Prompt
  ( defaultSystemPrompt
  , assemble
  , discoverAgents
  , loadAgents
  ) where

import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import System.Directory (doesFileExist, doesPathExist)
import System.FilePath (takeDirectory, (</>))

defaultSystemPrompt :: Text
defaultSystemPrompt =
  T.unlines
    [ "You are hilda, a coding agent working in the user's terminal."
    , "Use the tools to inspect files, change them and run commands."
    , "Read a file before you edit it. Prefer edit for existing files and write for new ones."
    , "View files with read, not cat or nl: read pages long files instead of cutting them."
    , "Keep replies short. When the task is done, summarise what changed."
    ]

-- | Combine instruction blocks (default or @--system@, then
-- @--append-system@), the working directory and AGENTS.md files (given as
-- path and content, outermost first). Blank blocks are dropped.
assemble :: [Text] -> FilePath -> [(FilePath, Text)] -> Text
assemble instructions cwd agents =
  T.intercalate "\n\n" $
    filter (not . T.null) (map T.strip instructions)
      <> ["Working directory: " <> T.pack cwd]
      <> [ "Instructions from " <> T.pack path <> ":\n\n" <> T.strip body
         | (path, body) <- agents
         ]

-- | AGENTS.md files from the repository root down to @cwd@. The root is
-- the nearest ancestor containing @.git@; without one, only @cwd@ is used.
discoverAgents :: FilePath -> IO [FilePath]
discoverAgents cwd = do
  let ancestors = chain cwd
  roots <- filterM (doesPathExist . (</> ".git")) ancestors
  let dirs = case roots of
        (root : _) -> takeWhileInclusive (/= root) ancestors
        []         -> [cwd]
  filterM doesFileExist (reverse [d </> "AGENTS.md" | d <- dirs])
  where
    chain d = let p = takeDirectory d in if p == d then [d] else d : chain p
    takeWhileInclusive f = foldr (\x acc -> x : if f x then acc else []) []

loadAgents :: [FilePath] -> IO [(FilePath, Text)]
loadAgents = traverse (\p -> (,) p . decodeUtf8Lenient <$> BS.readFile p)
