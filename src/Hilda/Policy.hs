-- | Permission modes. A mode maps each tool 'Effect' to a verdict; the
-- agent loop asks the user only when the verdict is 'Confirm'.
module Hilda.Policy
  ( Mode (..)
  , Verdict (..)
  , modeName
  , parseMode
  , authorize
  , visibleTools
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Hilda.Tools (Effect (..), Tool (..))

data Mode
  = Yolo     -- ^ Run every tool without asking. The default.
  | Ask      -- ^ Confirm each write, edit and shell command.
  | ReadOnly -- ^ Only tools that observe; others are hidden and refused.
  deriving stock (Eq, Show, Enum, Bounded)

data Verdict = Allow | Confirm | Deny Text
  deriving stock (Eq, Show)

modeName :: Mode -> Text
modeName Yolo     = "yolo"
modeName Ask      = "ask"
modeName ReadOnly = "read-only"

parseMode :: Text -> Maybe Mode
parseMode t = lookup (T.toLower t) [(modeName m, m) | m <- [minBound .. maxBound]]

authorize :: Mode -> Effect -> Verdict
authorize Yolo _           = Allow
authorize _ Observe        = Allow
authorize Ask _            = Confirm
authorize ReadOnly effect  = Deny ("refused: " <> T.toLower (T.pack (show effect)) <> " is not allowed in read-only mode")

-- | Tools advertised to the model. Denied tools are not offered at all.
visibleTools :: Mode -> [Tool] -> [Tool]
visibleTools mode = filter (not . denied . authorize mode . toolEffect)
  where
    denied (Deny _) = True
    denied _        = False
