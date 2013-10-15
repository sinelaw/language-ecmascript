-- | Simple textual diffing of JavaScript programs for inspecting test
-- failures
module Language.ECMAScript5.SourceDiff where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.PrettyPrint
import Data.List (intersperse, intercalate)

jsDiff :: Program a -> Program a -> String
jsDiff js1 js2 = 
  let plines = lines . show . prettyPrint
      diff = getGroupedDiff (plines js1) (plines js2)
  in ppDiff diff
