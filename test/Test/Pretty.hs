module Test.Pretty where

import Test.Tasty
import Test.Tasty.QuickCheck

import Language.ECMAScript5.Parser
import Language.ECMAScript5.PrettyPrint
import Language.ECMAScript5.Syntax
import Language.ECMAScript5.Syntax.Arbitrary()
import Language.ECMAScript5.Syntax.Annotations
import Language.ECMAScript5.SourceDiff
import Data.List

tests_pretty :: TestTree
tests_pretty = testGroup "Pretty-printer tests"
               [testProperty "Parse is the inverse of pretty" prettyParseEquivalence
               ,testProperty "Expressions not safe to print in an Expression Statement" unsafeExprStmtProp]

prettyParseEquivalence :: Program () -> Property
prettyParseEquivalence orig =
  let pp = show $ prettyPrint orig
  in case parseFromString pp of
    Left e -> 
      let err = "Can't parse pretty-printed code. The error was: " ++ (show e) ++
                "\nThe pretty-printed code in question:\n" ++ pp
      in whenFail (putStrLn err) False
    Right parsed ->
      let eq = (removeAnnotations parsed) == orig
          msg ="The parse of the pretty-printed AST didn't match the original\n"
               ++"Diff:\n" ++ jsDiff orig (reannotate (const ()) parsed)
      in whenFail (putStrLn msg) eq

unsafeExprStmtProp :: Expression () -> Bool
unsafeExprStmtProp e =
  let se = show $ prettyPrint e
      actuallyUnsafe = "{" `isPrefixOf` se || "function" `isPrefixOf` se
      oracleUnsafe = unsafeInExprStmt e
  in  actuallyUnsafe == oracleUnsafe
