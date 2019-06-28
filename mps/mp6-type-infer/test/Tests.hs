module Tests where

import Common
import Parser
import Infer

import Data.Map.Strict as H (Map, empty, fromList)

unifyRun :: String -> String
unifyRun s =
  case runFVState (unify (VarExp "") (parseEqList (lexer s))) 0 of
    ValidRes sEnv _ -> stringOfSub sEnv 5
    ErrorRes _ -> "error"

inferFV :: String -> String -> FVState String
inferFV s1 s2 = do
  tau <- freshTau
  sEnv <- infer (H.fromList (parseEnv (lexer s1))) (parseExp (lexer s2)) tau
  let tau' = liftMonoTy sEnv tau
  return (stringOfMonoTy (canonize tau') tau')

inferRun :: String -> String -> String
inferRun s1 s2 =
  case runFVState (inferFV s1 s2) 0 of
    ValidRes s' _ -> s'
    ErrorRes _ -> "error"

tests_unify_elim = [
    (unifyRun "(0 = int)", "{0 -> int}", 1),
    (unifyRun "(0 = int, 1 = bool)", "{0 -> int, 1 -> bool}", 1)
  ]

tests_unify_del = [
    (unifyRun "(int = int)", "{}", 1),
    (unifyRun "(0 = 0)", "{}", 1),
    (unifyRun "(0 = int, int = 0)", "{0 -> int}", 1)
  ]

tests_unify_orient = [
    (unifyRun "(int = 0)", "{0 -> int}", 2)
  ]

tests_unify_decomp = [
    (unifyRun "(0 -> 1 = int -> int)", "{0 -> int, 1 -> int}", 2),
    (unifyRun "(0 * int = (1 * 2) * int, 1 = string, 2 = string)", "{0 -> (string * string), 1 -> string, 2 -> string}", 2)
  ]

tests_unify_error = [
    (unifyRun "(0 = bool, int = 0)", "error", 1),
    (unifyRun "(0 = 0 -> int)", "error", 2),
    (unifyRun "(0 = 1, 1 = 2, 2 = 0 * 0)", "error", 2)
  ]

tests_unify_comp = [
    (unifyRun "(0 * int = 1, 0 = string)", "{0 -> string, 1 -> (string * int)}", 2),
    (unifyRun "(0 -> 1 = 2, 2 = 3 -> 4, 0 = int, 4 = bool)",
        "{0 -> int, 1 -> bool, 2 -> (int -> bool), 3 -> int, 4 -> bool}", 2)
  ]

tests_unify_all :: [(String, [(String, String, Int)])]
tests_unify_all = [
    ("basic elimination", tests_unify_elim),
    ("delete rule", tests_unify_del),
    ("orient rule", tests_unify_orient),
    ("decomposition rule", tests_unify_decomp),
    ("error cases", tests_unify_error),
    ("more complex tests", tests_unify_comp)
  ]

tests_infer_const = [
    (inferRun "()" "46", "int", 1),
    (inferRun "()" "true", "bool", 1),
    (inferRun "()" "()", "unit", 1),
    (inferRun "()" "[]", "'a list", 2)
  ]

tests_infer_var = [
    (inferRun "()" "x", "error", 1),
    (inferRun "(f -> int)" "f", "int", 2),
    (inferRun "(x -> 0. 0)" "x", "'a", 2)
  ]

tests_infer_let = [
    (inferRun "()" "let x = 0 in 0", "int", 1),
    (inferRun "()" "let f = 1 in f", "int", 2),
    (inferRun "(x -> bool)" "let x = 0 in x", "int", 1),
    (inferRun "()" "let h = [] in h", "'a list", 1)
  ]

tests_infer_op = [
    (inferRun "()" "6 + 6", "int", 1),
    (inferRun "()" "6 :: []", "int list", 2),
    (inferRun "(x -> string)" "snd ((print x), (\"6\" ^ \"booboo\"))", "string", 2),
    (inferRun "()" "not ((~ 7 * 8 / 1 - 4) > 3)", "bool", 2),
    (inferRun "()" "fst ((6 = hd (7 :: [])), tl [])", "bool", 3)
  ]

tests_infer_cond = [
    (inferRun "()" "if true then 62 else 252", "int", 2),
    (inferRun "()" "if false then true else false", "bool", 4),
    (inferRun "()" "if false then 0 else \"hello\"", "error", 4)
  ]

tests_infer_fun = [
    (inferRun "()" "fun x -> x + 1", "(int -> int)", 3),
    (inferRun "()" "fun x -> fun y -> 1", "('a -> ('b -> int))", 2),
    (inferRun "(x -> 7)" "fun y -> y + x", "(int -> int)", 3),
    (inferRun "()" "fun x -> x", "('a -> 'a)", 2)
  ]

tests_infer_app = [
    (inferRun "(f -> int -> int)" "f 0", "int", 2),
    (inferRun "(f -> string -> int)" "f 0", "error", 1),
    (inferRun "()" "(fun x -> x + 1) 6", "int", 1),
    (inferRun "(g -> int -> int -> int)" "g 1 2", "int", 1),
    (inferRun "(f -> 0 -> 0)" "f 0", "int", 2),
    (inferRun "(f -> 0 -> 0 -> 0)" "f 0 false", "error", 1),
    (inferRun "(f -> 0 . 1 . (0 -> 1) -> 0 -> 1, g -> int -> int)" "f g", "(int -> int)", 2)
  ]

tests_infer_rec = [
    (inferRun "()" "let rec f x = f x in f false", "'a", 2),
    (inferRun "()" "let rec f x = if 0 > x then (f (x - 1)) + 1 else 1 in f 5", "int", 2),
    (inferRun "()" "let rec f g = g f in f f", "error", 1),
    (inferRun "()" "let rec f x = x in ((f 0), (f false))", "(int * bool)", 5)
  ]

tests_infer_all :: [(String, [(String, String, Int)])]
tests_infer_all = [
    ("constants", tests_infer_const),
    ("vars", tests_infer_var),
    ("let expression", tests_infer_let),
    ("ops", tests_infer_op),
    ("conditional", tests_infer_cond),
    ("functions", tests_infer_fun),
    ("app", tests_infer_app),
    ("let rec expression", tests_infer_rec)
  ]
