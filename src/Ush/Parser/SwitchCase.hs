module Ush.Parser.SwitchCase (switch, (->:), defaultCase) where

import Control.Monad (void)
import Ush.Parser.ParseResult
import Ush.Parser.Parser
import Ush.Util.Collector

data SwitchCaseClause a = SwitchCaseClause (Parser ()) (Parser a)

type SwitchCaseCollector a b = Collector (SwitchCaseClause a) b

(->:) :: Parser a -> Parser b -> SwitchCaseCollector b ()
a ->: b = asCollector $ SwitchCaseClause (void a) b

infix 0 ->:

_switchCase :: [SwitchCaseClause a] -> Parser a
_switchCase [] = fail "Exhausted all switch clauses without a match."
_switchCase (SwitchCaseClause predicate parser : xs) = Parser $ \s ->
  parse
    ( if isSuccess $ parse predicate s
        then parser
        else _switchCase xs
    )
    s

switch :: SwitchCaseCollector a () -> Parser a
switch = _switchCase . collectedValues

defaultCase :: Parser ()
defaultCase = pure ()
