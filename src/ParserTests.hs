module ParserTests where

import AmethystSyntax
import AdvancedParser
import SyntaxExamples

-- Examples on running advanced parsers (error and position logging)
char1, char2, char3 :: Maybe (Leftover, Either Error Char)
char1 = runParser (charP 'a') (Leftover "abc" 0 0)
char2 = runParser (charP 'a') (Leftover "bcd" 0 1)
char3 = runParser (charP '\n') (Leftover "\nhello" 2 2)

cond1, cond2, cond3 :: Maybe (Leftover, Either Error Char)
cond1 = runParser (condCharP (`elem` "abc")) (Leftover "carbon" 5 5)
cond2 = runParser (condCharP (`elem` "abc")) (Leftover "zinc" 5 5)
cond3 = runParser (condCharP (`elem` ['0'..'9'])) (Leftover "724" 0 0)

str1, str2 :: Maybe (Leftover, Either Error String)
str1 = runParser (stringP "ana are") (Leftover "ana are mere" 0 0)
str2 = runParser (stringP "ana are") (Leftover "ana nu are mere" 0 0)

span1, span2, span3 :: Maybe (Leftover, Either Error String)
span1 = runParser (spanP (`elem` ['A'..'Z'])) (Leftover "EXEMPLul 1" 0 0)
span2 = runParser (spanP (`elem` ['A'..'Z'])) (Leftover "exemplul 2" 0 0)
span3 = runParser (notNull $ spanP (`elem` ['A'..'Z'])) (Leftover "exemplu null" 0 0)

condList1, condList2 :: Maybe (Leftover, Either Error [Char])
condList1 = runParser (condition ((== 3).length) (spanP (`elem` "abc"))) (Leftover "abczabc2" 0 0)
condList2 = runParser (condition ((== 3).length) (spanP (`elem` "abc"))) (Leftover "abcabc" 0 0)

white1, white2, white3, white4 :: Maybe (Leftover, Either Error String)
white1 = runParser ws (Leftover "    white space" 0 0)
white2 = runParser ws (Leftover "no space" 0 0)
white3 = runParser ws2 (Leftover "no space" 0 0)
white4 = runParser ws (Leftover "  \n\n  \n    hello" 0 0)

num1, num2 :: Maybe (Leftover, Either Error Int)
num1 = runParser numberP (Leftover "07658" 0 0)
num2 = runParser numberP (Leftover "  7658" 0 0)

lit1, lit2, lit3 :: Maybe (Leftover, Either Error String)
lit1 = runParser literalP (Leftover "abDe10" 0 0)
lit2 = runParser literalP (Leftover "AB2;" 0 0)
lit3 = runParser literalP (Leftover "word1 word2" 0 0)

word1, word2 :: Maybe (Leftover, Either Error String)
word1 = runParser wordP (Leftover "nume{}" 0 0) 
word2 = runParser wordP (Leftover "q0DEabc" 0 0) 

tape1, tape2 :: Maybe (Leftover, Either Error String)
tape1 = runParser tapeP (Leftover "SPACE()012  " 0 0) 
tape2 = runParser tapeP (Leftover "AbcDE" 0 0)

sym1, sym2, sym3, sym4 :: Maybe (Leftover, Either Error Char)
sym1 = runParser symbolP (Leftover "@AB" 1 1)
sym2 = runParser symbolP (Leftover "()" 1 1)
sym3 = runParser symbolP (Leftover "123" 1 1)
sym4 = runParser symbolP (Leftover "q0" 1 1)

move1, move2, move3, move4 :: Maybe (Leftover, Either Error Move)
move1 = runParser moveP (Leftover "L -> q0" 7 2)
move2 = runParser moveP (Leftover "R-> q1" 7 2)
move3 = runParser moveP (Leftover "N->q2" 7 2)
move4 = runParser moveP (Leftover "C -> q3" 7 2)

-- Error logging examples

charE1, charE2, charE3 :: Maybe (Leftover, Either Error Char)
charE1 = runParser (charPE 'a') (Leftover "abc" 0 0)
charE2 = runParser (charPE 'a') (Leftover "bcd" 0 0)
charE3 = runParser (charPE '\n') (Leftover "\nhello" 2 2)

moveE1, moveE2, moveE3:: Maybe (Leftover, Either Error Move)
moveE1 = runParser movePE (Leftover "L -> q0" 7 2)
moveE2 = runParser movePE (Leftover "x-> q1" 7 2)
moveE3 = runParser movePE (Leftover "@->q2" 7 2)

tr1, tr2, tr3, tr4, tr5, tr6, tr7, tr8 :: Maybe (Leftover, Either Error Transition)
tr1 = runParser transitionPE (Leftover transition1 0 0)
tr2 = runParser transitionPE (Leftover transition2 0 0)
tr3 = runParser transitionPE (Leftover transition3 0 0)
tr4 = runParser transitionPE (Leftover transition4 0 0)
tr5 = runParser transitionPE (Leftover transition5 0 0)
tr6 = runParser transitionPE (Leftover transition6 0 0)
tr7 = runParser transitionPE (Leftover transition7 0 0)
tr8 = runParser transitionPE (Leftover transition8 0 0)

st1, st2, st3, st4, st5, st6, st7, st8, st9 :: Maybe (Leftover, Either Error State)
st1 = runParser statePE (Leftover state1 0 0)
st2 = runParser statePE (Leftover state2 0 0)
st3 = runParser statePE (Leftover state3 0 0)
st4 = runParser statePE (Leftover state4 0 0)
st5 = runParser statePE (Leftover state5 0 0)
st6 = runParser statePE (Leftover state6 0 0)
st7 = runParser statePE (Leftover state7 0 0)
st8 = runParser statePE (Leftover state8 0 0)
st9 = runParser statePE (Leftover state9 0 0)

ma1, ma2, ma3, ma4, ma5, ma6, ma7, ma8 :: Maybe (Leftover, Either Error Automaton)
ma1 = runParser machinePE (Leftover machine1 0 0)
ma2 = runParser machinePE (Leftover machine2 0 0)
ma3 = runParser machinePE (Leftover machine3 0 0)
ma4 = runParser machinePE (Leftover machine4 0 0)
ma5 = runParser machinePE (Leftover machine5 0 0)
ma6 = runParser machinePE (Leftover machine6 0 0)
ma7 = runParser machinePE (Leftover machine7 0 0)
ma8 = runParser machinePE (Leftover machine8 0 0)

mc1, mc2, mc3, mc4, mc5, mc6, mc7, mc8, mc9, mc10, mc11, mc12, mc13, mc14 :: Maybe (Leftover, Either Error Automaton)
mc1 = runParser macroPE (Leftover macro1 0 0)
mc2 = runParser macroPE (Leftover macro2 0 0)
mc3 = runParser macroPE (Leftover macro3 0 0)
mc4 = runParser macroPE (Leftover macro4 0 0)
mc5 = runParser macroPE (Leftover macro5 0 0)
mc6 = runParser macroPE (Leftover macro6 0 0)
mc7 = runParser macroPE (Leftover macro7 0 0)
mc8 = runParser macroPE (Leftover macro8 0 0)
mc9 = runParser macroPE (Leftover macro9 0 0)
mc10 = runParser macroPE (Leftover macro10 0 0)
mc11 = runParser macroPE (Leftover macro11 0 0)
mc12 = runParser macroPE (Leftover macro12 0 0)
mc13 = runParser macroPE (Leftover macro13 0 0)
mc14 = runParser macroPE (Leftover macro14 0 0)

p1, p2, p3 :: Maybe (Leftover, Either Error Program)
p1 = runParser programPE (Leftover program1 0 0)
p2 = runParser programPE (Leftover program2 0 0)
p3 = runParser programPE (Leftover program3 0 0)
