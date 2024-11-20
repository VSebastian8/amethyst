module SyntaxExamples where

-- Transition Examples 
-- runParser transitionP transition1
transition1 = " A / B , R -> qstare ;  "
transition2 = "X/_,N->q2;"
transition3 = "Y/,,L->q3" -- error invalid character and missing ;
transition4 = "AB/CD , R -> q41;" -- error expected character, not string
transition5 = "B/B,C -> q" -- error C is not a valid move

-- State Examples
-- runParser stateP state1
state1 = "initial state nume{ B / B, L -> nume; }"
state2 = "accept state okk;"
state3 = "reject state nu_ok;"
state4 = "state renume {A/A,N->nume;H/B,R->renume;}"

-- Macro Examples
-- runParser macroP macro1
macro1 = "automata comp = complement(and);"
macro2 = "automata int = intersect(not, not, not);"
macro3 = "automata ren = reunion(and, or);"
macro4 = "automata lant = chain(not, or, move2l);" 
macro5= "automata repetare = repeat(5, not);"
macro6 = "automata move8r = move(R, 8);"
macro7 = "automata move2l = move(L, 2);"
macro8 = "automata rescriere = override(L, 5, 'V');"
macro9 = "automata scriere = place(\"ABCDE\");"
macro10 = "automata insert8 = shift(R, 8);"
macro11= "automata delete9 = shift(L, 8);"
{- Maybe macros:

automata goto_marker = find(R, 'X');
automata memcpy = copy(R, 8, 40); -- urmatoarele 8 pozitii vor fi copiate peste 40 de casute la dreapta
automata luck = random(q0, q1, q2, 2, 3, 4); -- state names and weights, this only if we have non-determinism
-}

-- Automata examples
-- runParser automataP auto1
auto1 = "automata or(){initial state q0 {0 / 0, R -> q0; 1 / 1 , R -> q1; _ / _ , N -> f;} state q1 {0 / 1 , R -> q1; 1 / 1 , N -> q1; _ / _, N -> f;} accept state f;}"
auto2 = "automata even(){\ 
\   initial state even {\
\      0 / 0, R -> even;\
\      1 / 1, R -> odd;\
\      B / B, N -> true;\
\   }\
\   state odd{\ 
\       0 / 0, R -> odd;\
\       1 / 1, R -> even;\
\       _ / _, N -> false;\
\   }\
\   accept state true;\
\   reject state false;\
\}"
auto3 = "automata two_even(even first, even second) {\
\       initial state q0{\
\           B / B, R -> first.even;\
\           _ / _, N -> first.even;\
\       }\
\       state first.true{\
\           _ / _, R -> second.even;\
\       }\
\       state first.false {_ / _, N -> false;}\
\       state second.true {_ / _, N -> true;}\
\       state second.false {_ / _, N -> false;}\
\       accept state true;\
\       reject state false;\
\}"

-- A program is just a list of automatas (explicit machines and macros)
program1 = " \
\   automata not(){    \
\       initial state q0 {     \
\           B / B , R -> q0;   \
\           1 / 0 , L -> q1;   \
\           0 / 1 , L -> q1;   \
\       }   \
\       accept state q1;    \
\   }   \
\   \
\   automata three = repeat(3, not);\
\   automata main (not n1, three n2) {    \
\       initial state q0 {      \
\           B/B,N-> n1.q0; _/B,R->qrej;     \
\       }   \
\       state n1.q1 {_/_,N->n2.q0;}     \
\       state n2.q1 {   \
\           _ / _ , N -> qacc;  \
\       }   \
\       reject state qrej; accept state qacc;   \
\   }   \
\ "

{- Syntax Tree:

Program [
    Machine "not" [] [
        State "q0" [
            Transition 'B' 'B' R "q0",
            Transition '1' '0' L "q1",
            Transition '0' '1' L "q1"
            ] 
            True, 
        Accept "q1"
        ] ,
    Macro "three" (Repeat 3 "not"),
    Machine "main" 
        [("n1", "not"), ("n2", "not")]
        [
        State "q0" [
            Transition 'B' 'B' N "n1.q0",
            Transition '_' 'B' R "qrej"
            ]
            True,
        State "n1.q1" [Transition '_' '_' N "n2.q0"] False
        State "n2.q1" [Transition '_' '_' N "qacc"] False
        Reject "qrej",
        Accept "qacc"
        ]
]
-}