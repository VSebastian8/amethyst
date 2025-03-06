module SyntaxExamples where

-- Transition Examples 
transition1, transition2, transition3, transition4, transition5, transition6, transition7, transition8 :: String
transition1 = " A / B , R -> qstare ;  "
transition2 = "X/_,N->q2;"
transition3 = "Y/,,L->q3" -- error expected tape symbol found ,
transition4 = "AB/CD , R -> q41;" -- error expected / found B
transition5 = "B/B,C -> q" -- error expected move found C
transition6 = "A/AA,L -> we"; -- error expected , found A
transition7 = "A/B,R-> starE;" -- error forbidden symbol
transition8 = "A/B,N -> qstare " -- error expected ;

-- State Examples
state1, state2, state3, state4, state5, state6, state7, state8, state9 :: String
state1 = "initial state nume{ B / B, L -> nume; --State done\n} "
state2 = "accept state okk;"
state3 = "reject state nu_ok;"
state4 = "state renume {{-state begins-}A/A,N->nume; --a transition\n {-comment\nin\nstate-}  H/B,R->renume;}"
state5 = "acceptstate bad;" -- error missing space
state6 = "reject statebad;" -- error missing space
state7 = "state {A/A,R->q;}" -- error missing state name
state8 = "initial state q;" -- error missing {
state9 = "initial state q2{}" -- error zero transitions

-- Macro Examples
macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8, macro9, macro10, macro11, macro12, macro13, macro14 :: String
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
macro11= "automata delete19 = shift(L, 19);"
macro12 = "automata bad = repeat(hello);" -- error expected number
macro13 = "automata bad = move(3, L);" -- error expected move
macro14 = "automata ch = chain();" -- error expected machine type
{- Maybe macros:

automata goto_marker = find(R, 'X');
automata memcpy = copy(R, 8, 40); -- urmatoarele 8 pozitii vor fi copiate peste 40 de casute la dreapta
automata luck = random(q0, q1, q2, 2, 3, 4); -- state names and weights, this only if we have non-determinism
-}

-- Automata examples
machine1, machine2, machine3, machine4, machine5, machine6, machine7, machine8 :: String
machine1 = "automata or(){initial state q0 {0 / 0, R -> q0; 1 / 1 , R -> q1; _ / _ , N -> f;} state q1 {0 / 1 , R -> q1; 1 / 1 , N -> q1; _ / _, N -> f;} accept state f;}"
machine2 = "automata even(){\n\ 
\   {-Comments like this are  \n\
\   useful for multiple \n\
\   lines -}\n\
\   initial state even \n\
\   { -- The state begins here\n\
\      0 / 0, R -> even; --This is a transition\n\
\      1 / 1, R -> odd;\n\
\      B / B, N -> true;\n\
\   }\n\
\   -- We can have comments now\n\
\   state odd{\n\ 
\       0 / 0, R -> odd;\n\
\       1 / 1, R -> even;\n\
\       _ / _, N -> false;\n\
\   }\n\
\   accept state true;\n\
\   reject state false;\n\
\}"
machine3 = "automata two_even(even first, even second) {\n\
\       initial state q0{\n\
\           B / B, R -> first.even;\n\
\           _ / _, N -> first.even;\n\
\       }\n\
\       state first.true{\n\
\           _ / _, R -> second.even;\n\
\       }\n\
\       state first.false {_ / _, N -> false;}\n\
\       state second.true {_ / _, N -> true;}\n\
\       state second.false {_ / _, N -> false;}\n\
\       accept state true;\n\
\       reject state false;\n\
\}"
machine4 = "\
\automata my_mach(){\n\
\   initial state q0{\n\
\       A / @ , R -> qstare X / Y , R -> qstare2;\
\   }\n\
\}\n\
\"  -- error missing ;
machine5 = "automata (not n){}" -- error expected machine name
machine6 = "automata hello(){}" -- error can't have 0 states
machine7 = "automata double (not n1, not ){}" -- error expected component name
machine8 = "automata double (, not n2){}" -- error expected component type

-- A program is just a list of automatas (explicit machines and macros)
program1, program2, program3 :: String
program1 = " \
\   automata not(){    \n\
\       initial state q0 {     \n\
\           @ / @ , R -> q0;   \n\
\           1 / 0 , L -> q1;   \n\
\           0 / 1 , L -> q1;   \n\
\       }   \n\
\       accept state q1;    \n\
\   }   \n\
\   \n\
\   -- Here is a comment in the program\n\
\   {-And another-}--and  another immediately after\n\
\   automata three = repeat(3, not); -- this is a macro\n\
\   automata main (not n1, three n2) {    \n\
\       initial state q0 {      \n\
\           B/B,N-> n1.q0; _/B,R->qrej;    \n\
\       }   \n\
\       state n1.q1 {_/_,N->n2.q0;}    \n\
\       state n2.q1 {   \n\
\           _ / _ , N -> qacc;  \n\
\       }   \n\
\       reject state qrej; accept state qacc;  \n\
\   } -- program done  \n\
\"
program2 = "\
\automata ch = chain(c1, c2);\n\
\   \n\
\auto\n\
\automata a(){}\n\
\"
program3 = "\
\automata output = place(\"HELLO-WORLD!\");\n\
\automata mv = move(R, 12);\n\
\automata place_and_move = chain(output, mv);\n\
\automata do3 = repeat(3, place_and_move);\n\
\automata go.back = move(L, 36);\n\
\automata main = chain(do3, go.back);\n\
\"

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
