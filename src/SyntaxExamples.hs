module SyntaxExamples where

transition1 = " A / B , R -> qstare ;  "
transition2 = "X/_,N->q2;"
transition3 = "Y/,,L->q3" -- error invalid character and missing ;
transition4 = "AB/CD , R -> q41;" -- error expected character, not string
transition5 = "B/B,C -> q" -- error C is not a valid move

state1 = "initial state nume{ B / B, L -> nume; }"
state2 = "accept state okk;"
state3 = "reject state nu_ok;"
state4 = "state renume {A/A,N->nume;H/B,R->renume;}"

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
\   automata main (not n1, not n2) {    \
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
    Automata "not" [
        State "q0" [
            Transition 'B' 'B' R "q0",
            Transition '1' '0' L "q1",
            Transition '0' '1' L "q1"
            ] 
            True, 
        Accept "q1"
        ] 
        [],
    Automata "main" [
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
        [("n1", "not"), ("n2", "not")]
]
-}

{- Macros examples

automata comp = complement(and);
automata nou = intersect(not, not, not);
automata vechi = reunion(and, or);
automata move8r = move(R, 8);
automata move2l = move(L, 2);
automata rescriere = override(L, 5, 'V');
automata scriere = place("abcde"); -- override la dreapta cu un sir de caractere
automata lant = chain(not, or, move2l);
automata insert8 = shift(R, 8);
automata delete9 = shift(L, 8);
automata repetare = repeat(5, not); -- echivalent cu chain(not, not, not, not, not);

Maybe:
automata goto_marker = find(R, 'X');
automata memcpy = copy(R, 8, 40); -- urmatoarele 8 pozitii vor fi copiate peste 40 de casute la dreapta

-}