% ---------------------------------------------------
% Bi-directional Definite Clause Grammar
% ---------------------------------------------------

:- style_check([-discontiguous, -singleton]).


:- op(900, yfx, '==>').
:- op(800, yfx, &).

% ---------------------------------------------------

s(mode:Mode, sem:M1-M2, st:St) -->
  np(mode:Mode, arg:X, sco:S, sem:M1-M2),
  vp(mode:Mode, arg:X, sem:S, st:St),
  ['.'].

np(mode:Mode, arg:X, sco:S, sem:M1-M2) -->
  det(mode:Mode, arg:X, res:R, sco:S, sem:M1-M2),
  noun(mode:Mode, arg:X, sem:R, st:pos).
np(mode:Mode, arg:X, sem:S, st:St) -->
  noun(mode:Mode, arg:X, sem:S, st:St).

vp(mode:Mode, arg:X, sem:M, st:St) -->
  iv(mode:Mode, arg:X, sem:M, st:St).


% ---------------------------------------------------
% Rules for processing
% ---------------------------------------------------

s(mode:proc, sem:M1-M2, st:St) -->
  np(mode:proc, arg:X, sem:M1-M0, st:pos),
  vp(mode:proc, arg:X, sem:M0-M2, st:St),
  ['.'].

vp(mode:proc, arg:X, sem:S, st:pos) -->
  ['is'],
  np(mode:proc, arg:X, sem:S, st:pos).
vp(mode:proc, arg:X, sem:S, st:pos) -->
  ['is'],
  jj(mode:proc, arg:X, sem:S, st:pos).

vp(mode:proc, arg:X, sem:M1-M2, st:neg) -->
  ['does not'],
  iv(mode:proc, arg:X, sem:M1-M2, st:neg).
vp(mode:proc, arg:X, sem:S, st:neg) -->
  ['is not'],
  np(mode:proc, arg:X, sem:S, st:neg).
vp(mode:proc, arg:X, sem:S, st:neg) -->
  ['is not'],
  jj(mode:proc, arg:X, sem:S, st:neg).

det(mode:proc, arg:X, res:[[]|M1]-M2, sco:[[]|M2]-[Sco, Res, M3|M4], sem:M1-[[forall(X, Res ==> Sco)|M3]|M4]) --> ['every'].

det(mode:proc, arg:X, res:[[]|M1]-M2, sco:[[]|M2]-[Sco, Res, M3|M4], sem:M1-[[none(X, Res ==> Sco)|M3]|M4]) --> ['no'].

noun(mode:proc, arg:X, sem:[M1|M2]-[[T|M1]|M2], st:pos) -->
   { lexicon(cat:noun, wform:List, arg:X, term:T);
     (   agent(X),  List = [X]  )},
   List.
noun(mode:proc, arg:X, sem:[M1|M2]-[[-T|M1]|M2], st:neg) -->
   { lexicon(cat:noun, wform:List, arg:X, term:T);
     (    agent(X),  List = [X]  )},
   List.

iv(mode:proc, arg:X, sem:[M1|M2]-[[T|M1]|M2], st:pos) -->
   { lexicon(cat:iv, wform:List, arg:X, term:T) },
   List.
iv(mode:proc, arg:X, sem:[M1|M2]-[[-T|M1]|M2], st:neg) -->
   { lexicon(cat:iv, wform:List, arg:X, term:T) },
   List.

jj(mode:proc, arg:X, sem:[M1|M2]-[[T|M1]|M2], st:pos) -->
   { lexicon(cat:jj, wform:List, arg:X, term:T) },
   List.
jj(mode:proc, arg:X, sem:[M1|M2]-[[-T|M1]|M2], st:neg) -->
   { lexicon(cat:jj, wform:List, arg:X, term:T) },
   List.


% ---------------------------------------------------
% Rules for converting to ASP
% ---------------------------------------------------

make_ASP([]).

make_ASP([forall(_,[T1]==>[T2])|Clauses]) :-
   portray_clause(T2 :- T1),
   make_ASP(Clauses).
make_ASP([none(_,[T1]==>[T2])|Clauses]) :-
   portray_clause(-T2 :- T1),
   make_ASP(Clauses).

make_ASP([T|Clauses]) :-
   write(T), writeln(.),
   make_ASP(Clauses).
make_ASP([[T1|[Ts]]|Clauses]) :-
   write(T1), write(" , "),
   make_ASP([Ts|Clauses]).


% ---------------------------------------------------
% Rules for generation
% ---------------------------------------------------

s(mode:gen, sem:S, st:St) -->
  np(mode:gen, arg:X, sem:S, st:pos),
  vp(mode:gen, arg:X, sem:S, st:St),
  ['.'].

vp(mode:gen, arg:X, sem:S, st:St) -->
  ['is'],
  { ( agent(X) ->
     true;
     X1 = X
     ) },
  np1(mode:gen, arg:X1, sem:S, st:St).
vp(mode:gen, arg:X, sem:S, st:pos) -->
  ['is'],
  jj(mode:gen, arg:X, sem:S, st:pos).
vp(mode:gen, arg:X, sem:S, st:neg) -->
  ['is not'],
  jj(mode:gen, arg:X, sem:S, st:neg).

det(mode:gen, arg:X, res:[Res, Sco, M3|M4]-[[]|M2], sco:M2-[[]|M1], sem:[[forall(X, Res ==> Sco)|M3]|M4]-M1) --> ['every'].

det(mode:gen, arg:X, res:[Res, Sco, M3|M4]-[[]|M2], sco:M2-[[]|M1], sem:[[none(X, Res ==> Sco)|M3]|M4]-M1) --> ['no'].

np(mode:gen, arg:X, sem:[[T|M1]|M2]-[M1|M2], st:_) -->
   { lexicon(cat:_, wform:_, arg:X, term:T),  agent(X),  List = [X] },
   List.
np(mode:gen, arg:X, sem:[[-T|M1]|M2]-[M1|M2], st:_) -->
   { lexicon(cat:_, wform:_, arg:X, term:T),  agent(X),  List = [X] },
   List.

np1(mode:gen, arg:X, sem:[[T|M1]|M2]-[M1|M2], st:pos) -->
   { lexicon(cat:noun, wform:List, arg:X, term:T) },
   List.
np1(mode:gen, arg:X, sem:[[-T|M1]|M2]-[M1|M2], st:neg) -->
   { lexicon(cat:noun, wform:List, arg:X, term:T) },
   ['not'],
   List.

noun(mode:gen, arg:X, sem:[[T|M1]|M2]-[M1|M2], st:pos) -->
   { lexicon(cat:_, wform:_, arg:X, term:T),  agent(X),  List = [X] },
   List.

noun(mode:gen, arg:X, sem:[[T|M1]|M2]-[M1|M2], st:pos) -->
   { lexicon(cat:noun, wform:List, arg:X, term:T) },
   List.
noun(mode:gen, arg:X, sem:[[-T|M1]|M2]-[M1|M2], st:neg) -->
   { lexicon(cat:noun, wform:List, arg:X, term:T) },
   ['is not'],
   List.

iv(mode:gen, arg:X, sem:[[T|M1]|M2]-[M1|M2], st:pos) -->
   { lexicon(cat:iv, wform:List, arg:X, term:T) },
   List.
iv(mode:gen, arg:X, sem:[[-T|M1]|M2]-[M1|M2], st:neg) -->
   { lexicon(cat:iv, wform:List, arg:X, term:T) },
   ['does not'],
   List.

jj(mode:gen, arg:X, sem:[[T|M1]|M2]-[M1|M2], st:pos) -->
   { lexicon(cat:jj, wform:List, arg:X, term:T) },
   List.
jj(mode:gen, arg:X, sem:[[-T|M1]|M2]-[M1|M2], st:neg) -->
   { lexicon(cat:jj, wform:List, arg:X, term:T) },
   List.



% ---------------------------------------------------
% Lexicon
% ---------------------------------------------------

lexicon(cat:noun, wform:[student], arg:X, term:student(X)).
lexicon(cat:noun, wform:[girl], arg:X, term:girl(X)).
lexicon(cat:noun, wform:[boy], arg:X, term:boy(X)).
lexicon(cat:iv, wform:[dreams], arg:X, term:dream(X)).
lexicon(cat:iv, wform:[parties], arg:X, term:party(X)).
lexicon(cat:iv, wform:[work], arg:X, term:work(X)).
lexicon(cat:iv, wform:[studies], arg:X, term:study(X)).
lexicon(cat:jj, wform:[good], arg:X, term:good(X)).
lexicon(cat:jj, wform:[bad], arg:X, term:bad(X)).
lexicon(cat:jj, wform:[happy], arg:X, term:happy(X)).
lexicon(cat:jj, wform:[naive], arg:X, term:naive(X)).
agent(bob).
agent(alice).

lexicon(cat:noun, wform:[penguin], arg:X, term:penguin(X)).
lexicon(cat:noun, wform:[bird], arg:X, term:bird(X)).
lexicon(cat:noun, wform:[eagle], arg:X, term:eagle(X)).
lexicon(cat:iv, wform:[fly], arg:X, term:fly(X)).
agent(emperor).
agent(baldey).

% ---------------------------------------------------
% ---------------------------------------------------
% Run it!
% ---------------------------------------------------
% ---------------------------------------------------

test :-
   Sentences = [ [alice, 'is', girl, '.'],
                 ['every', girl, 'is', happy, '.'],
                 ['every', student, 'is not', happy, '.'],
                 [bob, 'is', student, '.']
               ],
   writeln("1. Input sentences: "),
   write_sentences(Sentences), nl,
   writeln("2. Internal representation: "),
   process_sentences([[]], [M], Sentences), nl,
   reverse(M, RM),
   writeln("3. ASP representation: "),
   make_ASP(RM), nl,
   writeln("4. Generated (back) sentences: "),
   generate_sentences([RM], _), nl
.

% ---------------------------------------------------
% ---------------------------------------------------
% ---------------------------------------------------


process_sentences(M, M, []).
process_sentences(M1, M3, [Sentence|Sentences]) :-
   (  (member('does not', Sentence); member('is not', Sentence)) ->
       s(mode:proc, sem:M1-M2, st:neg, Sentence, []);
       s(mode:proc, sem:M1-M2, st:pos, Sentence, [])
   ),
   member(Mm, M2),
   findall(V, (member(V, Mm), nonvar(V)), D),
   length(D,N),
   (   N>0 ->
       delete(Mm, D, Mmn),
       select(Mm, M2, Mmn, M2n);
       M2n = M2
   ),
   numbervars(M2n),
   M2n = [[Out|_]],
   writeln(Out),
   process_sentences(M2n, M3, Sentences).


generate_sentences([[]], []).
generate_sentences(M1, [Sentence|Sentences]) :-
   (   s(mode:gen, sem:M1-M2, st:pos, Sentence, []);
       s(mode:gen, sem:M1-M2, st:neg, Sentence, [])
   ),
   writeln(Sentence),
   generate_sentences(M2, Sentences).


write_sentences([]).
write_sentences([Sentence|Sentences]) :-
   writeln(Sentence),
   write_sentences(Sentences).
