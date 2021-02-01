%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao genealogica.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

pertence([Y|T],Y).
pertence([H|T],Y):-
  H\=Y,
  pertence(T,Y).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

comprimento([],0).
comprimento([X|Y],R):- comprimento(Y,N), R is N+1.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -

diferente([],0).
diferente([H|T],Y):- pertence(T,H), diferente(T,Y).
diferente([H|T],Y):- \+(pertence(T,H)), diferente(T,K), Y is K +1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

apaga1([],L,T).
apaga1([H|T],L,X):- apaga1([],L,X).