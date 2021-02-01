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
% Extensao do predicado filho: Filho,Pai -> {V,F}

filho( joao,jose ).
filho( jose,manuel ).
filho(manuel,ana).
filho(ana,nancy).
filho( carlos,jose ).
filho( filipe,paulo).
filho( maria,paulo).
filho(maria,paulo).
filho(filipe,paulo).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pai: Pai,Filho -> {V,F}

pai( P,F ) :-
    filho( F,P ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado avo: Avo,Neto -> {V,F}


avo( A,F) :-
    filho(P,A),filho( F,P).


avo(antonio,nadia).
neto(F,A) :-
    avo(A,F).
neto(nuno,ana).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado genero: Masculino,Feminino -> {V,F}

homem(joao).
homem(jose).
mulher(maria).
mulher(joana).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado bisavo: Bisavo,Bisneto -> {V,F}

bisavo(B,F) :- filho(F,P),filho(P,A),filho(A,B).

bisneto(N,B) :- bisavo(B,N).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: Descendente,Ascendente -> {V,F}


descendente(A,X):-
		filho(A,G),
		descendente(G,X).

descendente(X,Y):-filho(X,Y).

ascendente(A,F):-
		descendente(F,A).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: Descendente,Ascendente,Grau -> {V,F}

grau(A,X,N):-
	filho(A,G),
	grau(G,X,C),
	N is C+1.

grau(A,X,1):-filho(A,X).

grauM(A,F,N):-
	grau(F,A,N).






