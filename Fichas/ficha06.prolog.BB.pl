--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

:- dynamic '-'/1.
:- dynamic mamifero/1.
:- dynamic morcego/1.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).
	
%--------------------------------- - - - - - - - - - questãao 1


voa(X) :-
	ave(X), nao( excecao( voa(X))). 

voa(X) :- excecao( -voa(X)).

%--------------------------------- - - - - - - - - - - questão 2

excecao(voa(X)) :- avestruz(X).
excecao(voa(X)) :- pinguim(X).
excecao(-voa(X)) :- morcego(X).

-voa(X) :-
	mamifero(X), nao( excecao( -voa(X))). %o morcegp voa...

-voa(X) :- excecao( voa(X)).

%--------------------------------- - - - - - - - - - - questão 3

-voa(tweety).

%--------------------------------- - - - - - - - - - - questão 4

ave(pitigui).

%--------------------------------- - - - - - - - - - - questão 5
 
 ave(X) :- canario(X).

%--------------------------------- - - - - - - - - - - questão 6

ave(X) :- piriquitos(X).

 %--------------------------------- - - - - - - - - - - questão 7

canario(piupiu).

 %--------------------------------- - - - - - - - - - - questão 8

 mamifero(silvestre).

 %--------------------------------- - - - - - - - - - - questão 9

 mamifero(X) :- cao(X).

 %--------------------------------- - - - - - - - - - - questão 10

 mamifero(X) :- gato(X).

 %--------------------------------- - - - - - - - - - - questão 11

 cao(boby).

 %--------------------------------- - - - - - - - - - - questão 12

ave(X):-avestruz(X).

 %--------------------------------- - - - - - - - - - - questão 13

ave(X) :- pinguim(X).

 %--------------------------------- - - - - - - - - - - questão 14

 avestruz(trux).

 %--------------------------------- - - - - - - - - - - questão 15

 pinguim(pingu).

 %--------------------------------- - - - - - - - - - - questão 16

mamifero(X) :- morcego(X).

 %--------------------------------- - - - - - - - - - - questão 17

 morcego(batemene).



