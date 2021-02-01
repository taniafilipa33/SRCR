%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida
% Representacao de conhecimento imperfeito

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic jogo/3.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a involucao do conhecimento

involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.

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

-jogo(J,A,AC):- nao(jogo(J,A,AC)), nao(excecao(jogo(J,A,AC))).
	
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).



pertence( X,[X|L] ).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L ).


%--------------------------------- - - - - - - - - - -  -  questão i

jogo( 1,aa,500 ).  

%--------------------------------- - - - - - - - - - -  -  questão ii
% Valor nulo tipo I - Desconhecido

jogo( 2,bb,xpto0123 ).
excecao( jogo( Jogo,Arbitro,Ajudas ) ) :-
    jogo( Jogo,Arbitro,xpto0123 ).

%--------------------------------- - - - - - - - - - -  -  questão iii 
%Valor nulo tipo II - Desconhecido tomado de um conjunto de valores

excecao(jogo(3,cc,500)).
excecao(jogo(3,cc,2500)).


%--------------------------------- - - - - - - - - - -  -  questão iv

excecao( jogo( 4,dd,Ajudas ) ) :-
    Ajudas >= 250, Ajudas =< 750.

%--------------------------------- - - - - - - - - - -  -  questão v 
% Valor nulo tipo III - Não permitido

jogo( 5,ee,xpto765 ).
excecao( jogo( Jogo,Arbitro,Ajudas ) ) :-
    jogo( Jogo,Arbitro,xpto765 ).

nulo( xpto765 ).

+jogo( J,A,C ) :: ( solucoes( Ajudas, (jogo( 5,ee,Ajudas ),
                              nao( nulo( Ajudas ) ) ),
                              S ),
                    comprimento( S,N ), N == 0 ).
 

%--------------------------------- - - - - - - - - - -  -  questão vi 

jogo(6,ff,250).
excecao(jogo(6,ff,A)):- A>=5000.

%--------------------------------- - - - - - - - - - -  -  questão vii

jogo(7,gg,xptoResto):- nao(X==2500).
excecao(jogo(J,A,AC)):-
	jogo(J,A,xptoResto).

%--------------------------------- - - - - - - - - - -  -  questão viii

cerca(X,Sup,Inf):- Sup is X*1.25,
					Inf is X*0.75.

excepcao( jogo( 8,hh,X )) :- cerca(1000,XSUP, XINF), 
                             X>XINF,
                             X<XSUP.

%--------------------------------- - - - - - - - - - -  -  questão ix

proximo(X,Sup,Inf):- Sup is X*1.1,
						Inf is X*0.9.

excecao(jogo(9,ii,X)) :- proximo(3000,XSUP,XINF),
							X>XINF,
							X<XSUP.

%--------------------------------- - - - - - - - - - -  -  questão x

%--------------------------------- - - - - - - - - - -  -  questão xi

%--------------------------------- - - - - - - - - - -  -  questão xii

