%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

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
:- dynamic servico/2.
:- dynamic ato/4.


%-------------------------------------------------
% Aplicação do PMF

-servico(Servico, Nome) :- 
    nao(servico(Servico, Nome)), 
    nao(excecao(servico(Servico, Nome))).
	
-ato(Ato, Prestador, Utente, Dia) :- 
    nao(ato(Ato, Prestador, Utente, Dia)), 
    nao(excecao(ato(Ato, Prestador, Utente, Dia))).

%--------------------------------- - - - - - - - - - -  -

servico(ortopedia, amelia).
servico(obstetricia, ana).


					
ato(penso,ana,joana,sabado).
ato(gesso,amelia,jose,domingo).

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
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).



pertence( X,[X|L] ).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L ).


					
%--------------------------------------------------------- - - - - - - -
%alinea a)
% tabela 1
servico('Amélia','Ortopedia').
servico('Ana','Obstetrícia').
servico('Maria','Obstetrícia').
servico('Mariana','Obstetrícia').
servico('Sofia','Geriatria').
servico('Susana','Geriatria').	 
excepcao(servico('#007','Teodora')).
excepcao(servico('#np9','Zulmira')).
nulo('#np9').

+servico(S,E) :: (solucoes(S,(servico(S,zulmira),nao(nulo(S))),L),comprimento(L,N),N ==0).

excecao(servico(geriatria,raquel))
excecao(servico(obstetricia,raquel)


% tabela 2
ato('Penso','Ana','Joana','sábado').
ato('Gesso','Amélia','José','domingo').
excepcao(ato('#017','Mariana','Joaquina','domingo')).
excepcao(ato('Domicílio','Maria','#121','#251')).
excepcao(ato('Domicílio','Susana','João','segunda')).
excepcao(ato('Domicílio','Susana','José','segunda')).
excepcao(ato('Sutura','#313','Josué','segunda')).
excepcao(ato('Sutura','Maria','Josefa','terça')).
excepcao(ato('Sutura','Maria','Josefa','sexta')).
excepcao(ato('Sutura','Mariana','Josefa','terça')).
excepcao(ato('Sutura','Mariana','Josefa','sexta')).
ato('Penso','Ana','Jacinta',X) :- (X='segunda';
								   X='terça';
								   X='quarta';
								   X='quinta';
								   X='sexta').