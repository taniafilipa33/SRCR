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
% Extensao do predicado soma : X,Y,Soma -> {V,F}

soma(X,Y,R):- R is X+Y .

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado soma : X,Y,Z,Soma -> {V,F}

soma(X,Y,Z,Soma) :- Soma is X+Y+Z.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado operacao : OP,X,Y,R -> {V,F}

operacao(soma,X,Y,R):- R is X+Y.
operacao(divisao,X,Y,R):- Y\=0, R is X/Y.
operacao(menos,X,Y,R):- R is X-Y.
operacao(multiplicacao,X,Y,R):- R is X*Y.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado somaL : L,R -> {V,F}

soma([],0).
soma([X|Y], N ) :- soma(Y,R), N is X+R.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado maior : X,Y,R -> {V,F}

maior(X,Y,X):- X>Y.
maior(X,Y,Y):- X=<Y.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado maior : X,Y,R -> OP,X,Y,R -> {V,F}

operacao(soma,X,Y,R,Op1):- Op1 is X+Y+R.
operacao(divisao,X,Y,R,Op2):- Y\=0, R\=0 ,Op2 is X/Y/R.
operacao(menos,X,Y,R,Op3):- Op3 is X-Y-R.
operacao(multiplicacao,X,Y,R,Op4):- Op4 is X*Y*R.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado maior : X,Y,R -> OP,X,Y,R -> {V,F}

operacao(soma,[],0).
operacao(divisao,[],1).
operacao(menos,[],0).
operacao(multiplicacao,[],1).
operacao(soma,[X|R],Op1):-operacao(soma,R,N,Op1), Op1 is X+N.
operacao(divisao,[X|R],Op2):- operacao(divisao,R,N,Op2), N\=0 ,Op2 is X/N.
operacao(menos,[X|R],Op3):- operacao(menos,R,N,Op3),Op3 is X-N.
operacao(multiplicacao,[X|R],Op4):-operacao(multiplicacao,R,N,Op4), Op4 is X*N.