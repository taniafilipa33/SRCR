%------TP4------%

%-----Set de umas cenas-----%
:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).



%-----par---------%

par(0).
par(-1) :- !, fail.
par(R) :- R < 0, X is R + 2, par(X).
par(R) :- R > 0, X is R - 2, par(X).


%----par2---------%

par2(R) :- mod(R, 2) =:= 0.

%-----impar-------%

impar(1).
impar(0) :- !, fail.
impar(R) :- R < 1, X is R + 2, impar(X).
impar(R) :- R > 1, X is R - 2, impar(X).


%-----naturais----%
naturais(1).
naturais(X) :- X < 1, !, fail.
naturais(X) :- R is X - 1, naturais(R).


%-----inteiros----%

inteiros(0).
inteiros(X) :- X < 0, R is X + 1, R > 0, !, fail.
inteiros(X) :- X > 0, R is X - 1, R < 0, !, fail.
inteiros(X) :- X > 0, R is X - 1, inteiros(R).
inteiros(X) :- X < 0, R is X + 1, inteiros(R).


%-----divisores---%

divisores(X, _) :- X < 1, !, fail.
divisores(X, L) :- div(X, X, L).

div(_, 1, [1]).
div(X, Y, [Y|L]) :- 0 is mod(X, Y), R is Y - 1, div(X, R, L).
div(X, Y, L) :- R is Y - 1, div(X, R, L).


%-----primo----------------------------------------------------------------ajudapf---%

primo(1).
primo(X) :- divisores(X, L), length(L, T), T is 2.


%-----mdc--------%


mdc(X, X, X).
mdc(X, Y, R) :- X > Y, XX is X - Y, mdc(XX, Y, R).
mdc(X, Y, R) :- X < Y, YY is Y - X, mdc(X, YY, R).



%-----mmc--------%


mmc(X, X, X).
mmc(X, Y, R) :- X > Y, mmcaux(X, Y, X, R).
mmc(X, Y, R) :- Y > X, mmcaux(Y, X, Y, R).

mmcaux(X, Y, Z, X) :- 0 is mod(X, Y).
mmcaux(X, Y, Z, R) :- XX is X + Z, mmcaux(XX, Y, Z, R).


%-----fib--------%

fib(0, 0).
fib(1, 1).
fib(X, R) :- X1 is X - 1, X2 is X - 2, fib(X1, R1), fib(X2, R2), R is R1 + R2.
