
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - Exercicio 2 Individual

% Tania Rocha

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais


:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag(toplevel_print_options,[quoted(true), portrayed(true), max_depth(0)]). 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic '-'/1.
:- dynamic(paragem/11).
:- op(  500,  fx, [ +, - ]).
:- op(  300, xfx, [ mod ]).
:- op(  200, xfy, [ ^ ]).


:- include('BaseC.pl').
:- include('grafo.pl').
:- use_module(library(lists)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).


%---------------------------------  predicados auxiliares ---------


membro(X, [X|_]).
membro(X, [_|Xs]):-
	membro(X, Xs).

membros([], _).
membros([X|Xs], Members):-
	membro(X, Members),
	membros(Xs, Members).


inverso(Xs, Ys):-
	inverso(Xs, [], Ys).

inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):-
	inverso(Xs, [X|Ys], Zs).

% Extensão do predicado solucoes   
solucoes( X,Y,Z ) :-findall( X,Y,Z ).

%concatena listas 
concat(List1, List2, Result):-
   append(List1, List2, Result).

insercao( Termo ) :-assert( Termo ).
insercao( Termo ) :-retract( Termo ), !,fail.
  
remove(P) :- retract(P).
remove(P) :- assert(P),!,fail.



%remove elementos replicados numa lista
remove_duplicates([],[]).

remove_duplicates([H | T], List) :-    
     member(H, T),
     remove_duplicates( T, List).

remove_duplicates([H | T], [H|T1]) :- 
      \+member(H, T),
      remove_duplicates( T, T1).

% Extensão do predicado comprimento : L , R -> {V,R} 
comprimento( S,N ) :-
    length( S,N ).



%-------------------------------- - - -- - - -- -   

%testa se um elemento de uma lista está na outra e retorna
intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        L3 = [Head|L3tail],
        intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :-
        intersection(L1tail, L2, L3).

%lista vazia dá erro
haUniao([X|L]).

%metodo que permite saber se duas paragens fazem parte da mesma carreira
mesma_carreira(paragem(A,S,D,F,G,H,J,K,L,P,O),paragem(I,U,Y,T,R,E,W,Q,Z,X,C)) :- haUniao(PP),intersection(K,Q,PP).

%total de de paragens de uma determinada carreira
numero_paragensCarreira(Ca,R) :- listaDeParagensCarr(Ca,F),length(F,R).

%Dado um carreira, devolve a lista de paragens da mesma
listaDeParagensCarr(Ca,R) :- findall(paragem(I,U,Y,T,R,E,W,Q,Z,X,C),(paragem(I,U,Y,T,R,E,W,Q,Z,X,C), member(Ca,Q) ),R).

%-------------------------------------------- - - - -  -

%%ver se há conecção:
adjacente(X,Y,grafo(_,Es)) :- member(aresta(X,Y),Es).

%distancia entre dois pontos
distancia(N1,N2,N3,N4,R):- N is sqrt((N3-N1)^2+(N4-N2)^2),N=R.

%distancia entre paragens
distEntreParagens(paragem(Id1,La,Lo,_,_,_,_,_,_,_,_),paragem(Id2,L,Lt,_,_,_,_,_,_,_,_),R):-distancia(La,Lo,L,Lt,W), R is W.

%----------------- ALINEA 1 - CALCULAR TRAJETO ENTRE DOIS PONTOS/PARAGENS-----------

% encontar um Caminho (metodo muito ineficiente)

caminho(G,A,B,P) :- caminho1(G,A,[B],P).

caminho1(_,A,[A|P1],[A|P1]).
caminho1(G,A,[Y|P1],P) :- 
   adjacente(X,Y,G), \+ memberchk(X,[Y|P1]), caminho1(G,A,[X,Y|P1],P).


%---pesquisa em profundidade primeiro 

dFirst(G,X,Y,R) :- dFirst(G,X,Y,[X],R).

dFirst(G,X,Y,V,[aresta(X,Y)]) :- adjacente(X,Y,G).

dFirst(G,X,Y,V,[aresta(X,Z)|R]) :-
 adjacente(X,Z,G),
 \+ memberchk(aresta(X,Z),V),
 \+ member(Z,V), 
 dFirst(G,Z,Y,[Z|V],R),
 Z \= Y.

%---pesquisa em largura primeiro 

bfs(G,X,Y,Visited) :-   bfs3(G,Y,Successors, 			% QUEUE
							[],						    % LIST OF NODES VISITED SO FAR
							RevVisited),
							sucessor(G,X,Successors),  % SOLUTION LIST OF NODES
							inverso(RevVisited, Visited).

bfs3(G,Y,[], History, []). %sem solução							

bfs3(G,Y,[aresta(P,Y)|_], History, [aresta(P,Y)|History]).

bfs3(G,Y,[aresta(X,Z)|RestQ], History, RevVisited) :-
						sucessor(G,Z,Cats),			 %LIST OF SUCCESSORS OF Node
						append(RestQ, Cats, Queue),					 %MAKE NEW QUEUE
						bfs3(G,Y, Queue, [aresta(X,Z)|History], RevVisited).
			
%sucessoresTdos(G,[],Vi,V,V).
%sucessoresTdos(G,[aresta(X,Y)|XR],Vi,H,V,R):-sucessor3(G,Y,Vi,H,RE), append(V,RE,P), sucessoresTdos(G,XR,Vi,H,P,R).

%-------- ALINEA 2 - Selecionar apenas algumas das operadoras de transporte para um determinado percurso-----------

%dada uma aresta verifica por que operadora é feito o transporte
auxAl(aresta(X,Y),R):- paragem(X,_,_,_,_,_,OP,_,_,_,_),R = OP.

%através da dFirst
dFOperadoras(G,X,Y,ListaOperadoras,R) :- dFOperadoras(G,X,Y,ListaOperadoras,[X],R).

dFOperadoras(G,X,Y,ListaOperadoras,V,[aresta(X,Y)]) :- adjacente(X,Y,G),
											auxAl(aresta(X,Y),R), 
											memberchk(R,ListaOperadoras).

dFOperadoras(G,X,Y,ListaOperadoras,V,[aresta(X,Z)|R]) :-
 adjacente(X,Z,G),
 auxAl(aresta(X,Z),RR),
 memberchk(RR,ListaOperadoras),
 \+ memberchk(aresta(X,Z),V),
 \+ member(Z,V), 
 dFOperadoras(G,Z,Y,ListaOperadoras,[Z|V],R),
 Z \= Y. 

%Através de Greedy

grTrans(G,X,Y,ListaOperadoras,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccessors(Y,PP,PriorityList),   % PRIORITY 
			retira(PriorityList,[],ListaOperadoras,Res), % retira da lista arestas que não sao feito com as dadas operadoras
			greedy2(G,X,Y,ListaOperadoras, Res,	         
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

greedy2(G,X,Y,ListaOperadoras,[], Visited, []).                  % NO SOLUTION FOUND

greedy2(G,X,Y,ListaOperadoras,[aresta(X,Y)|OtherSolns], Visited, Soln):-
										append(Visited,[aresta(X,Y)],Soln).


greedy2(G,X,Y,ListaOperadoras,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams), %retirar nodos que não tem adjacencia se estes não forem os pretendidos
	sortSuccessors(Y,Cams, PriorityQueue),
	retira(PriorityQueue,[],ListaOperadoras,Res), !, 
	greedy2(G,W,Y,ListaOperadoras,Res, [aresta(Z,W)|Visited], Soln).

%através de A*

estOpe(G,X,Y,ListaOperadoras,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccDist(Y,PP,PriorityList),   % PRIORITY 
			retira(PriorityList,[],ListaOperadoras,Res), %retira arestas de nao Operadoras
			star2(G,X,Y,ListaOperadoras,Res,	         
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

star2(G,X,Y,ListaOperadoras,[], Visited, []).    % NO SOLUTION FOUND

star2(G,X,Y,ListaOperadoras,[aresta(X,Y)|OtherSolns], Visited, Soln):-
										append(Visited,[aresta(X,Y)],Soln).


star2(G,X,Y,ListaOperadoras,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams),
	sortSuccDist(Y,Cams, PriorityQueue),
	retira(PriorityQueue,[],ListaOperadoras,Res), !, 
	star2(G,W,Y,ListaOperadoras,Res, [aresta(Z,W)|Visited], Soln).

%funçáo auxiliar que devolve arestas de paragens desejadas

retira([A],V,L,Res):-auxAl(A,P), member(P,L),
							append(V,[A],Res).
retira([A],V,L,V):-auxAl(A,P), nao(member(P,L)).	
retira([A|S],V,L,Res):-auxAl(A,P), member(P,L), append(V,[A],LL), retira(S,LL,L,Res).
retira([A|S],V,L,Res):-auxAl(A,P),
					    nao(member(P,L)),
						retira(S,V,L,Res).

 %-------- ALINEA 3 - Excluir apenas algumas das operadoras de transporte para um determinado percurso-----------

%através da dFirst
dFNop(G,X,Y,ListaOperadoras,R) :- dFNop(G,X,Y,ListaOperadoras,[X],R).

dFNop(G,X,Y,ListaOperadoras,V,[aresta(X,Y)]) :- 
					adjacente(X,Y,G),
					auxAl(aresta(X,Y),R),
					 \+ memberchk(R,ListaOperadoras).

dFNop(G,X,Y,ListaOperadoras,V,[aresta(X,Z)|R]) :-
 adjacente(X,Z,G),
 auxAl(aresta(X,Z),RR),
 \+ memberchk(RR,ListaOperadoras),
 \+ member(Z,V), 
 dFNop(G,Z,Y,ListaOperadoras,[Z|V],R),
 Z \= Y. 

%através do alg Greedy

grTraN(G,X,Y,ListaOperadoras,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccessors(Y,PP,PriorityList),	 % PRIORITY 
			retira2(PriorityList,[],ListaOperadoras,Res), % retira da lista arestas que não sao feito com as dadas operadoras
			greedy3(G,X,Y,ListaOperadoras, Res,	         
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

greedy3(G,X,Y,ListaOperadoras,[], Visited, []).                  % NO SOLUTION FOUND

greedy3(G,X,Y,ListaOperadoras,[aresta(X,Y)|OtherSolns], Visited, Soln):-
										append(Visited,[aresta(X,Y)],Soln).

greedy3(G,X,Y,ListaOperadoras,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams), %retirar nodos que não tem adjacencia se estes não forem os pretendidos
	sortSuccessors(Y,Cams, PriorityQueue),
	retira2(PriorityQueue,[],ListaOperadoras,Res), !, 
	greedy3(G,W,Y,ListaOperadoras,Res, [aresta(Z,W)|Visited], Soln).


%atraves do alg A*

estOpN(G,X,Y,ListaOperadoras,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccDist(Y,PP,PriorityList),    % PRIORITY 
			retira2(PriorityList,[],ListaOperadoras,Res), %retira arestas de nao Operadoras
			star3(G,X,Y,ListaOperadoras,Res,	        
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

star3(G,X,Y,ListaOperadoras,[], Visited, []).                  % NO SOLUTION FOUND

star3(G,X,Y,ListaOperadoras,[aresta(X,Y)|OtherSolns], Visited, Soln):-
										append(Visited,[aresta(X,Y)],Soln).


star3(G,X,Y,ListaOperadoras,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams),
	sortSuccDist(Y,Cams, PriorityQueue),
	retira2(PriorityQueue,[],ListaOperadoras,Res), !, 
	star3(G,W,Y,ListaOperadoras,Res, [aresta(Z,W)|Visited], Soln).


%funçaõ auxiliar que retira arestas das operadoras nao desejadas
retira2([A],V,L,Res):-auxAl(A,P), nao(member(P,L)),
							append(V,[A],Res).
retira2([A],V,L,V):-auxAl(A,P), member(P,L).							
retira2([A|S],V,L,Res):-auxAl(A,P), member(P,L), retira2(S,V,L,Res).
retira2([A|S],V,L,Res):-auxAl(A,P),
					    nao(member(P,L)), append(V,[A],LL),
						retira2(S,LL,L,Res).

 %-------- ALINEA 4 - Identificar quais as paragens com o maior numero de carreiras num determinado percurso-----------

%devolve os N primeiros elementos de uma lista
nPrim(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
nPrim(_, [], []).
nPrim(N, [X|Xs], [X|Ys]) :- M is N-1, nPrim(M, Xs, Ys).

%>Numero de carreiras de uma paragem
numCarrP(paragem(_,_,_,_,_,_,_,C,_,_,_),R):- length(C,R).

%ordena lista de paragens pela sua quantidade de carreiras

quick_sort2(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted). 
pivoting(H,[],[],[]).
pivoting(H,[X|T],[X|L],G):-numCarrP(H,HH),numCarrP(X,XX),XX=<HH,pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-numCarrP(H,HH),numCarrP(X,XX),XX>HH,pivoting(H,T,L,G).

%devolve paragem de uma aresta 
pp(aresta(X,_),R):- solucoes(paragem(X,B,A,C,V,L,O,U,G,E,W),paragem(X,B,A,C,V,L,O,U,G,E,W),R).

%devolve paragens de arestas
getParagens(P,R):-getParagens2(P,[],R).
getParagens2([],R,R).
getParagens2([X|XS],Li,R):-append(Li,TTT,ZZ),pp(X,TTT),
						getParagens2(XS,ZZ,R).

%(maior numero carreiras path)- funcao final Depth First
mNCP(G,X,Y,N,R):- dFirst(G,X,Y,P),getParagens(P,L),
				quick_sort2(L,RR),nPrim(N,RR,R).

%(maior numero carreiras path)- funcao final Greedy
mNCP2(G,X,Y,N,R):- greedy(G,X,Y,P),getParagens(P,L),
				quick_sort2(L,RR),nPrim(N,RR,R).

%(maior numero carreiras path)- funcao final A* (estrela)
mNCP3(G,X,Y,N,R):- estrela(G,X,Y,P),getParagens(P,L),
				quick_sort2(L,RR),nPrim(N,RR,R).



%-------- ALINEA 5 - Escolher o percurso com menor numero de paragens-----------

%sucessores a uma aresta
sucessor(grafo(_,Es),Z,R):- findall(aresta(Z,I),member(aresta(Z,I),Es),R).

%Sucessores mas não presentes num histórico
sucessor2(grafo(_,Es),Z,V,R):- setof(aresta(Z,I),(member(aresta(Z,I),Es),\+ member(aresta(Z,I),V)),R).
%Sucessores mas não presentes em dois historicos
sucessor3(grafo(_,Es),Z,V,Re,R):- findall(aresta(Z,I),(member(aresta(Z,I),Es),nao(member(aresta(Z,I),V)),nao(member(aresta(Z,I),Re))),R).

%algortimo greedy já é caracteristico para encontrar o caminho com menor paragens

greedy(G,X,Y,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccessors(Y,PP,PriorityList),
			greedy1(G,X,Y,PriorityList,	         % PRIORITY 
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

greedy1(G,X,Y,[], Visited, []).                  % NO SOLUTION FOUND

greedy1(G,X,Y,[aresta(X,Y)|OtherSolns], Visited, Soln):-
										append(Visited,[aresta(X,Y)],Soln).

greedy1(G,X,Y,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams), %retirar nodos que não tem adjacencia(n Pretendidos)
	sortSuccessors(Y,Cams, PriorityQueue), !, 
	greedy1(G,W,Y,PriorityQueue, [aresta(Z,W)|Visited], Soln).



%Devolve lista de arestas que não são "becos" sem saida

expande_greedy(G,FIM,[],V,V).
expande_greedy(G,FIM,[aresta(X,FIM)],V,ExpCaminhos):- append(V,[aresta(X,FIM)],ExpCaminhos).
expande_greedy(G,FIM,[aresta(X,Y)],V,V):- sucessor(G,Y,P),length(P,L),L=<0.
expande_greedy(G,FIM,[aresta(X,Y)],V,ExpCaminhos):- sucessor(G,Y,P),length(P,L),L>0,
													 append(V,[aresta(X,Y)],ExpCaminhos).
expande_greedy(G,FIM,[aresta(X,Y)|Caminhos],V,ExpCaminhos):- sucessor(G,Y,P),length(P,L),L=<0, nao(Y=FIM),
													 expande_greedy(G,FIM,Caminhos,V,ExpCaminhos).
expande_greedy(G,FIM,[aresta(X,Y)|Caminhos],V,ExpCaminhos):- sucessor(G,Y,P),length(P,L),L>0, 
													 append(V,[aresta(X,Y)],Next),
													 expande_greedy(G,FIM,Caminhos,Next,ExpCaminhos). 
expande_greedy(G,FIM,[aresta(X,FIM)|Caminhos],V,ExpCaminhos):-append(V,[aresta(X,FIM)],Next),
													 expande_greedy(G,FIM,Caminhos,Next,ExpCaminhos).

%ordena lista de paragens pela sua distancia ao Fim
sortSuccessors(Y,List,Sorted):-q_sort2(Y,List,[],Sorted).
q_sort2(Y,[],Acc,Acc).
q_sort2(Y,[H|T],Acc,Sorted):-
	pivoting2(Y,H,T,L1,L2),
	q_sort2(Y,L1,Acc,Sorted1),q_sort2(Y,L2,[H|Sorted1],Sorted).


pivoting2(Y,H,[],[],[]).
pivoting2(Y,H,[X|T],[X|L],G):-pp(aresta(Y,_),YY), devEl(YY,YYY),
							pp(H,P), devEl(P,PP),
							pp(X,P2), devEl(P2,P22),
							distEntreParagens(PP,YYY,XX),
							distEntreParagens(P22,YYY,HH),
							XX=<HH,
							pivoting2(Y,H,T,L,G).

pivoting2(Y,H,[X|T],L,[X|G]):-pp(aresta(Y,_),YY), devEl(YY,YYY),
							pp(H,P), devEl(P,PP),
							pp(X,P2), devEl(P2,P22),
							distEntreParagens(PP,YYY,XX),
							distEntreParagens(P22,YYY,HH),
							XX>HH,
							pivoting2(Y,H,T,L,G).




devolveMenorLista([R],P) :-length(R,P).
devolveMenorLista([R|Rs],G) :-devolveMenorLista(Rs,G), G is menor(length(R,A1),length(head(Rs),A2)).


%devolve todos os caminhos possiveis entre dois pontos (diferentes algoritmos):

allPathsDF(G,X,Y,P):-setof(Caminhos,dFirst(G,X,Y,Caminhos),P).

allPGreedy(G,X,Y,P):-setof(Caminhos,greedy(G,X,Y,Caminhos),P).

allPEstrela(G,X,Y,P):-setof(Caminhos,estrela(G,X,Y,Caminhos),P).



%devolve path com menos numero de paragens
menorPercursoDF(G,X,Y,R) :- findall((P,Q),(dFirst(G,X,Y,P),
								 length(P,Q), Q>0),Tamanhos), 
								devolveP(Tamanhos,B,300000,R).

menorPercursoGreedy(G,X,Y,R) :- findall((P,Q),(greedy(G,X,Y,P),
								 length(P,Q), Q>0),Tamanhos), 
								devolveP(Tamanhos,B,300000,R).
menorPercursoEstrela(G,X,Y,R) :- findall((P,Q),(estrela(G,X,Y,P), 
								length(P,Q), Q>0),Tamanhos),
								 devolveP(Tamanhos,B,300000,R).
 
devolveP([(C,D)],Q,MENOR,C):-D=<MENOR.
devolveP([(C,D)],Q,MENOR,Q):-D>MENOR.
devolveP([(C,D)|X],Q,MENOR,P):-D=<MENOR, devolveP(X,C,D,P).
devolveP([(C,D)|X],Q,MENOR,P):-D>MENOR, devolveP(X,Q,MENOR,P).

%-------- ALINEA 6 - Escolher o percurso com menos distancia-----------

%%através do algortimo A*(estrela) - algoritmo já desenvolvido com este fim

estrela(G,X,Y,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccDist(Y,PP,PriorityList),
			star1(G,X,Y,PriorityList,	         % PRIORITY 
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

star1(G,X,Y,[], Visited, []).                  % NO SOLUTION FOUND

star1(G,X,Y,[aresta(X,Y)|OtherSolns], Visited, Soln):-
					append(Visited,[aresta(X,Y)],Soln).


star1(G,X,Y,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams),
	sortSuccDist(Y,Cams, PriorityQueue), !, 
	star1(G,W,Y,PriorityQueue, [aresta(Z,W)|Visited], Soln).


%ordena lista de paragens pela sua distancia ao Fim + A sua distancia ao nodo seguinte
sortSuccDist(Y,List,Sorted):-q_sort3(Y,List,[],Sorted).
q_sort3(Y,[],Acc,Acc).
q_sort3(Y,[H|T],Acc,Sorted):-
	pivoting3(Y,H,T,L1,L2),
	q_sort3(Y,L1,Acc,Sorted1),q_sort3(Y,L2,[H|Sorted1],Sorted).

pivoting3(Y,H,[],[],[]).
pivoting3(Y,H,[X|T],[X|L],G):-pp(aresta(Y,_),YY), devEl(YY,YYY),
							pp(H,P), devEl(P,PP),
							pp(X,P2), devEl(P2,P22),
							distEntreParagens(PP,YYY,XX),
							distEntreParagens(P22,YYY,HH),
							distEntreParagens(PP,P22,Dist),
							XX+Dist=<HH+Dist,
							pivoting3(Y,H,T,L,G).

pivoting3(Y,H,[X|T],L,[X|G]):-pp(aresta(Y,_),YY), devEl(YY,YYY),
							pp(H,P), devEl(P,PP),
							pp(X,P2), devEl(P2,P22),
							distEntreParagens(PP,YYY,XX),
							distEntreParagens(P22,YYY,HH),
							distEntreParagens(PP,P22,Dist),
							XX+Dist>HH+Dist,
							pivoting3(Y,H,T,L,G).


%%para os restantes algoritmos (Calcular todos os paths possiveis e calcular a distancia para escolher o menor)

resolveDistDF(G,X,Y,P):- findall((P,Res),(dFirst(G,X,Y,P),
							length(P,O),O>0,
							calcDist(P,0,Res)),Tamanhos),
							devolveP(Tamanhos,B,100000000000,P).

resolveDistG(G,X,Y,P):- findall((P,Res),(greedy(G,X,Y,P),
							length(P,O),O>0,
							calcDist(P,0,Res)),Tamanhos),
							devolveP(Tamanhos,B,100000000000,P).

resolveDistE(G,X,Y,P):- findall((P,Q),(estrela(G,X,Y,P),
							length(P,LL),LL>0,
							calcDist(P,0,Q)),Tamanhos),
							devolveP(Tamanhos,B,100000000000,P).


%%Calcula distancia total de um path
calcDist([],Di,Di).
calcDist([A],Di,Di).
calcDist([A|X],Di,Dist):- pp(A,A1), head(X,X2), pp(X2,X22), head(A1,A2), head(X22,X3),
						 distEntreParagens(A2,X3,Dist2),
						 ZZ is Dist2 + Di,
						 calcDist(X,ZZ,Dist).


%-------- ALINEA 7 - Devolve percurso com abrigos com publicidade-----------



%com a depth first 

dFP(G,X,Y,R) :- dFP2(G,X,Y,[X],R).

dFP2(G,X,Y,V,[aresta(X,Y)]) :- adjacente(X,Y,G), pp(aresta(X,Y),P), head(P,H), temPub(H).

dFP2(G,X,Y,V,[aresta(X,Z)|R]) :-
 adjacente(X,Z,G),
 pp(aresta(X,Z),P), 
 head(P,H),
 temPub(H),
 \+ memberchk(aresta(X,Z),V),
 \+ member(Z,V), 
 dFP2(G,Z,Y,[Z|V],R),
 Z \= Y.

 %Com alg Greedy

grePub(G,X,Y,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccessors(Y,PP,PriorityList),	 % PRIORITY 
			retiraComP(PriorityList,[],Res),     % retira da paragens sem publicidade
			greedy4(G,X,Y, Res,	         
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

greedy4(G,X,Y,[], Visited, []).                  % NO SOLUTION FOUND

greedy4(G,X,Y,[aresta(X,Y)|OtherSolns], Visited, Soln):-
										append(Visited,[aresta(X,Y)],Soln).


greedy4(G,X,Y,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams), %retirar nodos que não tem adjacencia se estes não forem os pretendidos
	sortSuccessors(Y,Cams, PriorityQueue),
	retiraComP(PriorityQueue,[],Res), !, 
	greedy4(G,W,Y,Res, [aresta(Z,W)|Visited], Soln).



 %Com alg estrela

estrelaPub(G,X,Y,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccDist(Y,PP,PriorityList),
			retiraComP(PriorityList,[],Dev),        % Retira sem publicidade
			star4(G,X,Y,Dev,	         % PRIORITY 
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

star4(G,X,Y,[], Visited, []).                  % NO SOLUTION FOUND

star4(G,X,Y,[aresta(X,Y)|OtherSolns], Visited, Soln):-append(Visited,[aresta(X,Y)],Soln).


star4(G,X,Y,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams),
	sortSuccDist(Y,Cams, PriorityQueue),
	retiraComP(PriorityQueue,[],Dev), !, 
	star4(G,W,Y,Dev, [aresta(Z,W)|Visited], Soln).


%metodo que diz que passa por abrigo com publicidade(yes) ou nao (no)
temPub(paragem(A,B,C,V,G,'Yes',D,O,S,Y,P)).

%auxiliar retira adjacencias sem publicidade

retiraComP([A],V,Res):-pp(A,P),head(P,PP),temPub(PP),append(V,[A],Res).
retiraComP([A],V,V):-pp(A,P),head(P,PP),nao(temPub(PP)).							
retiraComP([A|S],V,Res):-pp(A,P),head(P,PP),nao(temPub(PP)), retiraComP(S,V,Res).
retiraComP([A|S],V,Res):-pp(A,P),head(P,PP),temPub(PP), append(V,[A],LL),
						retiraComP(S,LL,Res).

 %-------- ALINEA 8 - Devolve percurso abrigado-----------


dFAbrigo(G,X,Y,R) :- dFab(G,X,Y,[X],R).

dFab(G,X,Y,V,[aresta(X,Y)]) :- adjacente(X,Y,G), pp(aresta(X,Y),P), head(P,H), temAbrigo(H).

dFab(G,X,Y,V,[aresta(X,Z)|R]) :-
 adjacente(X,Z,G),
 pp(aresta(X,Z),P), 
 head(P,H),
 temAbrigo(H),
 \+ memberchk(aresta(X,Z),V),
 \+ member(Z,V), 
 dFab(G,Z,Y,[Z|V],R),
 Z \= Y. %maybe unecessary

%Com alg Greedy

greAbrigo(G,X,Y,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccessors(Y,PP,PriorityList),	 % PRIORITY 
			retiraSAb(PriorityList,[],Res),     % retira da paragens sem abrigo
			greedy5(G,X,Y, Res,	         
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

greedy5(G,X,Y,[], Visited, []).                  % NO SOLUTION FOUND

greedy5(G,X,Y,[aresta(X,Y)|OtherSolns], Visited, Soln):-
										append(Visited,[aresta(X,Y)],Soln).


greedy5(G,X,Y,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams), %retirar nodos que não tem adjacencia se estes não forem os pretendidos
	sortSuccessors(Y,Cams, PriorityQueue),
	retiraSAb(PriorityQueue,[],Res), !, 
	greedy5(G,W,Y,Res, [aresta(Z,W)|Visited], Soln).



%com alg Estrela

estrelaAbrigo(G,X,Y,Soln) :-         				     
			sucessor(G,X,PP),
			sortSuccDist(Y,PP,PriorityList),
			retiraSAb(PriorityList,[],Dev),        % Retira sem abrigo
			star5(G,X,Y,Dev,	       		     % PRIORITY 
			[],              					 % NODES VISITED
			Soln).								 % SOLUTION

star5(G,X,Y,[], Visited, []).                  % NO SOLUTION FOUND

star5(G,X,Y,[aresta(X,Y)|OtherSolns], Visited, Soln):-append(Visited,[aresta(X,Y)],Soln).


star5(G,X,Y,[aresta(Z,W)|OtherSolns], Visited, Soln) :-
	sucessor2(G,W,Visited,Successors),
	expande_greedy(G,Y,Successors,[],Cams),
	sortSuccDist(Y,Cams, PriorityQueue),
	retiraSAb(PriorityQueue,[],Dev), !, 
	star5(G,W,Y,Dev, [aresta(Z,W)|Visited], Soln).



%metodo que diz se é abrigado ou nao
temAbrigo(paragem(A,B,C,V,J,F,D,O,S,Y,P)):- nao(J = 'Sem Abrigo'). 

%auxiliar retira adjacencias sem abrigo

retiraSAb([A],V,Res):-pp(A,P),head(P,PP),temAbrigo(PP),
							append(V,[A],Res).
retiraSAb([A],V,V):-pp(A,P),head(P,PP),nao(temAbrigo(PP)).							
retiraSAb([A|S],V,Res):-pp(A,P),head(P,PP),nao(temAbrigo(PP)), retiraSAb(S,V,Res).
retiraSAb([A|S],V,Res):-pp(A,P),head(P,PP),temAbrigo(PP), append(V,[A],LL),
						retiraSAb(S,LL,Res).


%-------- ALINEA 9 - Escolher pontos intermétios num percurso -----------

%com a deapth first 
dFIntermedios(G,X,L,R) :- dFIntermedios2(G,X,L,[],R).

dFIntermedios2(G,X,[L],V,R) :- dFirst(G,X,L,P),concat(V,P,R).
dFIntermedios2(G,X,[L|LS],V,R):-
	dFirst(G,X,L,P),
	concat(V,P,Z),
	dFIntermedios2(G,L,LS,Z,R).

%com alg greedy

greedyIntermedios(G,X,L,R) :- dFIntermedios3(G,X,L,[],R).

dFIntermedios3(G,X,[L],V,R) :- greedy(G,X,L,P),concat(V,P,R).
dFIntermedios3(G,X,[L|LS],V,R):-
	greedy(G,X,L,P),
	concat(V,P,Z),
	dFIntermedios3(G,L,LS,Z,R).

%com alg estrela

estrelaIntermedios(G,X,L,R) :- dFIntermedios4(G,X,L,[],R).

dFIntermedios4(G,X,[L],V,R) :- estrela(G,X,L,P),concat(V,P,R).
dFIntermedios4(G,X,[L|LS],V,R):-
	estrela(G,X,L,P),
	concat(V,P,Z),
	dFIntermedios4(G,L,LS,Z,R).