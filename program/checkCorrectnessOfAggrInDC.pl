%%% -*- Mode: Prolog; -*-
:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').
:- use_module(library(lists)).
:- use_module(library(random)).

:- set_options(default).
:- set_inference(backward(lazy)).
:- initialization(init).

builtin(avg(_,_,_)).
builtin(min_list(_,_)).
builtin(max_list(_,_)).
builtin(lmin(_,_)).
builtin(lmax(_,_)).
builtin(oneElementOfList(_,_)).
builtin(length(_,_)).
builtin(listavg(_,_,_)).
builtin(member(_,_)).
builtin(getMean(_,_,_)).
builtin(logistic(_,_,_)).
builtin(softmax(_,_,_)).

getMean(X,P,Mean) :- dotProd(X,P,Mean).
dotProd([H1|T1], [H2|T2], Prod) :- dotProd(T1, T2, PartProd), Prod is H1*H2 + PartProd.
dotProd([], [H2], Prod) :- Prod is H2.

logistic(P,X,Result) :- dotProd(X,P,Mean), Result is 1/(1 + -1*exp(Mean)).

softmax(P,X,Result) :- calExponent(P,X,Sum,List), normalization(Sum, List, Result).

normalization(Sum, [X|Xs], [Y|Ys]) :- Y is X/Sum, normalization(Sum, Xs, Ys).
normalization(_, [], []).

calExponent([H|T], X, Sum, [Exp|Exps]) :-
    calExponent([H|T], X, 0, Sum, [Exp|Exps]).
calExponent([H|T], X, Acc0, Sum, [Exp|Exps]) :-
    dotProd(X,H,Prod),
    Exp is exp(-1*Prod),
    Acc is Acc0 + Exp,
    calExponent(T, X, Acc, Sum, Exps).
calExponent([], _, Sum, Sum, []).

count(P) ~ val(Count) := findall_forward(1,P,L), length(L,Count).

%oneElementOfList([H|_], X) :- X = H.
oneElementOfList([], []).
oneElementOfList(List, Elt) :- length(List, Length), random(0, Length, Index), nth0(Index, List, Elt).

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
maxMod(X, P) ~ val(Max) := findall_forward(X,P,L), lmax(L, Max1), oneElementOfList(Max1, Max).

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, Min) :- lmin(MMin, [], [], Min).
leftover([H|Seen], MMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, Min); Min=[H], !.
minMod(X, P) ~ val(Min) := findall_forward(X,P,L), lmin(L, Min1), oneElementOfList(Min1,Min).

max(X, P) ~ val(Max) := findall_forward(X,P,L), max_list(L, Max).
min(X, P) ~ val(Min) := findall_forward(X,P,L), min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> A is 0; sum_list(L, Sum), A is Sum / C.
avg(X, P) ~ val(Avg) := findall_forward(X,P,L), length(L,Cnt), listavg(L, Cnt, Avg).

%Facts
rating(C) ~ finite([0.7:high,0.3:low]) := true.
%rating(11) ~ finite([0.7:high,0.3:low]) := true.
%rating(12) ~ finite([0.7:high,0.3:low]) := true.
registration(11,26) := true.
registration(12,26) := true.
registration(13,26) := true.
registration(14,26) := true.

%rel13(116,327) := true.
%marks(florence) ~ finite([0.1:low, 0.2: med, 0.7: high]).
%marks(ann) ~ finite([0.1:low, 0.3: med, 0.6: high]).
%marks(branco) ~ finite([0.3:low, 0.2: med, 0.5: high]).

%inference
test(N) :-
	%query([],[],(maxMod(Du_M,(rel13(A_M,327),dur(A_M)~=Du_M)) ~= dur4),N,P), 
	query([],[],(maxMod(R, (registration(C,26), rating(C)~=R))~=low),N,P),
	%query([],[],(\+rating(11)~=high),N,P), 
	%query([],[],(maxMod(R, rating(C)~=R)~=X, X=low),N,P),  
	write('probability: '),writeln(P).
	%findall_forward(X, stress(X)~=true, Alist),
	%generate_backward(findall_forward(1, stress(X)~=true, L), PossibleWorld), length(L,Count),
	%between(1,N,SampID),
	%proof_query_backward_lazy(SampID, findall_forward([A,X,Y], (smokes(X)~=A, stress(X)~=false, grade(X)~=Y), L)),
        %generate_backward(stress(X)~=true, findall_forward([A], (smokes(X)~=A), L), Alist),
	%generate_backward(stress(X)~=true, findall_forward([A], (smokes(X)~=A), L), Alist),
	%writeln(Count).



