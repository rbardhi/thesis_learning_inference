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
%builtin(oneElementOfList(_,_)).
builtin(length(_,_)).
builtin(listavg(_,_,_)).
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
%oneElementOfList([], []).
%oneElementOfList(List, Elt) :- length(List, Length), random(0, Length, Index), nth0(Index, List, Elt).
pickOneElement(List) ~ uniform(List).

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
%maxMod(X, P) ~ val(Max) := findall_forward(X,P,L), lmax(L, Max1), oneElementOfList(Max1, Max).
maxMod(L) ~ val(Max) := lmax(L, Max1), pickOneElement(Max1) ~= Max.

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, Min) :- lmin(MMin, [], [], Min).
leftover([H|Seen], MMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, Min); Min=[H], !.
%minMod(X, P) ~ val(Min) := findall_forward(X,P,L), lmin(L, Min1), oneElementOfList(Min1,Min).
minMod(L) ~ val(Min) := lmin(L, Min1), pickOneElement(Min1) ~= Min.

%max(X, P) ~ val(Max) := findall_forward(X,P,L), max_list(L, Max).
max(L) ~ val(Max) := max_list(L, Max).

%min(X, P) ~ val(Min) := findall_forward(X,P,L), min_list(L, Min).
min(L) ~ val(Min) := min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
%avg(X, P) ~ val(Avg) := findall_forward(X,P,L), length(L,Cnt), listavg(L, Cnt, Avg).
avg(L) ~ val(Avg) := length(L,Cnt), listavg(L, Cnt, Avg).


