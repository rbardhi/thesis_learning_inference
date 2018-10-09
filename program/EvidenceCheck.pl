%%% -*- Mode: Prolog; -*-
:- use_module('../../DCRuleLearning/DC/dcpf.pl').
:- use_module('../../DCRuleLearning/DC/random/sampling.pl').
:- use_module('../../DCRuleLearning/DC/distributionalclause.pl').
:- use_module(library(lists)).
:- set_options(default).
:- set_debug(true).
:- set_inference(backward(lazy)).
:- initialization(init).

builtin(avg(_,_,_)).
builtin(min_list(_,_)).
builtin(max_list(_,_)).
builtin(lmin(_,_)).
builtin(lmax(_,_)).
builtin(length(_,_)).
builtin(listavg(_,_,_)).
builtin(getMean(_,_,_)).
builtin(logistic(_,_,_)).
builtin(softmax(_,_,_)).

getMean(X,P,Mean) :- dotProd(X,P,Mean).
dotProd([H1|T1], [H2|T2], Prod) :- dotProd(T1, T2, PartProd), Prod is H1*H2 + PartProd.
dotProd([], [H2], Prod) :- Prod is H2.

logistic(P,X,Result) :- dotProd(X,P,Mean), Result is 1/(1 + exp(-1*Mean)).

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

pickOneElement(List) ~ uniform(List).

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
maxMod(L) ~ val(Max) := lmax(L, Max1), pickOneElement(Max1) ~= Max.

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, [], Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, TMin, Min) :- TMin=[] -> lmin(MMin, [], [], Min); Min=TMin, !.
leftover([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, TMin, Min); leftover(Seen, MMin, [H|TMin], Min).
minMod(L) ~ val(Min) := lmin(L, Min1), pickOneElement(Min1) ~= Min.

max(L) ~ val(Max) := max_list(L, Max).

min(L) ~ val(Min) := min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(L) ~ val(Avg) := length(L,Cnt), listavg(L, Cnt, Avg).


person(ann) := true.
person(bob) := true.
person(carl) := true.

event1(X) ~ finite([0.1:true, 0.9:false]) := person(X).
event2(X) ~ finite([0.4:true, 0.6:false]) := person(X).

event3(X) ~ finite([0.8:true, 0.2:false]) := event1(X)~=true, event2(X)~=true.
event3(X) ~ finite([0.23:true, 0.77:false]) := event1(X)~=true, event2(X)~=false.
event3(X) ~ finite([0.41:true, 0.59:false]) := event1(X)~=false, event2(X)~=true.


test(N) :-
	init,
	query([event3(ann)~=false, event3(bob)~=false],[],(event1(ann)~=true),N,P),
	write('probability: '),writeln(P).


test1(N) :-
	init,
	query([event3(ann)~=false],[],(event1(ann)~=true),N,P),
	write('probability: '),writeln(P).





