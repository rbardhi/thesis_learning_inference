%%% -*- Mode: Prolog; -*-
:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').
:- use_module(library(lists)).

:- set_options(default).
:- set_inference(backward(lazy)).
:- initialization(init).

getMean(X,P,Mean) :- dotProd(X,P,Mean).
dotProd([H1|T1], [H2|T2], Prod) :- dotProd(T1, T2, PartProd), Prod is H1*H2 + PartProd.
dotProd([], [H2], Prod) :- Prod is H2.

count(P,Count) :- generate_backward(findall_forward(1,P,L), PossibleWorld), length(L,Count).

oneElementOfList([H|_], X) :- X = H.

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
maxMod(X, P, Max) :- generate_backward(findall_forward(X,P,L), PossibleWorld), lmax(L, Max1), oneElementOfList(Max1, Max).

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, Min) :- lmin(MMin, [], [], Min).
leftover([H|Seen], MMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, Min); Min=[H], !.
minMod(X, P, Min) :- generate_backward(findall_forward(X,P,L), PossibleWorld), lmin(L, Min1), oneElementOfList(Min1, Min).

max(X, P, Max) :- generate_backward(findall_forward(X,P,L), PossibleWorld), max_list(L, Max).

min(X, P, Min) :- generate_backward(findall_forward(X,P,L), PossibleWorld), min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> A is 0; sum_list(L, Sum), A is Sum / C.
avg(X, P, Avg) :- generate_backward(findall_forward(X,P,L), PossibleWorld), count(P,Cnt), listavg(L, Cnt, Avg).

builtin(avg(_,_,_)).
builtin(min(_,_,_)).
builtin(max(_,_,_)).
builtin(minMod(_,_,_)).
builtin(maxMod(_,_,_)).
builtin(count(_,_)).
builtin(getMean(_,_,_)).

%facts
grade(geo,ann) ~ gaussian(1,0.1).
grade(bio,ann) ~ gaussian(2,0.1).
grade(chem,ann) ~ gaussian(3,0.1).
grade(math,ann) ~ gaussian(4,0.1).
grade(comp,ann) ~ gaussian(5,0.1).
grade(lang,ann) ~ gaussian(6,0.1).
grade(hist,ann) ~ gaussian(7,0.1).


person(ann) := true.
person(bob) := true.
person(carl) := true.
person(rose) := true.
person(john) := true.

stress(ann) ~ val(true).
stress(bob) ~ val(true).
stress(carl) ~ val(true).
stress(rose) ~ finite([0.5:true,0.5:false]).
%stress(X) ~ finite([0.1:true,0.9:false]) := person(X).

smokes(bob) ~ val(true) := true.
smokes(carl) ~ val(true) := true.
smokes(rose) ~ val(false) := true.
smokes(john) ~ val(true) := true.

%grade(ann) ~ val(a) := true.
%grade(bob) ~ val(c) := true.
%grade(carl) ~ val(b) := true.
%grade(rose) ~ val(a) := true.
%grade(X) ~ finite([0.2:a,0.3:b,0.5:c]) := person(X).

%grade(ann) ~ val(80.0) := true.
%grade(bob) ~ val(90.3) := true.
%grade(carl) ~ val(45.9) := true.
%grade(rose) ~ val(87.9) := true.
%grade(X) ~ gaussian(68,1) := person(X).

%background theory

test2(N) :-
	findall(N, test(N), NList).

%inference
test(N) :-
	%query([],[],(stress(X) ~= true),N,P), 
	%write('probability: '),writeln(P).
	%findall_forward(X, stress(X)~=true, Alist),
	generate_backward(findall_forward(1, stress(X)~=true, L), PossibleWorld), length(L,Count),
	%between(1,N,SampID),
	%proof_query_backward_lazy(SampID, findall_forward([A,X,Y], (smokes(X)~=A, stress(X)~=false, grade(X)~=Y), L)),
        %generate_backward(stress(X)~=true, findall_forward([A], (smokes(X)~=A), L), Alist),
	%generate_backward(stress(X)~=true, findall_forward([A], (smokes(X)~=A), L), Alist),
	writeln(Count).


%inference
test1(P) :-
	%query([],[],(stress(X) ~= true),N,P), 
	%write('probability: '),writeln(P).
	%findall_forward(X, stress(X)~=true, Alist),
	generate_backward(findall_forward(1, P, L), PossibleWorld), length(L,Count),
	%between(1,N,SampID),
	%proof_query_backward_lazy(SampID, findall_forward([A,X,Y], (smokes(X)~=A, stress(X)~=false, grade(X)~=Y), L)),
        %generate_backward(stress(X)~=true, findall_forward([A], (smokes(X)~=A), L), Alist),
	%generate_backward(stress(X)~=true, findall_forward([A], (smokes(X)~=A), L), Alist),
	writeln(Count).

