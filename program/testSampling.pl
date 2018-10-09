%%% -*- Mode: Prolog; -*-
:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').
:- use_module(library(lists)).
:- set_options(default).
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

count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

countall(List,X,C) :-
    sort(List,List1),
    member(X,List1),
    count(List,X,C).

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


%samplesGenerator(Num, Evidence, Query, Var, SampleList) :- findall(Sample, genSample(Num, Evidence, Query, Var, Sample), SampleList). 
%genSample(Num, Evidence, Query, Var, Sample) :- between(1, Num, SID), generate_backward(Evidence, Query, World), storeSamples(Var, Query, World, [], Sample).
%storeSamples([H|T], (HQ, TQ), World, Sample, AllSample) :- findall(H, member(HQ, World), S), append(Sample, [S], Sample1), storeSamples(T, TQ, World, Sample1, AllSample).
%storeSamples([T], (TQ), World, Sample, AllSample) :- findall(T, member(TQ, World), S), append(Sample, [S], AllSample), !.
samplesGenerator(Num, Evidence, Query, Var, SampleList) :- findall(Var, genSample(Num, Evidence, Query, World), SampleList).
genSample(Num, Evidence, Query, World) :- between(1, Num, SID), generate_backward(Evidence, Query, World).



%samplesGenerator(Num, BNum, Evidence, Query, EVar, QVar, SampleList) :- 
%genSample(Num, ) :- generate_backward([], Query, World), isValid(Evidence, InstEvidence, [])
samplesGenerator(Num, MaxLoopIter, Evidence, Query, EVar, QVar, World, SampleList) :- findall(Res, genSample(Num, MaxLoopIter, Evidence, Query, EVar, QVar, World, Res), SampleList).
genSample(Num, MaxLoopIter, Evidence, Query, EVar, QVar, World, Res) :- between(1, Num, SID), generateSample(Num, MaxLoopIter, MaxLoopIter, Evidence, Query, EVar, QVar, World, Res). 

generateSample(Num, MaxLoopIter, 0, Evidence, Query, EVar, QVar, World, Res) :- Res=[], !.
generateSample(Num, MaxLoopIter, Iter, Evidence, Query, EVar, QVar, World, Res) :- ((generate_backward([], Query, World), isValid(Evidence, EVar)) -> Res=QVar, !; subtractOne(Iter,Iter1), generateSample(Num, MaxLoopIter, Iter1, Evidence, Query, EVar, QVar, World, Res)).
isValid([], []) :- true.
isValid([H1|Evidence], [H2|InstEvidence]) :- (H1=H2 -> isValid(Evidence, InstEvidence) ; false, !).
subtractOne(X,Y) :- Y is X-1.
%rules
%stress(ann) ~ val(true) := true.
%stress(X) ~ finite([0.000003:true, 0.999997:false]) := person(X).
stress(X) ~ finite([0.3:true, 0.7:false]) := person(X).
rating(X) ~ gaussian(6.5, 1.0) := stress(X)~=true.
rating(X) ~ gaussian(2.5, 1.0) := stress(X)~=false.
smokes(X) ~ finite([P:true, Q:false]) := rating(X)~=Y, logistic([2.0, -9.0], [Y], P), Q is 1-P, write('Q: '), writeln(Q).



%rating(X) ~ gaussian(4.5, 4.0) := person(X).
%smokes(X) ~ finite([0.98:true, 0.02:false]) := rating(X)~=Y, Y>5.0.
%smokes(X) ~ finite([0.5:true, 0.5:false]) := rating(X)~=Y, Y>4.0, Y<5.0.
%smokes(X) ~ finite([0.02:true, 0.98:false]) := rating(X)~=Y, Y<4.0.

%rating(X) ~ finite([0.3:true, 0.7:false]) := person(X).
%smokes(X) ~ finite([0.9:true, 0.1:false]) := rating(X)~=true.
%smokes(X) ~ finite([0.4:true, 0.6:false]) := rating(X)~=false.
person(ann) := true.
person(bob) := true.

smokes(bob, true).
stress(bob, true).
rating(bob, 0).
person(bob).

smokes(ann, false).
stress(ann, true).
rating(ann, 0).
person(ann).

test(N) :-
	init,
	%query([smokes(ann)~=false],[],(rating(ann)~=X, X<4.2),N,P), findall(A,recorded(all_samples,A,_),Samples), eraseall(all_samples), writeln(Samples),
	query([],[],(smokes(ann)~=false,stress(ann)~=false,rating(ann)~=I),N,P),
	write('probability: '),writeln(P).
