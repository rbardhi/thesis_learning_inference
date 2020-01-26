%%% -*- Mode: Prolog; -*-

:- use_module('../../DC/dcpf.pl').
:- use_module('../../DC/random/sampling.pl').
:- use_module('../../DC/distributionalclause.pl').
:- use_module(library(lists)).
:- set_options(default).
:- initialization(init).

builtin(min_list(_,_)).
builtin(max_list(_,_)).
builtin(lmindc(_,_)).
builtin(lmaxdc(_,_)).
builtin(length(_,_)).
builtin(listavgdc(_,_,_)).
builtin(getMean(_,_,_)).
builtin(logistic(_,_,_)).
builtin(softmax(_,_,_)).
builtin(oneElementOfListDC(_,_)).

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
    Exp is exp(Prod),
    Acc is Acc0 + Exp,
    calExponent(T, X, Acc, Sum, Exps).
calExponent([], _, Sum, Sum, []).

cnt(P) ~ val(Count) := length(P,Count).

pickOneElement(List) ~ uniform(List).
%oneElementOfListDC([H|_], X) :- X = H.
%oneElementOfListDC([H|T], X) :- T=[] -> X = H; oneElementOfListDC(T, X).
%pickOneElement(List) ~ val(X) := oneElementOfListDC(List, X).

lmaxdc(L, M) :- lmaxdc(L, [], [], M).
lmaxdc([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmaxdc(MMax, [], [], Max).
lmaxdc([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmaxdc(T, Seen, [H|MMax], Max); lmaxdc(T, [H|Seen], MMax, Max)).
maxMod(L) ~ val(Max) := lmaxdc(L, Max1), pickOneElement(Max1) ~= Max.

lmindc(L, M) :- lmindc(L, [], [], M).
lmindc([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftoverdc(Seen, MMin, [], Min).
lmindc([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmindc(T, Seen, [H|Left], Min); lmindc(T, [H|Seen], Left, Min)).
leftoverdc([], MMin, TMin, Min) :- TMin=[] -> lmindc(MMin, [], [], Min); Min=TMin, !.
leftoverdc([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftoverdc(Seen, MMin, TMin, Min); leftoverdc(Seen, MMin, [H|TMin], Min).
minMod(L) ~ val(Min) := lmindc(L, Min1), pickOneElement(Min1) ~= Min.

max(L) ~ val(Max) := max_list(L, Max).

min(L) ~ val(Min) := min_list(L, Min).

listavgdc(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(L) ~ val(Avg) := length(L,Cnt), listavgdc(L, Cnt, Avg).

samplesGenerator(Num, Evidence, Query, Var, SampleList) :- findall(Var, genSample(Num, Evidence, Query, World), SampleList).
genSample(Num, Evidence, Query, World) :- between(1, Num, SID), generate_backward(Evidence, Query, World).

n(W) ~ uniform([3,4,5]).

posX_t0(W,I) ~ gaussian(5,5).
posY_t0(W,I) ~ gaussian(5,5). 
shape(W,I) ~ finite([0.33:square,0.33:triangle,0.34:circle]).
left_of(W,I1,I2) ~ val(true) :=
  posX_t0(W,I1) ~= X1,
  posX_t0(W,I2) ~= X2,
  X2 < X1.

displ(W,Id) ~ gaussian(-0.0155034544497,0.0116877617216) := shape(W,Id)~=Sh_M,Sh_M==square.
displ(W,Id) ~ gaussian(-0.420987936054,0.153920477039) := shape(W,Id)~=Sh_M,Sh_M==triangle,findall_forward(Sh_M_1,(left_of(W,Id,Id_M)~=true,shape(W,Id_M)~=Sh_M_1),X_T_21),maxMod(X_T_21)~=square.
displ(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,findall_forward(Sh_M_1,(left_of(W,Id,Id_M)~=true,shape(W,Id_M)~=Sh_M_1),X_T_24),\+maxMod(X_T_24)~=_.
displ(W,Id) ~ gaussian(-0.453804179597,0.149470762582) := shape(W,Id)~=Sh_M,Sh_M==circle,findall_forward(Sh_M_1,(left_of(W,Id,Id_M)~=true,shape(W,Id_M)~=Sh_M_1),X_T_29),maxMod(X_T_29)~=square.
displ(W,Id) ~ gaussian(-0.677621026444,0.0651615784095) := shape(W,Id)~=Sh_M,Sh_M==circle,findall_forward(Sh_M_1,(left_of(W,Id,Id_M)~=true,shape(W,Id_M)~=Sh_M_1),X_T_30),maxMod(X_T_30)~=triangle.
displ(W,Id) ~ gaussian(-0.250824767913,0.132599298906).
posX_t1(W,Id) ~ gaussian(Mean,6.510182572294817e-30) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[1.0000000000000004,0.9999999999999992,-2.220446049250313e-15],Mean).
posX_t1(W,Id) ~ gaussian(2.07456818132,1.03860058829).
posY_t1(W,Id) ~ gaussian(Mean,3.5212991090549993e-31) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9999999999999996,8.881784197001252e-16],Mean).
posY_t1(W,Id) ~ gaussian(2.45345509971,1.7989746033).
