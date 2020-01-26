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
left_of(_,_,_) ~ val(_).

left_of_shape(W,I1,I2)~val(Sh) :=
  left_of(W,I1,I2)~=true,
  shape(W,I2)~=Sh.

shape_posX_t0(W,I,Sh)~val(X) :=
  shape(W,I)~=Sh,
  posX_t0(W,I)~=X. 



move_left_of(W,Id,Id1) ~ finite([0.058613295210864905:true,0.9413867047891351:false]) := left_of(W,Id,Id1)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==triangle.
move_left_of(W,Id,Id1) ~ finite([0.10022438294689603:true,0.8997756170531039:false]) := left_of(W,Id,Id1)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==circle.
move_left_of(W,Id,Id1) ~ finite([0.20864661654135339:true,0.7913533834586466:false]) := left_of(W,Id,Id1)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==triangle.
move_left_of(W,Id,Id1) ~ finite([0.19667389732465654:false,0.8033261026753434:true]) := \+left_of(W,Id,Id1)~=B_M,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==triangle.
move_left_of(W,Id,Id1) ~ finite([0.2860310421286031:false,0.7139689578713969:true]) := \+left_of(W,Id,Id1)~=B_M,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==circle.
move_left_of(W,Id,Id1) ~ finite([0.6171786120591581:true,0.38282138794084186:false]) := \+left_of(W,Id,Id1)~=B_M,shape(W,Id)~=Sh_M,Sh_M==triangle.
move_left_of(W,Id,Id1) ~ finite([0.4340487099107789:true,0.5659512900892211:false]).
displ(W,Id) ~ gaussian(Mean,0.055343789966424695) := move_left_of(W,Id_M,Id)~=true,true==true,posX_t0(W,Id)~=X_M,posX_t0(W,Id_M)~=X_M_1,getMean([X_M,X_M_1],[-0.9796114969139061,0.9452841707623271,-0.8061191622312804],Mean).
displ(W,Id) ~ gaussian(Mean,0.026650282576565133) := \+move_left_of(W,Id_M,Id)~=true,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.018358498514424965,0.021598520480384433],Mean).
displ(W,Id) ~ gaussian(-0.624477349746,1.0182261215).
posX_t1(W,Id) ~ gaussian(Mean,7.02334257005731e-29) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[1.0000000000000002,0.9999999999999994,-8.881784197001252e-15],Mean).
posX_t1(W,Id) ~ gaussian(2.06411317938,1.15146939737).
posY_t1(W,Id) ~ gaussian(Mean,1.362574658652532e-31) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9999999999999998,4.440892098500626e-16],Mean).
posY_t1(W,Id) ~ gaussian(2.50702824937,1.76719247461).
