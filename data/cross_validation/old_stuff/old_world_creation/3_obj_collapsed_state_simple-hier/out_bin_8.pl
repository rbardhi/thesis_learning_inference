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



move_left_of(W,Id,Id1) ~ finite([0.058231488138030196:true,0.9417685118619699:false]) := left_of(W,Id,Id1)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==triangle.
move_left_of(W,Id,Id1) ~ finite([0.10015060240963855:true,0.8998493975903614:false]) := left_of(W,Id,Id1)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==circle.
move_left_of(W,Id,Id1) ~ finite([0.19735599622285174:true,0.8026440037771483:false]) := left_of(W,Id,Id1)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==triangle.
move_left_of(W,Id,Id1) ~ finite([0.19624819624819625:false,0.8037518037518038:true]) := \+left_of(W,Id,Id1)~=B_M,shape(W,Id1)~=Sh_M,Sh_M==triangle.
move_left_of(W,Id,Id1) ~ finite([0.28804751299183373:false,0.7119524870081663:true]) := \+left_of(W,Id,Id1)~=B_M,shape(W,Id1)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
move_left_of(W,Id,Id1) ~ finite([0.6230248306997742:true,0.37697516930022573:false]) := \+left_of(W,Id,Id1)~=B_M,shape(W,Id1)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle.
move_left_of(W,Id,Id1) ~ finite([0.43462513582035495:true,0.5653748641796451:false]).
displ(W,Id) ~ gaussian(Mean,0.05518607620025203) := move_left_of(W,Id_M,Id)~=true,true==true,posX_t0(W,Id)~=X_M,posX_t0(W,Id_M)~=X_M_1,getMean([X_M,X_M_1],[-0.9800847779416251,0.9461268396889443,-0.8050863591655579],Mean).
displ(W,Id) ~ gaussian(Mean,0.02716559474228302) := \+move_left_of(W,Id_M,Id)~=true,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.019149316048714848,0.023563660965721333],Mean).
displ(W,Id) ~ gaussian(-0.62609270398,1.02219614421).
posX_t1(W,Id) ~ gaussian(Mean,4.7573105113201114e-29) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[1.0000000000000009,0.9999999999999997,-8.881784197001252e-15],Mean).
posX_t1(W,Id) ~ gaussian(2.06193561743,1.15210373688).
posY_t1(W,Id) ~ gaussian(Mean,1.4597193568919425e-30) := posY_t0(W,Id)~=X_M,getMean([X_M],[1.0000000000000009,-2.220446049250313e-15],Mean).
posY_t1(W,Id) ~ gaussian(2.51036363674,1.76810246039).
