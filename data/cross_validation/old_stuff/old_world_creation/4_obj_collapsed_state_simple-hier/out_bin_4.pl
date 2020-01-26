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



move_left_of(W,Id,Id1) ~ finite([0.04121808674254076:true,0.9587819132574592:false]) := left_of(W,Id,Id1)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==triangle.
move_left_of(W,Id,Id1) ~ finite([0.05596107055961071:true,0.9440389294403893:false]) := left_of(W,Id,Id1)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==circle.
move_left_of(W,Id,Id1) ~ finite([0.11348088531187123:true,0.8865191146881287:false]) := left_of(W,Id,Id1)~=B_M,B_M==true,shape(W,Id)~=Sh_M,Sh_M==triangle.
move_left_of(W,Id,Id1) ~ finite([0.6418485237483954:true,0.3581514762516046:false]) := \+left_of(W,Id,Id1)~=B_M,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==triangle.
move_left_of(W,Id,Id1) ~ finite([0.4646581691772885:false,0.5353418308227115:true]) := \+left_of(W,Id,Id1)~=B_M,shape(W,Id)~=Sh_M,Sh_M==square,shape(W,Id1)~=Sh_M_1,Sh_M_1==circle.
move_left_of(W,Id,Id1) ~ finite([0.38483547925608014:true,0.6151645207439199:false]) := \+left_of(W,Id,Id1)~=B_M,shape(W,Id)~=Sh_M,Sh_M==triangle.
move_left_of(W,Id,Id1) ~ finite([0.23834745762711865:true,0.7616525423728814:false]).
displ(W,Id) ~ gaussian(Mean,0.051829878017121425) := move_left_of(W,Id_M,Id)~=true,true==true,posX_t0(W,Id)~=X_M,posX_t0(W,Id_M)~=X_M_1,getMean([X_M,X_M_1],[-0.9763092713031816,0.9332889454044787,-0.785370870873934],Mean).
displ(W,Id) ~ gaussian(Mean,0.022765862959808217) := \+move_left_of(W,Id_M,Id)~=true,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.014104917466278453,0.014735414793799768],Mean).
displ(W,Id) ~ gaussian(-0.450462507238,0.784588027735).
posX_t1(W,Id) ~ gaussian(Mean,3.1378119677847218e-28) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9999999999999997,1.0000000000000004,1.865174681370263e-14],Mean).
posX_t1(W,Id) ~ gaussian(2.14663591882,1.29580060712).
posY_t1(W,Id) ~ gaussian(Mean,3.2901697435486224e-30) := posY_t0(W,Id)~=X_M,getMean([X_M],[1.0000000000000013,-3.1086244689504383e-15],Mean).
posY_t1(W,Id) ~ gaussian(2.4879684567,1.79340027706).
