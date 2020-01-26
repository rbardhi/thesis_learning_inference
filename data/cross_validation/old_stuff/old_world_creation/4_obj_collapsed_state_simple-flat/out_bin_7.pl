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



displ(W,Id) ~ gaussian(Mean,0.038390570050679224) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_19_Temp),avg(X_T_19_Temp)~=X_T_19,shape(W,Id_M)~=Sh_M_1,Sh_M_1==square,getMean([X_M,X_T_19],[-0.057328503155630255,0.07405018526971788,0.0016454632970153635],Mean).
displ(W,Id) ~ gaussian(Mean,0.029336383696403862) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_19_Temp),avg(X_T_19_Temp)~=X_T_19,shape(W,Id_M)~=Sh_M_1,Sh_M_1==triangle,getMean([X_M,X_T_19],[-0.051332029081880545,0.05620414792402001,0.02571026904288921],Mean).
displ(W,Id) ~ gaussian(Mean,0.02645151373183711) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_19_Temp),avg(X_T_19_Temp)~=X_T_19,shape(W,Id_M)~=Sh_M_1,Sh_M_1==circle,getMean([X_M,X_T_19],[-0.054025212173201546,0.06281335961495647,0.0248456194334801],Mean).
displ(W,Id) ~ gaussian(Mean,0.7386925201550127) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_2),maxMod(X_T_2)~=triangle,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_27_Temp),avg(X_T_27_Temp)~=X_T_27,getMean([X_M,X_T_27],[-0.6103355927063434,0.19066698517950964,0.7601483483187905],Mean).
displ(W,Id) ~ gaussian(Mean,0.8590492907123074) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_3),maxMod(X_T_3)~=circle,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_35_Temp),avg(X_T_35_Temp)~=X_T_35,getMean([X_M,X_T_35],[-0.8439204996234412,0.685738580920104,0.10061825206162434],Mean).
displ(W,Id) ~ gaussian(-0.0240695272798,0.0174429073397) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_4),\+maxMod(X_T_4)~=_.
displ(W,Id) ~ gaussian(-0.448192144763,0.782596086791).
posX_t1(W,Id) ~ gaussian(Mean,2.024955728974635e-28) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[1.000000000000001,0.9999999999999992,1.0658141036401503e-14],Mean).
posX_t1(W,Id) ~ gaussian(2.15124554205,1.29332489632).
posY_t1(W,Id) ~ gaussian(Mean,1.0340137169188883e-31) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9999999999999999,4.440892098500626e-16],Mean).
posY_t1(W,Id) ~ gaussian(2.48918912422,1.79592234746).
