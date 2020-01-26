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



displ(W,Id) ~ gaussian(Mean,0.060401136746502646) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_19_Temp),avg(X_T_19_Temp)~=X_T_19,shape(W,Id_M)~=Sh_M_1,Sh_M_1==square,getMean([X_M,X_T_19],[-0.07817809678563445,0.07456528435344931,0.04505029165709776],Mean).
displ(W,Id) ~ gaussian(Mean,0.031086415571383197) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_19_Temp),avg(X_T_19_Temp)~=X_T_19,shape(W,Id_M)~=Sh_M_1,Sh_M_1==triangle,getMean([X_M,X_T_19],[-0.04522682755487018,0.03567362232638576,0.042536822354926764],Mean).
displ(W,Id) ~ gaussian(Mean,0.030422346087318322) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_19_Temp),avg(X_T_19_Temp)~=X_T_19,shape(W,Id_M)~=Sh_M_1,Sh_M_1==circle,getMean([X_M,X_T_19],[-0.04845818423326283,0.03464605446727467,0.055218342911970225],Mean).
displ(W,Id) ~ gaussian(Mean,0.9293232203008367) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_2),maxMod(X_T_2)~=triangle,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_27_Temp),avg(X_T_27_Temp)~=X_T_27,getMean([X_M,X_T_27],[-0.8214263010951969,0.4025230944280831,0.7351651011822478],Mean).
displ(W,Id) ~ gaussian(Mean,0.6040358045587884) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_3),maxMod(X_T_3)~=circle,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_35_Temp),avg(X_T_35_Temp)~=X_T_35,getMean([X_M,X_T_35],[-0.9496598742277763,0.9065111265572381,-0.40175123200649665],Mean).
displ(W,Id) ~ gaussian(-0.000561705902962,0.000300684385915) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_4),\+maxMod(X_T_4)~=_,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displ(W,Id) ~ gaussian(Mean,0.012532764375510583) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_4),\+maxMod(X_T_4)~=_,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.026259598940195795,0.028672509615380815],Mean).
displ(W,Id) ~ gaussian(Mean,0.06101983326617781) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_4),\+maxMod(X_T_4)~=_,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.03825958649693815,-0.03415663838963118],Mean).
displ(W,Id) ~ gaussian(-0.622093178973,1.01239785151).
posX_t1(W,Id) ~ gaussian(Mean,1.8483992063310815e-29) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9999999999999992,0.9999999999999998,-1.3322676295501878e-15],Mean).
posX_t1(W,Id) ~ gaussian(2.07104851487,1.15077566373).
posY_t1(W,Id) ~ gaussian(Mean,0.0) := posY_t0(W,Id)~=X_M,getMean([X_M],[1.0,0.0],Mean).
posY_t1(W,Id) ~ gaussian(2.50823429683,1.7739781695).
