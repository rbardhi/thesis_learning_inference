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



displ(W,Id) ~ gaussian(Mean,0.060207164008462995) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_19_Temp),avg(X_T_19_Temp)~=X_T_19,shape(W,Id_M)~=Sh_M_1,Sh_M_1==square,getMean([X_M,X_T_19],[-0.07711382633887282,0.0745466678477985,0.04223940557656797],Mean).
displ(W,Id) ~ gaussian(Mean,0.02987100779457531) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_19_Temp),avg(X_T_19_Temp)~=X_T_19,shape(W,Id_M)~=Sh_M_1,Sh_M_1==triangle,getMean([X_M,X_T_19],[-0.04348834921710248,0.0345872819604044,0.040337487280039164],Mean).
displ(W,Id) ~ gaussian(Mean,0.028894868626558597) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_19_Temp),avg(X_T_19_Temp)~=X_T_19,shape(W,Id_M)~=Sh_M_1,Sh_M_1==circle,getMean([X_M,X_T_19],[-0.048023274276876006,0.036433158290197026,0.04967905924094544],Mean).
displ(W,Id) ~ gaussian(Mean,0.9270713327879593) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_2),maxMod(X_T_2)~=triangle,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_27_Temp),avg(X_T_27_Temp)~=X_T_27,getMean([X_M,X_T_27],[-0.8291159064600653,0.40609764927194203,0.7460519496669271],Mean).
displ(W,Id) ~ gaussian(Mean,0.5961382102205182) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_3),maxMod(X_T_3)~=circle,posX_t0(W,Id)~=X_M,findall_forward(X_M_1,(left_of(W,Id,Id_M)~=true,posX_t0(W,Id_M)~=X_M_1),X_T_35_Temp),avg(X_T_35_Temp)~=X_T_35,getMean([X_M,X_T_35],[-0.9569530725170929,0.9094738201342817,-0.3894667286177971],Mean).
displ(W,Id) ~ gaussian(-0.000563479711076,0.000301633915555) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_4),\+maxMod(X_T_4)~=_,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displ(W,Id) ~ gaussian(Mean,0.01085721602211675) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_4),\+maxMod(X_T_4)~=_,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.021222293433450234,0.021733494587986707],Mean).
displ(W,Id) ~ gaussian(Mean,0.06031987522896231) := findall_forward(Sh_M,(left_of(W,Id,Id_M)~=true,shape(W,Id)~=Sh_M),X_T_4),\+maxMod(X_T_4)~=_,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.046066804022207704,-0.02073965322913053],Mean).
displ(W,Id) ~ gaussian(-0.626409132874,1.02275729108).
posX_t1(W,Id) ~ gaussian(Mean,7.115779923845076e-29) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[1.0000000000000007,1.0,-9.769962616701378e-15],Mean).
posX_t1(W,Id) ~ gaussian(2.065577562,1.14169621246).
posY_t1(W,Id) ~ gaussian(Mean,4.148798016268301e-31) := posY_t0(W,Id)~=X_M,getMean([X_M],[1.0000000000000004,-1.3322676295501878e-15],Mean).
posY_t1(W,Id) ~ gaussian(2.50264786693,1.76941288538).
