%%% -*- Mode: Prolog; -*-

%%% FLAT MANUAL RULES SIMPLE SCENARIO


:- use_module('../../DC/dcpf.pl').
:- use_module('../../DC/random/sampling.pl').
:- use_module('../../DC/distributionalclause.pl').
:- use_module(library(lists)).

:- set_options(default),set_inference(backward(lw)),set_query_propagation(true).
:- set_debug(true).

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

n ~ uniform([3,4,5]) := true.

pos_t0(O) ~ gaussian(2.3113736698336846,0.10903887487251766).

shape(O) ~ finite([0.33:square,0.33:triangle,0.34:circle]).
left_of(I1,I2) ~ val(true) :=
  pos_t0(I1) ~= X1,
  pos_t0(I2) ~= X2,
  X2 < X1.
   
displ(Id) ~ gaussian(-0.0242731079349,0.0184643338662) := findall_forward(Sh_M,(left_of(Id,Id_M)~=true,shape(Id)~=Sh_M),X_T_1),maxMod(X_T_1)~=square.
displ(Id) ~ gaussian(-0.444313062679,0.150630524196) := findall_forward(Sh_M,(left_of(Id,Id_M)~=true,shape(Id)~=Sh_M),X_T_2),maxMod(X_T_2)~=triangle.
displ(Id) ~ gaussian(-0.582098983201,0.114195511165) := findall_forward(Sh_M,(left_of(Id,Id_M)~=true,shape(Id)~=Sh_M),X_T_3),maxMod(X_T_3)~=circle.
displ(Id) ~ gaussian(-0.000306626095075,0.000169235211926) := findall_forward(Sh_M,(left_of(Id,Id_M)~=true,shape(Id)~=Sh_M),X_T_4),\+maxMod(X_T_4)~=_.
displ(Id) ~ gaussian(-0.250817573203,0.132712871491).

pos_t1(Id) ~ gaussian(Mean,6.753761806351103e-30) := pos_t0(Id)~=X_M,displ(Id)~=X_M_1,getMean([X_M,X_M_1],[1.0000000000000007,0.9999999999999991,-2.6645352591003757e-15],Mean).
pos_t1(Id) ~ gaussian(2.07708874788,1.02424133076).
  
test :-
  query([shape(0)~=square,pos_t0(0)~=2.1859264030882377,shape(1)~=circle,pos_t0(1)~=2.364785285002114,shape(2)~=triangle,pos_t0(2)~=2.383409321410702,left_of(1,0)~=true,left_of(2,0)~=true,left_of(2,1)~=true,pos_t1(0)~=2.1859264030882377,pos_t1(1)~=2.364785285002114,pos_t1(2)~=1.4041191604918521],[],(findall_forward(X,displ(O)~=X,L)),1,P),
  write('P = '),writeln(P).
  
  
test2 :- 
  init,
  query_for_structure_learning([n~=3,shape(0)~=square,pos_t0(0)~=2.1859264030882377,shape(1)~=circle,pos_t0(1)~=2.364785285002114,shape(2)~=triangle,pos_t0(2)~=2.383409321410702,left_of(1,0)~=true,left_of(2,0)~=true,left_of(2,1)~=true,pos_t1(0)~=2.1859264030882377,pos_t1(1)~=2.364785285002114,pos_t1(2)~=1.4041191604918521],[],(findall_forward(X,(n~=N,N1 is N-1,between(0,N1,O),displ(O)~=X),L)),1,P,L,S,0),
  writeln(S),
  write('P = '),writeln(P).  
  
  
  
