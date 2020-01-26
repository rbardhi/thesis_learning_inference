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


n(W) ~ uniform([2,3,4,5]) :=true.
posX_t0(W,I) ~ gaussian(5,5) :=true.
posY_t0(W,I) ~ gaussian(5,5) :=true. 
shape(W,I) ~ finite([0.33:square,0.33:triangle,0.34:circle]) :=true.
left_of(_,_,_) ~ val(_) :=true.



l_o_shape(W,I1,I2) ~ val(Sh) :=
  left_of(W,I1,I2) ~= true,
  shape(W,I2) ~= Sh.
l_o_posX_t0(W,I1,I2) ~ val(X) :=
  left_of(W,I1,I2) ~= true,
  posX_t0(W,I2) ~= X. 
  
mmshl(W,I) ~ val(Sh) :=
  findall_forward(X,(left_of(W,I,Id1)~=true,shape(W,Id1)~=X),L),
  maxMod(L)~=Sh.
  
avglshpos(W,I,Sh) ~ val(XM) :=
  findall_forward(X, (l_o_shape(W,I,I1)~=Sh,l_o_posX_t0(W,I,I1)~=X),L),
  avg(L)~=XM.
  
all_combined(W,I) ~ val(XM) :=
  findall_forward(X,(left_of(W,I,Id1)~=true,shape(W,Id1)~=X),L),
  maxMod(L)~=Sh,
  findall_forward(X, (l_o_shape(W,I,I1)~=Sh,l_o_posX_t0(W,I,I1)~=X),L1),
  avg(L1)~=XM.


displ(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==square.
displ(W,Id) ~ gaussian(Mean,0.8147045162370223) := shape(W,Id)~=Sh_M,Sh_M==triangle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==square,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-0.9578519493119666,0.8222531898777903,-0.18472920813091487],Mean).
displ(W,Id) ~ gaussian(Mean,1.2066059086792194) := shape(W,Id)~=Sh_M,Sh_M==triangle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-0.5427105608691571,0.332260033068774,0.1563001367947887],Mean).
displ(W,Id) ~ gaussian(Mean,0.4573920284566493) := shape(W,Id)~=Sh_M,Sh_M==triangle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-0.5922899820432264,0.42197387948067666,0.6870474685044756],Mean).
displ(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+mmshl(W,Id)~=Sh_M_1.
displ(W,Id) ~ gaussian(Mean,0.42762748653418087) := shape(W,Id)~=Sh_M,Sh_M==circle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==square,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-1.1171690953507998,0.9509403889276163,-0.20841375989597521],Mean).
displ(W,Id) ~ gaussian(Mean,0.5176553042131348) := shape(W,Id)~=Sh_M,Sh_M==circle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-0.844026742316502,0.6899044855722276,-0.42894584499936994],Mean).
displ(W,Id) ~ gaussian(Mean,0.480530025788717) := shape(W,Id)~=Sh_M,Sh_M==circle,mmshl(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,all_combined(W,Id)~=X_M_1,getMean([X_M,X_M_1],[-0.5553165097827354,0.12297520455961118,-0.16245641105029818],Mean).
displ(W,Id) ~ gaussian(Mean,0.01617589680593372) := shape(W,Id)~=Sh_M,Sh_M==circle,\+mmshl(W,Id)~=Sh_M_1,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.034283949451320135,0.030174071448593017],Mean).
displ(W,Id) ~ gaussian(-0.626131288288,1.00904832119) := true.
posX_t1(W,Id) ~ gaussian(Mean,2.6742045338678254e-2) := posX_t0(W,Id)~=X_M,displ(W,Id)~=X_M_1,getMean([X_M,X_M_1],[1.000000000000001,1.0000000000000009,-1.865174681370263e-14],Mean).
posX_t1(W,Id) ~ gaussian(2.18233172502,1.14065361555) := true.
posY_t1(W,Id) ~ gaussian(Mean,2.7791835375961054e-3) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9999999999999997,8.881784197001252e-16],Mean).
posY_t1(W,Id) ~ gaussian(2.47720203767,1.76457477189) := true.




test3 :- 
  init,
  query_for_structure_learning([n(15)~=3,shape(15,0)~=triangle,posX_t0(15,0)~=1.8561159226307185,posY_t0(15,0)~=1.5278495659722418,shape(15,1)~=square,posX_t0(15,1)~=1.3049408887844467,posY_t0(15,1)~=4.419516374631876,shape(15,2)~=triangle,posX_t0(15,2)~=3.6321372049512135,posY_t0(15,2)~=0.6907988598901138,left_of(15,0,1)~=true,left_of(15,2,0)~=true,left_of(15,2,1)~=true,posX_t1(15,0)~=1.8561159226307185,posY_t1(15,0)~=1.5278495659722418,posX_t1(15,1)~=1.3049408887844467,posY_t1(15,1)~=4.419516374631876,posX_t1(15,2)~=0.4808273918245239,posY_t1(15,2)~=0.6907988598901138],[],(findall_forward(X,(between(15,15,W),n(W)~=N,N1 is N-1,between(0,N1,O),displ(W,O)~=X),L)),1,P,L,S,0),
  writeln(S),
  write('P = '),writeln(P).
  
  
  
  
  
  
  
  
 
  
  
  
  
  



