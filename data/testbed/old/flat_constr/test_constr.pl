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
heavy(W,I) ~ val(_) := true.

left_of(_,_,_) ~ val(_) :=true.
l_o_shape(W,I1,I2) ~ val(Sh) :=
  left_of(W,I1,I2) ~= true,
  shape(W,I2) ~= Sh.
l_o_posX_t0(W,I1,I2) ~ val(X) :=
  left_of(W,I1,I2) ~= true,
  posX_t0(W,I2) ~= X. 
  
latleastOne(W,I) ~ val(Sh) :=
  findall_forward(S,(left_of(W,I,I1)~=true,shape(W,I1)~=S),L),member(Sh,L).  
  
ratleastOne(W,I) ~ val(Sh) :=
  findall_forward(S,(left_of(W,I1,I)~=true,heavy(W,I1,true),shape(W,I1)~=S),L),member(Sh,L). 
  
mmshl(W,I) ~ val(Sh) :=
  findall_forward(X,(left_of(W,I,Id1)~=true,shape(W,Id1)~=X),L),
  maxMod(L)~=Sh.
mmshr(W,I) ~ val(Sh) :=
  findall_forward(X,(left_of(W,Id1,I)~=true,shape(W,Id1)~=X),L),
  maxMod(L)~=Sh.
  
avglshpos(W,I,Sh) ~ val(XM) :=
  findall_forward(X, (l_o_shape(W,I,I1)~=Sh,l_o_posX_t0(W,I,I1)~=X),L),
  avg(L)~=XM.
  
avgrshpos(W,I,Sh) ~ val(XM) :=
  findall_forward(X, (l_o_shape(W,I1,I)~=Sh,heavy(W,I1)~=true,l_o_posX_t0(W,I1,I)~=X),L),
  avg(L)~=XM.
  
all_combined(W,I) ~ val(XM) :=
  findall_forward(X,(left_of(W,I,Id1)~=true,shape(W,Id1)~=X),L),
  maxMod(L)~=Sh,
  findall_forward(X, (l_o_shape(W,I,I1)~=Sh,l_o_posX_t0(W,I,I1)~=X),L1),
  avg(L1)~=XM.


displX(W,Id) ~ gaussian(0.0,0.0) := heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==square,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==square,getMean([X_M],[0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.1686631675172591) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==square,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==triangle,getMean([X_M],[-0.112803183092658,0.438114800250603],Mean).
displX(W,Id) ~ gaussian(Mean,0.38020965538662643) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==square,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==circle,getMean([X_M],[-0.34958972660455817,1.278785634912718],Mean).
displX(W,Id) ~ gaussian(Mean,0.7253103970248951) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==square,\+latleastOne(W,Id)~=Sh_M_2,getMean([X_M],[-0.8451640692971285,2.318236707313864],Mean).
displX(W,Id) ~ gaussian(Mean,0.8512109008046777) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==triangle,getMean([X_M],[-0.6322508382003057,2.4747070610907165],Mean).
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==circle,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==square,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.8138251904066727) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==circle,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==triangle,getMean([X_M],[-0.0903681082748701,1.5936230474384112],Mean).
displX(W,Id) ~ gaussian(Mean,0.6766784264650292) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==circle,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==circle,getMean([X_M],[-0.36513475922107247,2.0165679843396225],Mean).
displX(W,Id) ~ gaussian(Mean,0.7724786515266666) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==circle,\+latleastOne(W,Id)~=Sh_M_2,getMean([X_M],[-0.8088437051480033,2.9745219007562658],Mean).
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==square,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,1.0160142623811814) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==triangle,getMean([X_M],[-0.6316348667685283,1.3383225912526782],Mean).
displX(W,Id) ~ gaussian(Mean,0.6010085128818674) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==square,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==square,getMean([X_M],[-0.7392561790506169,0.6803453744001571],Mean).
displX(W,Id) ~ gaussian(Mean,0.42977721791135054) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==square,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==triangle,getMean([X_M],[-0.4584789569350065,-0.08174169274544307],Mean).
displX(W,Id) ~ gaussian(Mean,0.4692379498200781) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==square,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==circle,getMean([X_M],[-0.6654836361029464,0.7473544660789082],Mean).
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==square,\+latleastOne(W,Id)~=Sh_M_2,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.4395006016894876) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==triangle,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==square,getMean([X_M],[-0.6195165337286193,0.42034612442158625],Mean).
displX(W,Id) ~ gaussian(Mean,0.4687175931776329) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==triangle,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==triangle,getMean([X_M],[-0.7536276471159021,0.6221007921260586],Mean).
displX(W,Id) ~ gaussian(Mean,0.5127286103551405) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==triangle,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==circle,getMean([X_M],[-0.6799079881903066,0.7987420612865035],Mean).
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==triangle,\+latleastOne(W,Id)~=Sh_M_2,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.0) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,ratleastOne(W,Id)~=Sh_M_1,Sh_M_1==circle,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.723387676665104) := heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M,Sh_M==circle,\+ratleastOne(W,Id)~=Sh_M_1,getMean([X_M],[-0.684211638729395,0.7100184836543977],Mean).
displX(W,Id) ~ gaussian(-0.196650343836,1.02450487478) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.0024958096597325835) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9990071655238638,0.996602323557095,0.0025208201706039546],Mean).
posX_t1(W,Id) ~ gaussian(2.37873538632,1.34903184679) := true.
posY_t1(W,Id) ~ gaussian(Mean,3.537599920211219e-31) := posY_t0(W,Id)~=X_M,getMean([X_M],[1.0000000000000004,-8.881784197001252e-16],Mean).
posY_t1(W,Id) ~ gaussian(2.49987925977,1.79494162456) := true.
