%%% -*- Mode: Prolog; -*-

:- use_module('../../../../DC/dcpf.pl').
:- use_module('../../../../DC/random/sampling.pl').
:- use_module('../../../../DC/distributionalclause.pl').
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


n(W) ~ val(_) :=true.
posX_t0(W,I) ~ val(_) :=true.
posY_t0(W,I) ~ val(_) :=true. 
shape(W,I) ~ val(_) :=true.
heavy(W,I) ~ val(_) := true.
left_of(W,I,I1) ~ val(_) :=true.
l_o_shape(W,I1,I2) ~ val(Sh) :=
  left_of(W,I1,I2) ~= true,
  shape(W,I2) ~= Sh.
l_o_posX_t0(W,I1,I2) ~ val(X) :=
  left_of(W,I1,I2) ~= true,
  posX_t0(W,I2) ~= X. 
  
atleastOne(W,I) ~ val(Sh) :=
  findall_forward(S,(left_of(W,I,I1)~=true,shape(W,I1)~=S),L),member(Sh,L).  
  
latleastOne(W,I) ~ val(Sh) :=
  findall_forward(S,(left_of(W,I,I1)~=true,shape(W,I1)~=S),L),member(Sh,L).
  
ratleastOne(W,I) ~ val(Sh) :=
  findall_forward(S,(left_of(W,I1,I)~=true,heavy(W,I1)~=true,shape(W,I1)~=S),L),member(Sh,L).
  
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
  
almove_left_of(W,I) ~ val(X) :=
  findall_forward(S,(move_left_of(W,I1,I)~=true,posX_t0(W,I1)~=S),L),
  avg(L)~=X.
  
armove_left_of(W,I) ~ val(X) :=
  findall_forward(S,(move_left_of(W,I,I1)~=true,heavy(W,I1)~=true,posX_t0(W,I1)~=S),L),
  avg(L)~=X.


displX(W,Id) ~ gaussian(0.0,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M_1,Sh_M_1==square,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==square,getMean([X_M],[0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.1836321270535627) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M_1,Sh_M_1==square,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==triangle,getMean([X_M],[-0.13884514012488253,0.5187820493432472],Mean).
displX(W,Id) ~ gaussian(Mean,0.40080762722997937) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M_1,Sh_M_1==square,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==circle,getMean([X_M],[-0.4030030220956305,1.4546545640835844],Mean).
displX(W,Id) ~ gaussian(Mean,0.7265900851575011) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M_1,Sh_M_1==square,\+latleastOne(W,Id)~=Sh_M_2,getMean([X_M],[-0.8606898016793468,2.3213067323774004],Mean).
displX(W,Id) ~ gaussian(Mean,0.6042384042950738) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,getMean([X_M],[-0.5409480224959214,0.9646300098098409],Mean).
displX(W,Id) ~ gaussian(Mean,0.5508247460111486) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==square,getMean([X_M],[-0.7596787830525081,0.726871189150194],Mean).
displX(W,Id) ~ gaussian(Mean,0.43738247853645257) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==triangle,getMean([X_M],[-0.463592952239526,-0.06954907213688744],Mean).
displX(W,Id) ~ gaussian(Mean,0.48560015646966237) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==circle,getMean([X_M],[-0.665460022355347,0.7401042861930525],Mean).
displX(W,Id) ~ gaussian(Mean,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==square,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,\+latleastOne(W,Id)~=Sh_M_2,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.8548223195376284) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==square,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.6181787214303377,2.4737227581952936],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(0.0,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,heavy(W,Id)~=B_M,B_M==false,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==square.
displX(W,Id) ~ gaussian(0.0,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,heavy(W,Id)~=B_M,B_M==false,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displX(W,Id) ~ gaussian(Mean,0.2170547201553463) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,heavy(W,Id)~=B_M,B_M==false,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==circle,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.2186874552777248,0.702113728813572],Mean).
displX(W,Id) ~ gaussian(0.471667515701,0.745888858595) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,heavy(W,Id)~=B_M,B_M==false,\+latleastOne(W,Id)~=Sh_M_2.
displX(W,Id) ~ gaussian(0.0,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(Mean,0.4276154046686835) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==square,getMean([X_M],[-0.6663978858260841,0.3410618941175554],Mean).
displX(W,Id) ~ gaussian(Mean,0.41929486207253114) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==triangle,getMean([X_M],[-0.6799370036912508,0.39720210964408387],Mean).
displX(W,Id) ~ gaussian(Mean,0.5058072909528881) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==circle,getMean([X_M],[-0.6917127871675111,0.8051076826661379],Mean).
displX(W,Id) ~ gaussian(Mean,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==triangle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,heavy(W,Id)~=B_M,B_M==false,posX_t0(W,Id)~=X_M,\+latleastOne(W,Id)~=Sh_M_2,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==square,posX_t0(W,Id)~=X_M,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==square,getMean([X_M],[-0.0,0.0],Mean).
displX(W,Id) ~ gaussian(Mean,0.597423299748268) := ratleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==square,posX_t0(W,Id)~=X_M,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==triangle,getMean([X_M],[-0.05656048746411046,1.4781625990875673],Mean).
displX(W,Id) ~ gaussian(Mean,0.6240777885921056) := ratleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==square,posX_t0(W,Id)~=X_M,latleastOne(W,Id)~=Sh_M_2,Sh_M_2==circle,getMean([X_M],[-0.444775703019218,2.23726946596424],Mean).
displX(W,Id) ~ gaussian(Mean,0.7546166004403342) := ratleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==square,posX_t0(W,Id)~=X_M,\+latleastOne(W,Id)~=Sh_M_2,getMean([X_M],[-0.8016234810787068,2.9360981563486455],Mean).
displX(W,Id) ~ gaussian(Mean,0.8771088402119824) := ratleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.6361910274665821,2.497461940660922],Mean).
displX(W,Id) ~ gaussian(0.0,0.0) := ratleastOne(W,Id)~=Sh_M,Sh_M==circle,shape(W,Id)~=Sh_M_1,Sh_M_1==circle.
displX(W,Id) ~ gaussian(0.0,0.0) := \+ratleastOne(W,Id)~=Sh_M,heavy(W,Id)~=B_M,B_M==true.
displX(W,Id) ~ gaussian(0.0,0.0) := \+ratleastOne(W,Id)~=Sh_M,heavy(W,Id)~=B_M,B_M==false,shape(W,Id)~=Sh_M_1,Sh_M_1==square.
displX(W,Id) ~ gaussian(Mean,1.0188214211219446) := \+ratleastOne(W,Id)~=Sh_M,heavy(W,Id)~=B_M,B_M==false,shape(W,Id)~=Sh_M_1,Sh_M_1==triangle,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.5203887135954517,0.8163535898896905],Mean).
displX(W,Id) ~ gaussian(Mean,0.7132919355170552) := \+ratleastOne(W,Id)~=Sh_M,heavy(W,Id)~=B_M,B_M==false,shape(W,Id)~=Sh_M_1,Sh_M_1==circle,posX_t0(W,Id)~=X_M,getMean([X_M],[-0.6774768201286413,0.6915378076079413],Mean).
displX(W,Id) ~ gaussian(-0.192749621006,1.02655563657) := true.
posX_t1(W,Id) ~ gaussian(Mean,6.211111246348661e-18) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.9999999999958252,0.9999999999565148,-3.3301361668236495e-11],Mean).
posX_t1(W,Id) ~ gaussian(2.3754573241,1.34899102631) := true.
posY_t1(W,Id) ~ gaussian(Mean,2.8442717664288795e-31) := posY_t0(W,Id)~=X_M,getMean([X_M],[0.9999999999999997,8.881784197001252e-16],Mean).
posY_t1(W,Id) ~ gaussian(2.50285062704,1.79660130616) := true.
