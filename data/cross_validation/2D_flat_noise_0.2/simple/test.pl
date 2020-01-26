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
builtin((_->_;_)).

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
size(W,I) ~ val(_) :=true.
heavy(W,I) ~ val(_) := true.
left_of(W,I,I1) ~ val(_) :=true.
north_of(W,I,I1) ~ val(_) :=true.
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
  
atleastOneLeft(W,I) ~ val(Sh) :=
  findall_forward(S,(left_of(W,I,I1)~=true,shape(W,I1)~=S),L),member(Sh,L).
  
atleastOneRight(W,I) ~ val(Sh) :=
  findall_forward(S,(left_of(W,I1,I)~=true,heavy(W,I1)~=true,shape(W,I1)~=S),L),member(Sh,L).
  
atleastOneNorth(W,I) ~ val(Sh) :=
  findall_forward(S,(north_of(W,I,I1)~=true,shape(W,I1)~=S),L),member(Sh,L).
  
atleastOneSouth(W,I) ~ val(Sh) :=
  findall_forward(S,(north_of(W,I1,I)~=true,heavy(W,I1)~=true,shape(W,I1)~=S),L),member(Sh,L).

blocked_left(W,I) ~ val(B) :=
  posX_t0(W,I)~=X,posY_t0(W,I)~=Y,size(W,I)~=S,
  findall_forward(I1,(left_of(W,I,I1)~=true,posX_t0(W,I1)~=X1,posY_t0(W,I1)~=Y1,size(W,I1)~=S1,sqrt(((X-0.75) - X1)**2 + ((Y+0) - Y1)**2) =< S + S1),L),
  (member(Q,L) -> B = true ; B = false).

blocked_right(W,I,B) ~ val(B) :=
  posX_t0(W,I)~=X,posY_t0(W,I)~=Y,size(W,I)~=S,
  findall_forward(I1,(left_of(W,I1,I)~=true,posX_t0(W,I1)~=X1,posY_t0(W,I1)~=Y1,size(W,I1)~=S1,sqrt(((X+0.75) - X1)**2 + ((Y+0) - Y1)**2) =< S + S1),L),
  (member(Q,L) -> B = true ; B = false).

blocked_south(W,I) ~ val(B) :=
  posX_t0(W,I)~=X,posY_t0(W,I)~=Y,size(W,I)~=S,
  findall_forward(I1,(north_of(W,I1,I)~=true,posX_t0(W,I1)~=X1,posY_t0(W,I1)~=Y1,size(W,I1)~=S1,sqrt(((X+0) - X1)**2 + ((Y-0.75) - Y1)**2) =< S + S1),L),
  (member(Q,L) -> B = true ; B = false).

blocked_north(W,I) ~ val(B) :=
  posX_t0(W,I)~=X,posY_t0(W,I)~=Y,size(W,I)~=S,
  findall_forward(I1,(north_of(W,I,I1)~=true,posX_t0(W,I1)~=X1,posY_t0(W,I1)~=Y1,size(W,I1)~=S1,sqrt(((X+0) - X1)**2 + ((Y+0.75) - Y1)**2) =< S + S1),L),
  (member(Q,L) -> B = true ; B = false).
  
mlo(W,I) ~ val(S) := findall_forward(B,move_left_of(W,_,I)~=B,L), (member(true,L) -> S = true ; S = false).
mno(W,I) ~ val(S) := findall_forward(B,move_north_of(W,_,I)~=B,L), (member(true,L) -> S = true ; S = false).
mro(W,I) ~ val(S) := findall_forward(B,(move_left_of(W,I,I1)~=B,heavy(W,I1)~=true),L), (member(true,L) -> S = true ; S = false).
mso(W,I) ~ val(S) := findall_forward(B,(move_north_of(W,I,I1)~=B,heavy(W,I1)~=true),L), (member(true,L) -> S = true ; S = false).

/*case(W,I) ~ val(C) :=
  mlo(W,I)~=true,mno(W,I)~=true,
  C = left_north.
case(W,I) ~ val(C) :=
  mlo(W,I)~=true,mno(W,I)~=false,
  C = left.
case(W,I) ~ val(C) :=
  mno(W,I)~=true,mlo(W,I)~=false,
  C = north.*/


displX(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==square.
displX(W,Id) ~ gaussian(-0.00695365967816,0.00142617912622) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneLeft(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displX(W,Id) ~ gaussian(-0.0321877000837,0.015743315846) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneLeft(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==circle.
displX(W,Id) ~ gaussian(-0.0382331132419,0.0187298203347) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneLeft(W,Id)~=Sh_M_1,Sh_M_1==circle,\+atleastOneNorth(W,Id)~=Sh_M_2.
displX(W,Id) ~ gaussian(-0.0478813854381,0.0211459787782) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==square.
displX(W,Id) ~ gaussian(-0.0125450405075,0.00578023397555) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displX(W,Id) ~ gaussian(-0.00173405930712,0.00123143517619) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,atleastOneNorth(W,Id)~=Sh_M_2,Sh_M_2==circle.
displX(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneLeft(W,Id)~=Sh_M_1,\+atleastOneNorth(W,Id)~=Sh_M_2.
displX(W,Id) ~ gaussian(-0.098527308009,0.0680747873964) := true.
displY(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==square.
displY(W,Id) ~ gaussian(0.00617831675192,0.000932300170304) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneNorth(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displY(W,Id) ~ gaussian(0.0282322849668,0.0136604410072) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneNorth(W,Id)~=Sh_M_1,Sh_M_1==circle,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==circle.
displY(W,Id) ~ gaussian(0.0331125962921,0.0180713661207) := shape(W,Id)~=Sh_M,Sh_M==triangle,atleastOneNorth(W,Id)~=Sh_M_1,Sh_M_1==circle,\+atleastOneLeft(W,Id)~=Sh_M_2.
displY(W,Id) ~ gaussian(0.0498941674157,0.0249022212325) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==square.
displY(W,Id) ~ gaussian(0.0054715358256,0.00277292173132) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==triangle.
displY(W,Id) ~ gaussian(0.00363636620948,0.0018096465412) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,atleastOneLeft(W,Id)~=Sh_M_2,Sh_M_2==circle.
displY(W,Id) ~ gaussian(0.0,0.0) := shape(W,Id)~=Sh_M,Sh_M==triangle,\+atleastOneNorth(W,Id)~=Sh_M_1,\+atleastOneLeft(W,Id)~=Sh_M_2.
displY(W,Id) ~ gaussian(0.0960912426084,0.0657730959229) := true.
posX_t1(W,Id) ~ gaussian(Mean,0.17444638592976958) := posX_t0(W,Id)~=X_M,displX(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8733258068902818,0.9275777679041082,0.3063715434830181],Mean).
posX_t1(W,Id) ~ gaussian(2.37601030063,1.39782492093) := true.
posY_t1(W,Id) ~ gaussian(Mean,0.17525969653384704) := posY_t0(W,Id)~=X_M,displY(W,Id)~=X_M_1,getMean([X_M,X_M_1],[0.8759534047972118,0.9307313924253291,0.3178185032793137],Mean).
posY_t1(W,Id) ~ gaussian(2.62643578724,1.42362471712) := true.
