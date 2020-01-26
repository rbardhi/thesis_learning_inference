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
builtin(good_cube(_,_,_)).
builtin(good_sphere(_,_,_)).
builtin(move_t0(_,_,_)).
builtin(goal(_,_)).

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

good_cube(W,I,true) :- 
  shape(W,I,cube),
  findall(O,(left_of_t0(W,O,I),shape(W,O,cylinder)),L1),
  findall(O,shape(W,O,cylinder),L2),
  length(L1,N),length(L2,N).
good_cube(W,I,false) :- \+good_cube(W,I,true). 

good_sphere(W,I,true) :- 
  shape(W,I,sphere),
  findall(O,(right_of_t0(W,O,I),shape(W,O,cylinder)),L1),
  findall(O,shape(W,O,cylinder),L2),
  length(L1,N),length(L2,N).
good_sphere(W,I,false) :- \+good_sphere(W,I,true). 

move_t0(W,I,left) :- 
  shape(W,I,cube), 
  good_cube(W,I,false).
move_t0(W,I,right) :- 
  shape(W,I,sphere), 
  findall(O,good_cube(W,O,true),L1),
  findall(O,shape(W,O,cube),L2),
  length(L1,N),length(L2,N),
  good_sphere(W,I,false).

goal(W,true) :- 
  \+move_t0(W,_,_),
  findall(I,good_cube(W,I,true),L1),
  findall(I,shape(W,I,cube),L2),
  length(L1,N1),length(L2,N1),
  findall(I,good_sphere(W,I,true),L3),
  findall(I,shape(W,I,sphere),L4),
  length(L3,N2),length(L4,N2).
goal(W,false) :- \+goal(W,true).


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 0
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(0,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(0,0) ~ val(-1.9738246202468872).
posY_t0(0,0) ~ val(2.8072707653045654).
posX_t1(0,0) ~ val(-2.6675119343707867).
posY_t1(0,0) ~ val(2.113583451180666).
right_of_t0(0,0,1) := true.
left_of_t0(0,0,2) := true.
shape(0,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(0,1) ~ val(3.3494784832000732).
posY_t0(0,1) ~ val(-1.7233879566192627).
posX_t1(0,1) ~ val(3.3494784832000732).
posY_t1(0,1) ~ val(-1.7233879566192627).
left_of_t0(0,1,0) := true.
left_of_t0(0,1,2) := true.
shape(0,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(0,2) ~ val(-1.522005319595337).
posY_t0(0,2) ~ val(-2.4900858402252197).
posX_t1(0,2) ~ val(-1.522005319595337).
posY_t1(0,2) ~ val(-2.4900858402252197).
right_of_t0(0,2,0) := true.
right_of_t0(0,2,1) := true.
move_t0(0,0,left) := true.
goal(0) ~ val(false).
good_cube(0,0) ~ val(false).
good_sphere(0,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 1
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(1,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(1,0) ~ val(-2.6675119343707867).
posY_t0(1,0) ~ val(2.113583451180666).
posX_t1(1,0) ~ val(-3.501700857685792).
posY_t1(1,0) ~ val(1.2793945278656604).
right_of_t0(1,0,1) := true.
left_of_t0(1,0,2) := true.
shape(1,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(1,1) ~ val(3.3494784832000732).
posY_t0(1,1) ~ val(-1.7233879566192627).
posX_t1(1,1) ~ val(3.3494784832000732).
posY_t1(1,1) ~ val(-1.7233879566192627).
left_of_t0(1,1,0) := true.
left_of_t0(1,1,2) := true.
shape(1,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(1,2) ~ val(-1.522005319595337).
posY_t0(1,2) ~ val(-2.4900858402252197).
posX_t1(1,2) ~ val(-1.522005319595337).
posY_t1(1,2) ~ val(-2.4900858402252197).
right_of_t0(1,2,0) := true.
right_of_t0(1,2,1) := true.
move_t0(1,0,left) := true.
goal(1) ~ val(false).
good_cube(1,0) ~ val(false).
good_sphere(1,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 2
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(2,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(2,0) ~ val(-3.501700857685792).
posY_t0(2,0) ~ val(1.2793945278656604).
posX_t1(2,0) ~ val(-4.218037133570532).
posY_t1(2,0) ~ val(0.5630582519809206).
right_of_t0(2,0,1) := true.
left_of_t0(2,0,2) := true.
shape(2,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(2,1) ~ val(3.3494784832000732).
posY_t0(2,1) ~ val(-1.7233879566192627).
posX_t1(2,1) ~ val(3.3494784832000732).
posY_t1(2,1) ~ val(-1.7233879566192627).
left_of_t0(2,1,0) := true.
left_of_t0(2,1,2) := true.
shape(2,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(2,2) ~ val(-1.522005319595337).
posY_t0(2,2) ~ val(-2.4900858402252197).
posX_t1(2,2) ~ val(-1.522005319595337).
posY_t1(2,2) ~ val(-2.4900858402252197).
right_of_t0(2,2,0) := true.
right_of_t0(2,2,1) := true.
move_t0(2,0,left) := true.
goal(2) ~ val(false).
good_cube(2,0) ~ val(false).
good_sphere(2,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 3
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(3,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(3,0) ~ val(-4.218037133570532).
posY_t0(3,0) ~ val(0.5630582519809206).
posX_t1(3,0) ~ val(-4.934886723954683).
posY_t1(3,0) ~ val(-0.1537913384032309).
right_of_t0(3,0,1) := true.
left_of_t0(3,0,2) := true.
shape(3,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(3,1) ~ val(3.3494784832000732).
posY_t0(3,1) ~ val(-1.7233879566192627).
posX_t1(3,1) ~ val(3.3494784832000732).
posY_t1(3,1) ~ val(-1.7233879566192627).
left_of_t0(3,1,0) := true.
left_of_t0(3,1,2) := true.
shape(3,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(3,2) ~ val(-1.522005319595337).
posY_t0(3,2) ~ val(-2.4900858402252197).
posX_t1(3,2) ~ val(-1.522005319595337).
posY_t1(3,2) ~ val(-2.4900858402252197).
right_of_t0(3,2,0) := true.
right_of_t0(3,2,1) := true.
move_t0(3,0,left) := true.
goal(3) ~ val(false).
good_cube(3,0) ~ val(false).
good_sphere(3,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 4
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(4,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(4,0) ~ val(-4.566066741943359).
posY_t0(4,0) ~ val(-0.4180573523044586).
posX_t1(4,0) ~ val(-5.37105271250493).
posY_t1(4,0) ~ val(-1.2230433228660291).
right_of_t0(4,0,1) := true.
left_of_t0(4,0,2) := true.
shape(4,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(4,1) ~ val(3.9949753284454346).
posY_t0(4,1) ~ val(-2.4990766048431396).
posX_t1(4,1) ~ val(3.9949753284454346).
posY_t1(4,1) ~ val(-2.4990766048431396).
left_of_t0(4,1,0) := true.
left_of_t0(4,1,2) := true.
shape(4,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(4,2) ~ val(-1.6770718097686768).
posY_t0(4,2) ~ val(-4.8249359130859375).
posX_t1(4,2) ~ val(-1.6770718097686768).
posY_t1(4,2) ~ val(-4.8249359130859375).
right_of_t0(4,2,0) := true.
right_of_t0(4,2,1) := true.
move_t0(4,0,left) := true.
goal(4) ~ val(false).
good_cube(4,0) ~ val(false).
good_sphere(4,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 5
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(5,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(5,0) ~ val(-5.37105271250493).
posY_t0(5,0) ~ val(-1.2230433228660291).
posX_t1(5,0) ~ val(-6.007175742941044).
posY_t1(5,0) ~ val(-1.8591663533021432).
right_of_t0(5,0,1) := true.
shape(5,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(5,1) ~ val(3.9949753284454346).
posY_t0(5,1) ~ val(-2.4990766048431396).
posX_t1(5,1) ~ val(3.9949753284454346).
posY_t1(5,1) ~ val(-2.4990766048431396).
left_of_t0(5,1,0) := true.
left_of_t0(5,1,2) := true.
shape(5,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(5,2) ~ val(-1.6770718097686768).
posY_t0(5,2) ~ val(-4.8249359130859375).
posX_t1(5,2) ~ val(-1.6770718097686768).
posY_t1(5,2) ~ val(-4.8249359130859375).
right_of_t0(5,2,1) := true.
move_t0(5,0,left) := true.
goal(5) ~ val(false).
good_cube(5,0) ~ val(false).
good_sphere(5,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 6
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(6,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(6,0) ~ val(3.867204189300537).
posY_t0(6,0) ~ val(-4.821146011352539).
posX_t1(6,0) ~ val(3.867204189300537).
posY_t1(6,0) ~ val(-4.821146011352539).
right_of_t0(6,0,1) := true.
right_of_t0(6,0,2) := true.
shape(6,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(6,1) ~ val(0.3153548538684845).
posY_t0(6,1) ~ val(0.18131887912750244).
posX_t1(6,1) ~ val(1.0720776923796724).
posY_t1(6,1) ~ val(0.9380417176386905).
right_of_t0(6,1,2) := true.
left_of_t0(6,1,0) := true.
blocked_t0(6,1,behind) := true.
shape(6,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(6,2) ~ val(-0.7300168871879578).
posY_t0(6,2) ~ val(1.7576556205749512).
posX_t1(6,2) ~ val(-0.7300168871879578).
posY_t1(6,2) ~ val(1.7576556205749512).
left_of_t0(6,2,0) := true.
left_of_t0(6,2,1) := true.
blocked_t0(6,2,front) := true.
move_t0(6,1,right) := true.
goal(6) ~ val(false).
good_cube(6,0) ~ val(true).
good_sphere(6,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 7
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(7,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(7,0) ~ val(-3.679595470428467).
posY_t0(7,0) ~ val(-3.9106414318084717).
posX_t1(7,0) ~ val(-3.679595470428467).
posY_t1(7,0) ~ val(-3.9106414318084717).
right_of_t0(7,0,1) := true.
right_of_t0(7,0,2) := true.
shape(7,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(7,1) ~ val(4.525230884552002).
posY_t0(7,1) ~ val(-3.7449662685394287).
posX_t1(7,1) ~ val(4.525230884552002).
posY_t1(7,1) ~ val(-3.7449662685394287).
left_of_t0(7,1,0) := true.
left_of_t0(7,1,2) := true.
shape(7,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(7,2) ~ val(-2.413264036178589).
posY_t0(7,2) ~ val(0.277258962392807).
posX_t1(7,2) ~ val(-2.413264036178589).
posY_t1(7,2) ~ val(0.277258962392807).
right_of_t0(7,2,1) := true.
left_of_t0(7,2,0) := true.
goal(7) ~ val(true).
good_cube(7,0) ~ val(true).
good_sphere(7,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 8
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(8,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(8,0) ~ val(4.667672634124756).
posY_t0(8,0) ~ val(-2.2729482650756836).
posX_t1(8,0) ~ val(3.9236836953805936).
posY_t1(8,0) ~ val(-3.016937203819846).
right_of_t0(8,0,1) := true.
left_of_t0(8,0,2) := true.
shape(8,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(8,1) ~ val(1.5577178001403809).
posY_t0(8,1) ~ val(4.958014011383057).
posX_t1(8,1) ~ val(1.5577178001403809).
posY_t1(8,1) ~ val(4.958014011383057).
left_of_t0(8,1,0) := true.
left_of_t0(8,1,2) := true.
shape(8,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(8,2) ~ val(3.2543301582336426).
posY_t0(8,2) ~ val(-5.493769645690918).
posX_t1(8,2) ~ val(3.2543301582336426).
posY_t1(8,2) ~ val(-5.493769645690918).
right_of_t0(8,2,0) := true.
right_of_t0(8,2,1) := true.
move_t0(8,0,left) := true.
goal(8) ~ val(false).
good_cube(8,0) ~ val(false).
good_sphere(8,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 9
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(9,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(9,0) ~ val(3.9236836953805936).
posY_t0(9,0) ~ val(-3.016937203819846).
posX_t1(9,0) ~ val(3.302955329032961).
posY_t1(9,0) ~ val(-3.6376655701674783).
right_of_t0(9,0,1) := true.
left_of_t0(9,0,2) := true.
shape(9,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(9,1) ~ val(1.5577178001403809).
posY_t0(9,1) ~ val(4.958014011383057).
posX_t1(9,1) ~ val(1.5577178001403809).
posY_t1(9,1) ~ val(4.958014011383057).
left_of_t0(9,1,0) := true.
left_of_t0(9,1,2) := true.
shape(9,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(9,2) ~ val(3.2543301582336426).
posY_t0(9,2) ~ val(-5.493769645690918).
posX_t1(9,2) ~ val(3.2543301582336426).
posY_t1(9,2) ~ val(-5.493769645690918).
right_of_t0(9,2,0) := true.
right_of_t0(9,2,1) := true.
move_t0(9,0,left) := true.
goal(9) ~ val(false).
good_cube(9,0) ~ val(false).
good_sphere(9,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 10
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(10,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(10,0) ~ val(3.302955329032961).
posY_t0(10,0) ~ val(-3.6376655701674783).
posX_t1(10,0) ~ val(2.4807311499788343).
posY_t1(10,0) ~ val(-4.459889749221605).
right_of_t0(10,0,1) := true.
left_of_t0(10,0,2) := true.
shape(10,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(10,1) ~ val(1.5577178001403809).
posY_t0(10,1) ~ val(4.958014011383057).
posX_t1(10,1) ~ val(1.5577178001403809).
posY_t1(10,1) ~ val(4.958014011383057).
left_of_t0(10,1,0) := true.
left_of_t0(10,1,2) := true.
shape(10,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(10,2) ~ val(3.2543301582336426).
posY_t0(10,2) ~ val(-5.493769645690918).
posX_t1(10,2) ~ val(3.2543301582336426).
posY_t1(10,2) ~ val(-5.493769645690918).
right_of_t0(10,2,0) := true.
right_of_t0(10,2,1) := true.
move_t0(10,0,left) := true.
goal(10) ~ val(false).
good_cube(10,0) ~ val(false).
good_sphere(10,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 11
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(11,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(11,0) ~ val(2.4807311499788343).
posY_t0(11,0) ~ val(-4.459889749221605).
posX_t1(11,0) ~ val(1.7704023176977217).
posY_t1(11,0) ~ val(-5.170218581502717).
right_of_t0(11,0,1) := true.
blocked_t0(11,0,front) := true.
shape(11,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(11,1) ~ val(1.5577178001403809).
posY_t0(11,1) ~ val(4.958014011383057).
posX_t1(11,1) ~ val(1.5577178001403809).
posY_t1(11,1) ~ val(4.958014011383057).
left_of_t0(11,1,0) := true.
left_of_t0(11,1,2) := true.
shape(11,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(11,2) ~ val(3.2543301582336426).
posY_t0(11,2) ~ val(-5.493769645690918).
posX_t1(11,2) ~ val(3.2543301582336426).
posY_t1(11,2) ~ val(-5.493769645690918).
right_of_t0(11,2,1) := true.
blocked_t0(11,2,behind) := true.
move_t0(11,0,left) := true.
goal(11) ~ val(false).
good_cube(11,0) ~ val(false).
good_sphere(11,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 12
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(12,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(12,0) ~ val(1.7970571517944336).
posY_t0(12,0) ~ val(-5.405144214630127).
posX_t1(12,0) ~ val(1.117260393141399).
posY_t1(12,0) ~ val(-6.084940973283162).
left_of_t0(12,0,1) := true.
left_of_t0(12,0,2) := true.
shape(12,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(12,1) ~ val(-5.034622669219971).
posY_t0(12,1) ~ val(-2.8447213172912598).
posX_t1(12,1) ~ val(-5.034622669219971).
posY_t1(12,1) ~ val(-2.8447213172912598).
right_of_t0(12,1,0) := true.
right_of_t0(12,1,2) := true.
shape(12,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(12,2) ~ val(-0.0822870135307312).
posY_t0(12,2) ~ val(-4.427548408508301).
posX_t1(12,2) ~ val(-0.0822870135307312).
posY_t1(12,2) ~ val(-4.427548408508301).
right_of_t0(12,2,0) := true.
left_of_t0(12,2,1) := true.
move_t0(12,0,left) := true.
goal(12) ~ val(false).
good_cube(12,0) ~ val(false).
good_sphere(12,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 13
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(13,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(13,0) ~ val(1.117260393141399).
posY_t0(13,0) ~ val(-6.084940973283162).
posX_t1(13,0) ~ val(1.117260393141399).
posY_t1(13,0) ~ val(-6.084940973283162).
right_of_t0(13,0,2) := true.
left_of_t0(13,0,1) := true.
shape(13,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(13,1) ~ val(-5.034622669219971).
posY_t0(13,1) ~ val(-2.8447213172912598).
posX_t1(13,1) ~ val(-4.314645572817599).
posY_t1(13,1) ~ val(-2.1247442208888883).
right_of_t0(13,1,0) := true.
right_of_t0(13,1,2) := true.
shape(13,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(13,2) ~ val(-0.0822870135307312).
posY_t0(13,2) ~ val(-4.427548408508301).
posX_t1(13,2) ~ val(-0.0822870135307312).
posY_t1(13,2) ~ val(-4.427548408508301).
left_of_t0(13,2,0) := true.
left_of_t0(13,2,1) := true.
move_t0(13,1,right) := true.
goal(13) ~ val(false).
good_cube(13,0) ~ val(true).
good_sphere(13,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 14
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(14,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(14,0) ~ val(1.117260393141399).
posY_t0(14,0) ~ val(-6.084940973283162).
posX_t1(14,0) ~ val(1.117260393141399).
posY_t1(14,0) ~ val(-6.084940973283162).
right_of_t0(14,0,2) := true.
left_of_t0(14,0,1) := true.
shape(14,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(14,1) ~ val(-4.314645572817599).
posY_t0(14,1) ~ val(-2.1247442208888883).
posX_t1(14,1) ~ val(-3.6336627534808055).
posY_t1(14,1) ~ val(-1.4437614015520945).
right_of_t0(14,1,0) := true.
right_of_t0(14,1,2) := true.
shape(14,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(14,2) ~ val(-0.0822870135307312).
posY_t0(14,2) ~ val(-4.427548408508301).
posX_t1(14,2) ~ val(-0.0822870135307312).
posY_t1(14,2) ~ val(-4.427548408508301).
left_of_t0(14,2,0) := true.
left_of_t0(14,2,1) := true.
move_t0(14,1,right) := true.
goal(14) ~ val(false).
good_cube(14,0) ~ val(true).
good_sphere(14,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 15
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(15,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(15,0) ~ val(1.117260393141399).
posY_t0(15,0) ~ val(-6.084940973283162).
posX_t1(15,0) ~ val(1.117260393141399).
posY_t1(15,0) ~ val(-6.084940973283162).
right_of_t0(15,0,2) := true.
shape(15,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(15,1) ~ val(-3.6336627534808055).
posY_t0(15,1) ~ val(-1.4437614015520945).
posX_t1(15,1) ~ val(-2.8015021464707406).
posY_t1(15,1) ~ val(-0.6116007945420296).
right_of_t0(15,1,2) := true.
shape(15,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(15,2) ~ val(-0.0822870135307312).
posY_t0(15,2) ~ val(-4.427548408508301).
posX_t1(15,2) ~ val(-0.0822870135307312).
posY_t1(15,2) ~ val(-4.427548408508301).
left_of_t0(15,2,0) := true.
left_of_t0(15,2,1) := true.
move_t0(15,1,right) := true.
goal(15) ~ val(false).
good_cube(15,0) ~ val(true).
good_sphere(15,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 16
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(16,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(16,0) ~ val(-1.5041167736053467).
posY_t0(16,0) ~ val(-1.8112951517105103).
posX_t1(16,0) ~ val(-2.3007414242106607).
posY_t1(16,0) ~ val(-2.6079198023158243).
right_of_t0(16,0,1) := true.
left_of_t0(16,0,2) := true.
shape(16,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(16,1) ~ val(1.0979212522506714).
posY_t0(16,1) ~ val(4.275920391082764).
posX_t1(16,1) ~ val(1.0979212522506714).
posY_t1(16,1) ~ val(4.275920391082764).
left_of_t0(16,1,0) := true.
left_of_t0(16,1,2) := true.
shape(16,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(16,2) ~ val(-1.8191182613372803).
posY_t0(16,2) ~ val(-5.212826728820801).
posX_t1(16,2) ~ val(-1.8191182613372803).
posY_t1(16,2) ~ val(-5.212826728820801).
right_of_t0(16,2,0) := true.
right_of_t0(16,2,1) := true.
move_t0(16,0,left) := true.
goal(16) ~ val(false).
good_cube(16,0) ~ val(false).
good_sphere(16,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 17
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(17,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(17,0) ~ val(-2.3007414242106607).
posY_t0(17,0) ~ val(-2.6079198023158243).
posX_t1(17,0) ~ val(-3.08334974792738).
posY_t1(17,0) ~ val(-3.3905281260325437).
right_of_t0(17,0,1) := true.
left_of_t0(17,0,2) := true.
shape(17,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(17,1) ~ val(1.0979212522506714).
posY_t0(17,1) ~ val(4.275920391082764).
posX_t1(17,1) ~ val(1.0979212522506714).
posY_t1(17,1) ~ val(4.275920391082764).
left_of_t0(17,1,0) := true.
left_of_t0(17,1,2) := true.
shape(17,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(17,2) ~ val(-1.8191182613372803).
posY_t0(17,2) ~ val(-5.212826728820801).
posX_t1(17,2) ~ val(-1.8191182613372803).
posY_t1(17,2) ~ val(-5.212826728820801).
right_of_t0(17,2,0) := true.
right_of_t0(17,2,1) := true.
move_t0(17,0,left) := true.
goal(17) ~ val(false).
good_cube(17,0) ~ val(false).
good_sphere(17,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 18
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(18,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(18,0) ~ val(-3.08334974792738).
posY_t0(18,0) ~ val(-3.3905281260325437).
posX_t1(18,0) ~ val(-3.6601262572636464).
posY_t1(18,0) ~ val(-3.96730463536881).
right_of_t0(18,0,1) := true.
left_of_t0(18,0,2) := true.
shape(18,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(18,1) ~ val(1.0979212522506714).
posY_t0(18,1) ~ val(4.275920391082764).
posX_t1(18,1) ~ val(1.0979212522506714).
posY_t1(18,1) ~ val(4.275920391082764).
left_of_t0(18,1,0) := true.
left_of_t0(18,1,2) := true.
shape(18,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(18,2) ~ val(-1.8191182613372803).
posY_t0(18,2) ~ val(-5.212826728820801).
posX_t1(18,2) ~ val(-1.8191182613372803).
posY_t1(18,2) ~ val(-5.212826728820801).
right_of_t0(18,2,0) := true.
right_of_t0(18,2,1) := true.
move_t0(18,0,left) := true.
goal(18) ~ val(false).
good_cube(18,0) ~ val(false).
good_sphere(18,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 19
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(19,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(19,0) ~ val(-3.6975746154785156).
posY_t0(19,0) ~ val(-2.7971646785736084).
posX_t1(19,0) ~ val(-3.6975746154785156).
posY_t1(19,0) ~ val(-2.7971646785736084).
right_of_t0(19,0,1) := true.
right_of_t0(19,0,2) := true.
shape(19,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(19,1) ~ val(3.341526746749878).
posY_t0(19,1) ~ val(-1.3718339204788208).
posX_t1(19,1) ~ val(3.953507297549258).
posY_t1(19,1) ~ val(-0.7598533696794408).
right_of_t0(19,1,2) := true.
left_of_t0(19,1,0) := true.
shape(19,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(19,2) ~ val(0.4315657615661621).
posY_t0(19,2) ~ val(5.36394739151001).
posX_t1(19,2) ~ val(0.4315657615661621).
posY_t1(19,2) ~ val(5.36394739151001).
left_of_t0(19,2,0) := true.
left_of_t0(19,2,1) := true.
move_t0(19,1,right) := true.
goal(19) ~ val(false).
good_cube(19,0) ~ val(true).
good_sphere(19,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 20
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(20,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(20,0) ~ val(-3.6975746154785156).
posY_t0(20,0) ~ val(-2.7971646785736084).
posX_t1(20,0) ~ val(-3.6975746154785156).
posY_t1(20,0) ~ val(-2.7971646785736084).
right_of_t0(20,0,1) := true.
right_of_t0(20,0,2) := true.
shape(20,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(20,1) ~ val(3.953507297549258).
posY_t0(20,1) ~ val(-0.7598533696794408).
posX_t1(20,1) ~ val(4.535929576321512).
posY_t1(20,1) ~ val(-0.17743109090718678).
right_of_t0(20,1,2) := true.
left_of_t0(20,1,0) := true.
shape(20,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(20,2) ~ val(0.4315657615661621).
posY_t0(20,2) ~ val(5.36394739151001).
posX_t1(20,2) ~ val(0.4315657615661621).
posY_t1(20,2) ~ val(5.36394739151001).
left_of_t0(20,2,0) := true.
left_of_t0(20,2,1) := true.
move_t0(20,1,right) := true.
goal(20) ~ val(false).
good_cube(20,0) ~ val(true).
good_sphere(20,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 21
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(21,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(21,0) ~ val(-3.6975746154785156).
posY_t0(21,0) ~ val(-2.7971646785736084).
posX_t1(21,0) ~ val(-3.6975746154785156).
posY_t1(21,0) ~ val(-2.7971646785736084).
right_of_t0(21,0,1) := true.
right_of_t0(21,0,2) := true.
shape(21,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(21,1) ~ val(4.535929576321512).
posY_t0(21,1) ~ val(-0.17743109090718678).
posX_t1(21,1) ~ val(5.369906950841497).
posY_t1(21,1) ~ val(0.6565462836127991).
right_of_t0(21,1,2) := true.
left_of_t0(21,1,0) := true.
shape(21,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(21,2) ~ val(0.4315657615661621).
posY_t0(21,2) ~ val(5.36394739151001).
posX_t1(21,2) ~ val(0.4315657615661621).
posY_t1(21,2) ~ val(5.36394739151001).
left_of_t0(21,2,0) := true.
left_of_t0(21,2,1) := true.
move_t0(21,1,right) := true.
goal(21) ~ val(false).
good_cube(21,0) ~ val(true).
good_sphere(21,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 22
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(22,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(22,0) ~ val(-3.6975746154785156).
posY_t0(22,0) ~ val(-2.7971646785736084).
posX_t1(22,0) ~ val(-3.6975746154785156).
posY_t1(22,0) ~ val(-2.7971646785736084).
right_of_t0(22,0,1) := true.
right_of_t0(22,0,2) := true.
shape(22,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(22,1) ~ val(5.369906950841497).
posY_t0(22,1) ~ val(0.6565462836127991).
posX_t1(22,1) ~ val(6.049271943583956).
posY_t1(22,1) ~ val(1.3359112763552574).
left_of_t0(22,1,0) := true.
shape(22,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(22,2) ~ val(0.4315657615661621).
posY_t0(22,2) ~ val(5.36394739151001).
posX_t1(22,2) ~ val(0.4315657615661621).
posY_t1(22,2) ~ val(5.36394739151001).
left_of_t0(22,2,0) := true.
move_t0(22,1,right) := true.
goal(22) ~ val(false).
good_cube(22,0) ~ val(true).
good_sphere(22,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 23
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(23,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(23,0) ~ val(1.2789599895477295).
posY_t0(23,0) ~ val(4.138781547546387).
posX_t1(23,0) ~ val(0.569132312235315).
posY_t1(23,0) ~ val(3.4289538702339724).
left_of_t0(23,0,1) := true.
left_of_t0(23,0,2) := true.
shape(23,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(23,1) ~ val(4.1493096351623535).
posY_t0(23,1) ~ val(-0.9525533318519592).
posX_t1(23,1) ~ val(4.1493096351623535).
posY_t1(23,1) ~ val(-0.9525533318519592).
right_of_t0(23,1,0) := true.
left_of_t0(23,1,2) := true.
shape(23,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(23,2) ~ val(-3.693817138671875).
posY_t0(23,2) ~ val(2.290310859680176).
posX_t1(23,2) ~ val(-3.693817138671875).
posY_t1(23,2) ~ val(2.290310859680176).
right_of_t0(23,2,0) := true.
right_of_t0(23,2,1) := true.
move_t0(23,0,left) := true.
goal(23) ~ val(false).
good_cube(23,0) ~ val(false).
good_sphere(23,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 24
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(24,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(24,0) ~ val(0.569132312235315).
posY_t0(24,0) ~ val(3.4289538702339724).
posX_t1(24,0) ~ val(-0.046505253216333364).
posY_t1(24,0) ~ val(2.813316304782324).
left_of_t0(24,0,1) := true.
left_of_t0(24,0,2) := true.
shape(24,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(24,1) ~ val(4.1493096351623535).
posY_t0(24,1) ~ val(-0.9525533318519592).
posX_t1(24,1) ~ val(4.1493096351623535).
posY_t1(24,1) ~ val(-0.9525533318519592).
right_of_t0(24,1,0) := true.
left_of_t0(24,1,2) := true.
shape(24,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(24,2) ~ val(-3.693817138671875).
posY_t0(24,2) ~ val(2.290310859680176).
posX_t1(24,2) ~ val(-3.693817138671875).
posY_t1(24,2) ~ val(2.290310859680176).
right_of_t0(24,2,0) := true.
right_of_t0(24,2,1) := true.
move_t0(24,0,left) := true.
goal(24) ~ val(false).
good_cube(24,0) ~ val(false).
good_sphere(24,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 25
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(25,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(25,0) ~ val(-0.046505253216333364).
posY_t0(25,0) ~ val(2.813316304782324).
posX_t1(25,0) ~ val(-0.7889260184545288).
posY_t1(25,0) ~ val(2.0708955395441286).
right_of_t0(25,0,1) := true.
left_of_t0(25,0,2) := true.
shape(25,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(25,1) ~ val(4.1493096351623535).
posY_t0(25,1) ~ val(-0.9525533318519592).
posX_t1(25,1) ~ val(4.1493096351623535).
posY_t1(25,1) ~ val(-0.9525533318519592).
left_of_t0(25,1,0) := true.
left_of_t0(25,1,2) := true.
shape(25,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(25,2) ~ val(-3.693817138671875).
posY_t0(25,2) ~ val(2.290310859680176).
posX_t1(25,2) ~ val(-3.693817138671875).
posY_t1(25,2) ~ val(2.290310859680176).
right_of_t0(25,2,0) := true.
right_of_t0(25,2,1) := true.
move_t0(25,0,left) := true.
goal(25) ~ val(false).
good_cube(25,0) ~ val(false).
good_sphere(25,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 26
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(26,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(26,0) ~ val(-0.7889260184545288).
posY_t0(26,0) ~ val(2.0708955395441286).
posX_t1(26,0) ~ val(-1.4034794385534486).
posY_t1(26,0) ~ val(1.456342119445209).
right_of_t0(26,0,1) := true.
left_of_t0(26,0,2) := true.
shape(26,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(26,1) ~ val(4.1493096351623535).
posY_t0(26,1) ~ val(-0.9525533318519592).
posX_t1(26,1) ~ val(4.1493096351623535).
posY_t1(26,1) ~ val(-0.9525533318519592).
left_of_t0(26,1,0) := true.
left_of_t0(26,1,2) := true.
shape(26,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(26,2) ~ val(-3.693817138671875).
posY_t0(26,2) ~ val(2.290310859680176).
posX_t1(26,2) ~ val(-3.693817138671875).
posY_t1(26,2) ~ val(2.290310859680176).
right_of_t0(26,2,0) := true.
right_of_t0(26,2,1) := true.
move_t0(26,0,left) := true.
goal(26) ~ val(false).
good_cube(26,0) ~ val(false).
good_sphere(26,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 27
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(27,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(27,0) ~ val(-1.4034794385534486).
posY_t0(27,0) ~ val(1.456342119445209).
posX_t1(27,0) ~ val(-2.2096272921465547).
posY_t1(27,0) ~ val(0.6501942658521029).
right_of_t0(27,0,1) := true.
left_of_t0(27,0,2) := true.
shape(27,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(27,1) ~ val(4.1493096351623535).
posY_t0(27,1) ~ val(-0.9525533318519592).
posX_t1(27,1) ~ val(4.1493096351623535).
posY_t1(27,1) ~ val(-0.9525533318519592).
left_of_t0(27,1,0) := true.
left_of_t0(27,1,2) := true.
shape(27,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(27,2) ~ val(-3.693817138671875).
posY_t0(27,2) ~ val(2.290310859680176).
posX_t1(27,2) ~ val(-3.693817138671875).
posY_t1(27,2) ~ val(2.290310859680176).
right_of_t0(27,2,0) := true.
right_of_t0(27,2,1) := true.
move_t0(27,0,left) := true.
goal(27) ~ val(false).
good_cube(27,0) ~ val(false).
good_sphere(27,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 28
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(28,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(28,0) ~ val(-2.2096272921465547).
posY_t0(28,0) ~ val(0.6501942658521029).
posX_t1(28,0) ~ val(-2.9071479828379307).
posY_t1(28,0) ~ val(-0.04732642483927296).
right_of_t0(28,0,1) := true.
blocked_t0(28,0,behind) := true.
shape(28,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(28,1) ~ val(4.1493096351623535).
posY_t0(28,1) ~ val(-0.9525533318519592).
posX_t1(28,1) ~ val(4.1493096351623535).
posY_t1(28,1) ~ val(-0.9525533318519592).
left_of_t0(28,1,0) := true.
left_of_t0(28,1,2) := true.
shape(28,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(28,2) ~ val(-3.693817138671875).
posY_t0(28,2) ~ val(2.290310859680176).
posX_t1(28,2) ~ val(-3.693817138671875).
posY_t1(28,2) ~ val(2.290310859680176).
right_of_t0(28,2,1) := true.
blocked_t0(28,2,front) := true.
move_t0(28,0,left) := true.
goal(28) ~ val(false).
good_cube(28,0) ~ val(false).
good_sphere(28,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 29
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(29,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(29,0) ~ val(3.7443430423736572).
posY_t0(29,0) ~ val(-2.9517486095428467).
posX_t1(29,0) ~ val(3.017719495290841).
posY_t1(29,0) ~ val(-3.678372156625663).
left_of_t0(29,0,1) := true.
left_of_t0(29,0,2) := true.
shape(29,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(29,1) ~ val(-3.024193286895752).
posY_t0(29,1) ~ val(-5.489288330078125).
posX_t1(29,1) ~ val(-3.024193286895752).
posY_t1(29,1) ~ val(-5.489288330078125).
right_of_t0(29,1,0) := true.
right_of_t0(29,1,2) := true.
shape(29,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(29,2) ~ val(-4.731186866760254).
posY_t0(29,2) ~ val(1.7708135843276978).
posX_t1(29,2) ~ val(-4.731186866760254).
posY_t1(29,2) ~ val(1.7708135843276978).
right_of_t0(29,2,0) := true.
left_of_t0(29,2,1) := true.
move_t0(29,0,left) := true.
goal(29) ~ val(false).
good_cube(29,0) ~ val(false).
good_sphere(29,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 30
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(30,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(30,0) ~ val(3.017719495290841).
posY_t0(30,0) ~ val(-3.678372156625663).
posX_t1(30,0) ~ val(2.425331290259791).
posY_t1(30,0) ~ val(-4.270760361656713).
left_of_t0(30,0,1) := true.
left_of_t0(30,0,2) := true.
shape(30,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(30,1) ~ val(-3.024193286895752).
posY_t0(30,1) ~ val(-5.489288330078125).
posX_t1(30,1) ~ val(-3.024193286895752).
posY_t1(30,1) ~ val(-5.489288330078125).
right_of_t0(30,1,0) := true.
right_of_t0(30,1,2) := true.
shape(30,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(30,2) ~ val(-4.731186866760254).
posY_t0(30,2) ~ val(1.7708135843276978).
posX_t1(30,2) ~ val(-4.731186866760254).
posY_t1(30,2) ~ val(1.7708135843276978).
right_of_t0(30,2,0) := true.
left_of_t0(30,2,1) := true.
move_t0(30,0,left) := true.
goal(30) ~ val(false).
good_cube(30,0) ~ val(false).
good_sphere(30,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 31
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(31,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(31,0) ~ val(2.425331290259791).
posY_t0(31,0) ~ val(-4.270760361656713).
posX_t1(31,0) ~ val(1.7216129080819988).
posY_t1(31,0) ~ val(-4.9744787438345055).
left_of_t0(31,0,1) := true.
left_of_t0(31,0,2) := true.
shape(31,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(31,1) ~ val(-3.024193286895752).
posY_t0(31,1) ~ val(-5.489288330078125).
posX_t1(31,1) ~ val(-3.024193286895752).
posY_t1(31,1) ~ val(-5.489288330078125).
right_of_t0(31,1,0) := true.
right_of_t0(31,1,2) := true.
shape(31,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(31,2) ~ val(-4.731186866760254).
posY_t0(31,2) ~ val(1.7708135843276978).
posX_t1(31,2) ~ val(-4.731186866760254).
posY_t1(31,2) ~ val(1.7708135843276978).
right_of_t0(31,2,0) := true.
left_of_t0(31,2,1) := true.
move_t0(31,0,left) := true.
goal(31) ~ val(false).
good_cube(31,0) ~ val(false).
good_sphere(31,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 32
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(32,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(32,0) ~ val(1.7216129080819988).
posY_t0(32,0) ~ val(-4.9744787438345055).
posX_t1(32,0) ~ val(1.7216129080819988).
posY_t1(32,0) ~ val(-4.9744787438345055).
right_of_t0(32,0,2) := true.
left_of_t0(32,0,1) := true.
shape(32,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(32,1) ~ val(-3.024193286895752).
posY_t0(32,1) ~ val(-5.489288330078125).
posX_t1(32,1) ~ val(-2.4159660953683098).
posY_t1(32,1) ~ val(-4.881061138550683).
right_of_t0(32,1,0) := true.
right_of_t0(32,1,2) := true.
shape(32,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(32,2) ~ val(-4.731186866760254).
posY_t0(32,2) ~ val(1.7708135843276978).
posX_t1(32,2) ~ val(-4.731186866760254).
posY_t1(32,2) ~ val(1.7708135843276978).
left_of_t0(32,2,0) := true.
left_of_t0(32,2,1) := true.
move_t0(32,1,right) := true.
goal(32) ~ val(false).
good_cube(32,0) ~ val(true).
good_sphere(32,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 33
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(33,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(33,0) ~ val(1.7216129080819988).
posY_t0(33,0) ~ val(-4.9744787438345055).
posX_t1(33,0) ~ val(1.7216129080819988).
posY_t1(33,0) ~ val(-4.9744787438345055).
right_of_t0(33,0,2) := true.
left_of_t0(33,0,1) := true.
shape(33,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(33,1) ~ val(-2.4159660953683098).
posY_t0(33,1) ~ val(-4.881061138550683).
posX_t1(33,1) ~ val(-1.7016405530051593).
posY_t1(33,1) ~ val(-4.166735596187532).
right_of_t0(33,1,0) := true.
right_of_t0(33,1,2) := true.
shape(33,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(33,2) ~ val(-4.731186866760254).
posY_t0(33,2) ~ val(1.7708135843276978).
posX_t1(33,2) ~ val(-4.731186866760254).
posY_t1(33,2) ~ val(1.7708135843276978).
left_of_t0(33,2,0) := true.
left_of_t0(33,2,1) := true.
move_t0(33,1,right) := true.
goal(33) ~ val(false).
good_cube(33,0) ~ val(true).
good_sphere(33,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 34
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(34,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(34,0) ~ val(1.7216129080819988).
posY_t0(34,0) ~ val(-4.9744787438345055).
posX_t1(34,0) ~ val(1.7216129080819988).
posY_t1(34,0) ~ val(-4.9744787438345055).
right_of_t0(34,0,2) := true.
left_of_t0(34,0,1) := true.
shape(34,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(34,1) ~ val(-1.7016405530051593).
posY_t0(34,1) ~ val(-4.166735596187532).
posX_t1(34,1) ~ val(-0.9219627097228315).
posY_t1(34,1) ~ val(-3.3870577529052044).
right_of_t0(34,1,0) := true.
right_of_t0(34,1,2) := true.
shape(34,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(34,2) ~ val(-4.731186866760254).
posY_t0(34,2) ~ val(1.7708135843276978).
posX_t1(34,2) ~ val(-4.731186866760254).
posY_t1(34,2) ~ val(1.7708135843276978).
left_of_t0(34,2,0) := true.
left_of_t0(34,2,1) := true.
move_t0(34,1,right) := true.
goal(34) ~ val(false).
good_cube(34,0) ~ val(true).
good_sphere(34,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 35
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(35,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(35,0) ~ val(1.7216129080819988).
posY_t0(35,0) ~ val(-4.9744787438345055).
posX_t1(35,0) ~ val(1.7216129080819988).
posY_t1(35,0) ~ val(-4.9744787438345055).
right_of_t0(35,0,2) := true.
left_of_t0(35,0,1) := true.
shape(35,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(35,1) ~ val(-0.9219627097228315).
posY_t0(35,1) ~ val(-3.3870577529052044).
posX_t1(35,1) ~ val(-0.20820873912138405).
posY_t1(35,1) ~ val(-2.673303782303757).
right_of_t0(35,1,0) := true.
right_of_t0(35,1,2) := true.
shape(35,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(35,2) ~ val(-4.731186866760254).
posY_t0(35,2) ~ val(1.7708135843276978).
posX_t1(35,2) ~ val(-4.731186866760254).
posY_t1(35,2) ~ val(1.7708135843276978).
left_of_t0(35,2,0) := true.
left_of_t0(35,2,1) := true.
move_t0(35,1,right) := true.
goal(35) ~ val(false).
good_cube(35,0) ~ val(true).
good_sphere(35,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 36
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(36,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(36,0) ~ val(1.7216129080819988).
posY_t0(36,0) ~ val(-4.9744787438345055).
posX_t1(36,0) ~ val(1.7216129080819988).
posY_t1(36,0) ~ val(-4.9744787438345055).
right_of_t0(36,0,1) := true.
right_of_t0(36,0,2) := true.
shape(36,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(36,1) ~ val(-0.20820873912138405).
posY_t0(36,1) ~ val(-2.673303782303757).
posX_t1(36,1) ~ val(0.3963041874901331).
posY_t1(36,1) ~ val(-2.06879085569224).
left_of_t0(36,1,0) := true.
shape(36,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(36,2) ~ val(-4.731186866760254).
posY_t0(36,2) ~ val(1.7708135843276978).
posX_t1(36,2) ~ val(-4.731186866760254).
posY_t1(36,2) ~ val(1.7708135843276978).
left_of_t0(36,2,0) := true.
move_t0(36,1,right) := true.
goal(36) ~ val(false).
good_cube(36,0) ~ val(true).
good_sphere(36,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 37
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(37,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(37,0) ~ val(-2.7266414165496826).
posY_t0(37,0) ~ val(1.064864993095398).
posX_t1(37,0) ~ val(-2.7266414165496826).
posY_t1(37,0) ~ val(1.064864993095398).
right_of_t0(37,0,1) := true.
right_of_t0(37,0,2) := true.
shape(37,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(37,1) ~ val(2.720245599746704).
posY_t0(37,1) ~ val(2.6638383865356445).
posX_t1(37,1) ~ val(2.720245599746704).
posY_t1(37,1) ~ val(2.6638383865356445).
left_of_t0(37,1,0) := true.
left_of_t0(37,1,2) := true.
shape(37,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(37,2) ~ val(3.0418782234191895).
posY_t0(37,2) ~ val(-4.339413642883301).
posX_t1(37,2) ~ val(3.0418782234191895).
posY_t1(37,2) ~ val(-4.339413642883301).
right_of_t0(37,2,1) := true.
left_of_t0(37,2,0) := true.
goal(37) ~ val(true).
good_cube(37,0) ~ val(true).
good_sphere(37,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 38
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(38,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(38,0) ~ val(2.0541696548461914).
posY_t0(38,0) ~ val(1.9906916618347168).
posX_t1(38,0) ~ val(1.4212626186131407).
posY_t1(38,0) ~ val(1.3577846256016661).
right_of_t0(38,0,1) := true.
left_of_t0(38,0,2) := true.
shape(38,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(38,1) ~ val(2.669872760772705).
posY_t0(38,1) ~ val(4.588876247406006).
posX_t1(38,1) ~ val(2.669872760772705).
posY_t1(38,1) ~ val(4.588876247406006).
left_of_t0(38,1,0) := true.
left_of_t0(38,1,2) := true.
shape(38,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(38,2) ~ val(-2.827380895614624).
posY_t0(38,2) ~ val(3.77880859375).
posX_t1(38,2) ~ val(-2.827380895614624).
posY_t1(38,2) ~ val(3.77880859375).
right_of_t0(38,2,0) := true.
right_of_t0(38,2,1) := true.
move_t0(38,0,left) := true.
goal(38) ~ val(false).
good_cube(38,0) ~ val(false).
good_sphere(38,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 39
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(39,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(39,0) ~ val(1.4212626186131407).
posY_t0(39,0) ~ val(1.3577846256016661).
posX_t1(39,0) ~ val(0.853931124705679).
posY_t1(39,0) ~ val(0.7904531316942044).
right_of_t0(39,0,1) := true.
left_of_t0(39,0,2) := true.
shape(39,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(39,1) ~ val(2.669872760772705).
posY_t0(39,1) ~ val(4.588876247406006).
posX_t1(39,1) ~ val(2.669872760772705).
posY_t1(39,1) ~ val(4.588876247406006).
left_of_t0(39,1,0) := true.
left_of_t0(39,1,2) := true.
shape(39,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(39,2) ~ val(-2.827380895614624).
posY_t0(39,2) ~ val(3.77880859375).
posX_t1(39,2) ~ val(-2.827380895614624).
posY_t1(39,2) ~ val(3.77880859375).
right_of_t0(39,2,0) := true.
right_of_t0(39,2,1) := true.
move_t0(39,0,left) := true.
goal(39) ~ val(false).
good_cube(39,0) ~ val(false).
good_sphere(39,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 40
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(40,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(40,0) ~ val(0.853931124705679).
posY_t0(40,0) ~ val(0.7904531316942044).
posX_t1(40,0) ~ val(0.2006497443123033).
posY_t1(40,0) ~ val(0.1371717513008287).
right_of_t0(40,0,1) := true.
left_of_t0(40,0,2) := true.
shape(40,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(40,1) ~ val(2.669872760772705).
posY_t0(40,1) ~ val(4.588876247406006).
posX_t1(40,1) ~ val(2.669872760772705).
posY_t1(40,1) ~ val(4.588876247406006).
left_of_t0(40,1,0) := true.
left_of_t0(40,1,2) := true.
shape(40,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(40,2) ~ val(-2.827380895614624).
posY_t0(40,2) ~ val(3.77880859375).
posX_t1(40,2) ~ val(-2.827380895614624).
posY_t1(40,2) ~ val(3.77880859375).
right_of_t0(40,2,0) := true.
right_of_t0(40,2,1) := true.
move_t0(40,0,left) := true.
goal(40) ~ val(false).
good_cube(40,0) ~ val(false).
good_sphere(40,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 41
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(41,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(41,0) ~ val(1.4711809158325195).
posY_t0(41,0) ~ val(-2.9543521404266357).
posX_t1(41,0) ~ val(1.4711809158325195).
posY_t1(41,0) ~ val(-2.9543521404266357).
right_of_t0(41,0,1) := true.
right_of_t0(41,0,2) := true.
shape(41,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(41,1) ~ val(1.2360855340957642).
posY_t0(41,1) ~ val(5.28458309173584).
posX_t1(41,1) ~ val(1.2360855340957642).
posY_t1(41,1) ~ val(5.28458309173584).
left_of_t0(41,1,0) := true.
left_of_t0(41,1,2) := true.
shape(41,2) ~ finite([0.35:cube,0.65:cylinder]).
posX_t0(41,2) ~ val(1.348386526107788).
posY_t0(41,2) ~ val(3.358701229095459).
posX_t1(41,2) ~ val(1.348386526107788).
posY_t1(41,2) ~ val(3.358701229095459).
right_of_t0(41,2,1) := true.
left_of_t0(41,2,0) := true.
goal(41) ~ val(true).
good_cube(41,0) ~ val(true).
good_sphere(41,0) ~ val(true).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 42
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(42,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(42,0) ~ val(0.0654878169298172).
posY_t0(42,0) ~ val(-4.0469207763671875).
posX_t1(42,0) ~ val(0.0654878169298172).
posY_t1(42,0) ~ val(-4.0469207763671875).
right_of_t0(42,0,1) := true.
right_of_t0(42,0,2) := true.
shape(42,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(42,1) ~ val(-3.942063808441162).
posY_t0(42,1) ~ val(1.9548792839050293).
posX_t1(42,1) ~ val(-3.159949046590247).
posY_t1(42,1) ~ val(2.7369940457559445).
right_of_t0(42,1,2) := true.
left_of_t0(42,1,0) := true.
shape(42,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(42,2) ~ val(0.1704125553369522).
posY_t0(42,2) ~ val(1.9979825019836426).
posX_t1(42,2) ~ val(0.1704125553369522).
posY_t1(42,2) ~ val(1.9979825019836426).
left_of_t0(42,2,0) := true.
left_of_t0(42,2,1) := true.
move_t0(42,1,right) := true.
goal(42) ~ val(false).
good_cube(42,0) ~ val(true).
good_sphere(42,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 43
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(43,0) ~ finite([0.35:cylinder,0.65:cube]).
posX_t0(43,0) ~ val(0.0654878169298172).
posY_t0(43,0) ~ val(-4.0469207763671875).
posX_t1(43,0) ~ val(0.0654878169298172).
posY_t1(43,0) ~ val(-4.0469207763671875).
right_of_t0(43,0,1) := true.
right_of_t0(43,0,2) := true.
shape(43,1) ~ finite([0.35:cylinder,0.65:sphere]).
posX_t0(43,1) ~ val(-3.159949046590247).
posY_t0(43,1) ~ val(2.7369940457559445).
posX_t1(43,1) ~ val(-2.498569562313695).
posY_t1(43,1) ~ val(3.3983735300324964).
right_of_t0(43,1,2) := true.
left_of_t0(43,1,0) := true.
shape(43,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(43,2) ~ val(0.1704125553369522).
posY_t0(43,2) ~ val(1.9979825019836426).
posX_t1(43,2) ~ val(0.1704125553369522).
posY_t1(43,2) ~ val(1.9979825019836426).
left_of_t0(43,2,0) := true.
left_of_t0(43,2,1) := true.
move_t0(43,1,right) := true.
goal(43) ~ val(false).
good_cube(43,0) ~ val(true).
good_sphere(43,0) ~ val(false).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Example 44
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shape(44,0) ~ finite([0.35:sphere,0.65:cube]).
posX_t0(44,0) ~ val(0.0654878169298172).
posY_t0(44,0) ~ val(-4.0469207763671875).
posX_t1(44,0) ~ val(0.0654878169298172).
posY_t1(44,0) ~ val(-4.0469207763671875).
right_of_t0(44,0,1) := true.
right_of_t0(44,0,2) := true.
shape(44,1) ~ finite([0.35:cube,0.65:sphere]).
posX_t0(44,1) ~ val(-2.498569562313695).
posY_t0(44,1) ~ val(3.3983735300324964).
posX_t1(44,1) ~ val(-1.7226345245276993).
posY_t1(44,1) ~ val(4.174308567818493).
right_of_t0(44,1,2) := true.
left_of_t0(44,1,0) := true.
shape(44,2) ~ finite([0.35:sphere,0.65:cylinder]).
posX_t0(44,2) ~ val(0.1704125553369522).
posY_t0(44,2) ~ val(1.9979825019836426).
posX_t1(44,2) ~ val(0.1704125553369522).
posY_t1(44,2) ~ val(1.9979825019836426).
left_of_t0(44,2,0) := true.
left_of_t0(44,2,1) := true.
move_t0(44,1,right) := true.
goal(44) ~ val(false).
good_cube(44,0) ~ val(true).
good_sphere(44,0) ~ val(false).
