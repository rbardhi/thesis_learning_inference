%%% -*- Mode: Prolog; -*-
:- use_module(library(lists)).

cnt(X, P,Count) :- findall(X,P,L), length(L,Count).

oneElementOfList([H|_], X) :- X = H.

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
maxMod(X, P, Max) :- findall(X,P,L), lmax(L, Max1), oneElementOfList(Max1, Max).

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, [], Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, TMin, Min) :- TMin=[] -> lmin(MMin, [], [], Min); Min=TMin, !.
leftover([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, TMin, Min); leftover(Seen, MMin, [H|TMin], Min).
minMod(X, P, Min) :- findall(X,P,L), lmin(L, Min1), oneElementOfList(Min1, Min).

list2set([], []).
list2set([H|T], [H|T1]) :- subtract(T, [H], T2), list2set(T2, T1).

%maxMod(Template,Goal,G) :-
%    findall(X,bagof(Template,Goal,X),Lists),
%    flatten(Lists,G3),
%    list2set(G3,Gset),
%    member(G,Gset).

%minMod(Template,Goal,G) :-
%    findall(X,bagof(Template,Goal,X),Lists),
%    flatten(Lists,G3),
%    list2set(G3,Gset),
%    member(G,Gset).

max(X, P, Max) :- findall(X,P,L), max_list(L, Max).

min(X, P, Min) :- findall(X,P,L), min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(X, P, Avg) :- findall(X,P,L), length(L,Cnt), listavg(L, Cnt, Avg).
%%%%%%%%%%% Declarative Bias starts here %%%%%%%%%%%%
%Discontiguous
:- discontiguous goal/2.
:- discontiguous good_cube/3.
:- discontiguous good_sphere/3.
:- discontiguous shape/3.
:- discontiguous move_t0/3.
:- discontiguous posX_t0/3.
:- discontiguous posX_t1/3.
:- discontiguous posY_t0/3.
:- discontiguous posY_t1/3.
:- discontiguous blocked_t0/3.
:- discontiguous blocked_t1/3.
:- discontiguous right_of_t0/3.
:- discontiguous right_of_t1/3.
:- discontiguous left_of_t0/3.
:- discontiguous left_of_t1/3.

%Dynamics
:- dynamic goal/2.
:- dynamic good_cube/3.
:- dynamic good_sphere/3.
:- dynamic shape/3.
:- dynamic move_t0/3.
:- dynamic posX_t0/3.
:- dynamic posX_t1/3.
:- dynamic posY_t0/3.
:- dynamic posY_t1/3.
:- dynamic blocked_t0/3.
:- dynamic blocked_t1/3.
:- dynamic right_of_t0/3.
:- dynamic right_of_t1/3.
:- dynamic left_of_t0/3.
:- dynamic left_of_t1/3.

%Types
base(goal(w,b)).
base(good_cube(w,i,b)).
base(good_sphere(w,i,b)).
base(shape(w,i,sh)).
base(move_t0(w,i,dir)).
base(posX_t0(w,i,x)).
base(posX_t1(w,i,x)).
base(posY_t0(w,i,x)).
base(posY_t1(w,i,x)).
base(blocked_t0(w,i,dir)).
base(blocked_t1(w,i,dir)).
base(right_of_t0(w,i,i)).
base(right_of_t1(w,i,i)).
base(left_of_t0(w,i,i)).
base(left_of_t1(w,i,i)).

%Modes
mode(posX_t1,none,goal(+,-)).
mode(posX_t1,none,good_cube(+,+,-)).
mode(posX_t1,none,good_sphere(+,+,-)).
mode(posX_t1,none,shape(+,+,-)).
mode(posX_t1,none,move_t0(+,+,-)).
mode(posX_t1,none,posX_t0(+,+,-)).
%mode(posX_t1,none,blocked_t0(+,+,-)).
mode(posX_t1,none,right_of_t0(+,-,+)).
mode(posX_t1,none,left_of_t0(+,-,+)).

mode(posY_t1,none,goal(+,-)).
mode(posY_t1,none,good_cube(+,+,-)).
mode(posY_t1,none,good_sphere(+,+,-)).
mode(posY_t1,none,shape(+,+,-)).
mode(posY_t1,none,move_t0(+,+,-)).
mode(posY_t1,none,posY_t0(+,+,-)).
%mode(posY_t1,none,blocked_t0(+,+,-)).
mode(posY_t1,none,right_of_t0(+,-,+)).
mode(posY_t1,none,left_of_t0(+,-,+)).

%Aggregations
agg(none).
agg(minMode).
agg(maxMode).

%Threshold
thres(goal, 2, discrete, [true,false]).
thres(good_cube, 3, discrete, [true,false]).
thres(good_sphere, 3, discrete, [true,false]).
thres(shape, 3, discrete, [cube,sphere,cylinder]).
thres(move_t0, 3, discrete, [left,right]).
thres(posX_t0, 3, continuous, []).
thres(posX_t1, 3, continuous, []).
thres(posY_t0, 3, continuous, []).
thres(posY_t1, 3, continuous, []).
thres(blocked_t0, 3, discrete, [front,left,behind,right]).
thres(blocked_t1, 3, discrete, [front,left,behind,right]).
thres(right_of_t0, 3, discrete, []).
thres(right_of_t1, 3, discrete, []).
thres(left_of_t0, 3, discrete, []).
thres(left_of_t1, 3, discrete, []).

%Targets
learn(posX_t1, 3, 3, continuous).
learn(posY_t1, 3, 3, continuous).
%%%%%%%%%%% Declarative Bias ends here %%%%%%%%%%%%


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
%World 0
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(0,0,cube).
shape(0,1,sphere).
shape(0,2,cylinder).
%Timestep 0
posX_t0(0,0,-1.9738246202468872).
posY_t0(0,0,2.8072707653045654).
posX_t0(0,1,3.3494784832000732).
posY_t0(0,1,-1.7233879566192627).
posX_t0(0,2,-1.522005319595337).
posY_t0(0,2,-2.4900858402252197).
right_of_t0(0,0,1).
right_of_t0(0,2,0).
right_of_t0(0,2,1).
left_of_t0(0,0,2).
left_of_t0(0,1,0).
left_of_t0(0,1,2).
good_cube(0,0,false).
good_sphere(0,1,true).
goal(0,false).
move_t0(0,0,left).
%Timestep 1
posX_t1(0,0,-2.6675119343707867).
posY_t1(0,0,2.113583451180666).
posX_t1(0,1,3.3494784832000732).
posY_t1(0,1,-1.7233879566192627).
posX_t1(0,2,-1.522005319595337).
posY_t1(0,2,-2.4900858402252197).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10000,0,cube).
shape(10000,1,sphere).
shape(10000,2,cylinder).
%Timestep 0
posX_t0(10000,0,-2.6675119343707867).
posY_t0(10000,0,2.113583451180666).
posX_t0(10000,1,3.3494784832000732).
posY_t0(10000,1,-1.7233879566192627).
posX_t0(10000,2,-1.522005319595337).
posY_t0(10000,2,-2.4900858402252197).
right_of_t0(10000,0,1).
right_of_t0(10000,2,0).
right_of_t0(10000,2,1).
left_of_t0(10000,0,2).
left_of_t0(10000,1,0).
left_of_t0(10000,1,2).
good_cube(10000,0,false).
good_sphere(10000,1,true).
goal(10000,false).
move_t0(10000,0,left).
%Timestep 1
posX_t1(10000,0,-3.501700857685792).
posY_t1(10000,0,1.2793945278656604).
posX_t1(10000,1,3.3494784832000732).
posY_t1(10000,1,-1.7233879566192627).
posX_t1(10000,2,-1.522005319595337).
posY_t1(10000,2,-2.4900858402252197).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20000,0,cube).
shape(20000,1,sphere).
shape(20000,2,cylinder).
%Timestep 0
posX_t0(20000,0,-3.501700857685792).
posY_t0(20000,0,1.2793945278656604).
posX_t0(20000,1,3.3494784832000732).
posY_t0(20000,1,-1.7233879566192627).
posX_t0(20000,2,-1.522005319595337).
posY_t0(20000,2,-2.4900858402252197).
right_of_t0(20000,0,1).
right_of_t0(20000,2,0).
right_of_t0(20000,2,1).
left_of_t0(20000,0,2).
left_of_t0(20000,1,0).
left_of_t0(20000,1,2).
good_cube(20000,0,false).
good_sphere(20000,1,true).
goal(20000,false).
move_t0(20000,0,left).
%Timestep 1
posX_t1(20000,0,-4.218037133570532).
posY_t1(20000,0,0.5630582519809206).
posX_t1(20000,1,3.3494784832000732).
posY_t1(20000,1,-1.7233879566192627).
posX_t1(20000,2,-1.522005319595337).
posY_t1(20000,2,-2.4900858402252197).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30000,0,cube).
shape(30000,1,sphere).
shape(30000,2,cylinder).
%Timestep 0
posX_t0(30000,0,-4.218037133570532).
posY_t0(30000,0,0.5630582519809206).
posX_t0(30000,1,3.3494784832000732).
posY_t0(30000,1,-1.7233879566192627).
posX_t0(30000,2,-1.522005319595337).
posY_t0(30000,2,-2.4900858402252197).
right_of_t0(30000,0,1).
right_of_t0(30000,2,0).
right_of_t0(30000,2,1).
left_of_t0(30000,0,2).
left_of_t0(30000,1,0).
left_of_t0(30000,1,2).
good_cube(30000,0,false).
good_sphere(30000,1,true).
goal(30000,false).
move_t0(30000,0,left).
%Timestep 1
posX_t1(30000,0,-4.934886723954683).
posY_t1(30000,0,-0.1537913384032309).
posX_t1(30000,1,3.3494784832000732).
posY_t1(30000,1,-1.7233879566192627).
posX_t1(30000,2,-1.522005319595337).
posY_t1(30000,2,-2.4900858402252197).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 1
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1,0,cube).
shape(1,1,sphere).
shape(1,2,cylinder).
%Timestep 0
posX_t0(1,0,-4.566066741943359).
posY_t0(1,0,-0.4180573523044586).
posX_t0(1,1,3.9949753284454346).
posY_t0(1,1,-2.4990766048431396).
posX_t0(1,2,-1.6770718097686768).
posY_t0(1,2,-4.8249359130859375).
right_of_t0(1,0,1).
right_of_t0(1,2,0).
right_of_t0(1,2,1).
left_of_t0(1,0,2).
left_of_t0(1,1,0).
left_of_t0(1,1,2).
good_cube(1,0,false).
good_sphere(1,1,true).
goal(1,false).
move_t0(1,0,left).
%Timestep 1
posX_t1(1,0,-5.37105271250493).
posY_t1(1,0,-1.2230433228660291).
posX_t1(1,1,3.9949753284454346).
posY_t1(1,1,-2.4990766048431396).
posX_t1(1,2,-1.6770718097686768).
posY_t1(1,2,-4.8249359130859375).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10001
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10001,0,cube).
shape(10001,1,sphere).
shape(10001,2,cylinder).
%Timestep 0
posX_t0(10001,0,-5.37105271250493).
posY_t0(10001,0,-1.2230433228660291).
posX_t0(10001,1,3.9949753284454346).
posY_t0(10001,1,-2.4990766048431396).
posX_t0(10001,2,-1.6770718097686768).
posY_t0(10001,2,-4.8249359130859375).
right_of_t0(10001,0,1).
right_of_t0(10001,2,1).
left_of_t0(10001,1,0).
left_of_t0(10001,1,2).
good_cube(10001,0,false).
good_sphere(10001,1,true).
goal(10001,false).
move_t0(10001,0,left).
%Timestep 1
posX_t1(10001,0,-6.007175742941044).
posY_t1(10001,0,-1.8591663533021432).
posX_t1(10001,1,3.9949753284454346).
posY_t1(10001,1,-2.4990766048431396).
posX_t1(10001,2,-1.6770718097686768).
posY_t1(10001,2,-4.8249359130859375).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 2
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2,0,cube).
shape(2,1,sphere).
shape(2,2,cylinder).
%Timestep 0
posX_t0(2,0,3.867204189300537).
posY_t0(2,0,-4.821146011352539).
posX_t0(2,1,0.3153548538684845).
posY_t0(2,1,0.18131887912750244).
posX_t0(2,2,-0.7300168871879578).
posY_t0(2,2,1.7576556205749512).
right_of_t0(2,0,1).
right_of_t0(2,0,2).
right_of_t0(2,1,2).
left_of_t0(2,1,0).
left_of_t0(2,2,0).
left_of_t0(2,2,1).
blocked_t0(2,2,front).
blocked_t0(2,1,behind).
good_cube(2,0,true).
good_sphere(2,1,false).
goal(2,false).
move_t0(2,1,right).
%Timestep 1
posX_t1(2,0,3.867204189300537).
posY_t1(2,0,-4.821146011352539).
posX_t1(2,1,1.0720776923796724).
posY_t1(2,1,0.9380417176386905).
posX_t1(2,2,-0.7300168871879578).
posY_t1(2,2,1.7576556205749512).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 6
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(6,0,cube).
shape(6,1,sphere).
shape(6,2,cylinder).
%Timestep 0
posX_t0(6,0,-3.679595470428467).
posY_t0(6,0,-3.9106414318084717).
posX_t0(6,1,4.525230884552002).
posY_t0(6,1,-3.7449662685394287).
posX_t0(6,2,-2.413264036178589).
posY_t0(6,2,0.277258962392807).
right_of_t0(6,0,1).
right_of_t0(6,0,2).
right_of_t0(6,2,1).
left_of_t0(6,1,0).
left_of_t0(6,1,2).
left_of_t0(6,2,0).
good_cube(6,0,true).
good_sphere(6,1,true).
goal(6,true).
%Timestep 1
posX_t1(6,0,-3.679595470428467).
posY_t1(6,0,-3.9106414318084717).
posX_t1(6,1,4.525230884552002).
posY_t1(6,1,-3.7449662685394287).
posX_t1(6,2,-2.413264036178589).
posY_t1(6,2,0.277258962392807).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 9
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(9,0,cube).
shape(9,1,sphere).
shape(9,2,cylinder).
%Timestep 0
posX_t0(9,0,4.667672634124756).
posY_t0(9,0,-2.2729482650756836).
posX_t0(9,1,1.5577178001403809).
posY_t0(9,1,4.958014011383057).
posX_t0(9,2,3.2543301582336426).
posY_t0(9,2,-5.493769645690918).
right_of_t0(9,0,1).
right_of_t0(9,2,0).
right_of_t0(9,2,1).
left_of_t0(9,0,2).
left_of_t0(9,1,0).
left_of_t0(9,1,2).
good_cube(9,0,false).
good_sphere(9,1,true).
goal(9,false).
move_t0(9,0,left).
%Timestep 1
posX_t1(9,0,3.9236836953805936).
posY_t1(9,0,-3.016937203819846).
posX_t1(9,1,1.5577178001403809).
posY_t1(9,1,4.958014011383057).
posX_t1(9,2,3.2543301582336426).
posY_t1(9,2,-5.493769645690918).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10009
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10009,0,cube).
shape(10009,1,sphere).
shape(10009,2,cylinder).
%Timestep 0
posX_t0(10009,0,3.9236836953805936).
posY_t0(10009,0,-3.016937203819846).
posX_t0(10009,1,1.5577178001403809).
posY_t0(10009,1,4.958014011383057).
posX_t0(10009,2,3.2543301582336426).
posY_t0(10009,2,-5.493769645690918).
right_of_t0(10009,0,1).
right_of_t0(10009,2,0).
right_of_t0(10009,2,1).
left_of_t0(10009,0,2).
left_of_t0(10009,1,0).
left_of_t0(10009,1,2).
good_cube(10009,0,false).
good_sphere(10009,1,true).
goal(10009,false).
move_t0(10009,0,left).
%Timestep 1
posX_t1(10009,0,3.302955329032961).
posY_t1(10009,0,-3.6376655701674783).
posX_t1(10009,1,1.5577178001403809).
posY_t1(10009,1,4.958014011383057).
posX_t1(10009,2,3.2543301582336426).
posY_t1(10009,2,-5.493769645690918).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20009
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20009,0,cube).
shape(20009,1,sphere).
shape(20009,2,cylinder).
%Timestep 0
posX_t0(20009,0,3.302955329032961).
posY_t0(20009,0,-3.6376655701674783).
posX_t0(20009,1,1.5577178001403809).
posY_t0(20009,1,4.958014011383057).
posX_t0(20009,2,3.2543301582336426).
posY_t0(20009,2,-5.493769645690918).
right_of_t0(20009,0,1).
right_of_t0(20009,2,0).
right_of_t0(20009,2,1).
left_of_t0(20009,0,2).
left_of_t0(20009,1,0).
left_of_t0(20009,1,2).
good_cube(20009,0,false).
good_sphere(20009,1,true).
goal(20009,false).
move_t0(20009,0,left).
%Timestep 1
posX_t1(20009,0,2.4807311499788343).
posY_t1(20009,0,-4.459889749221605).
posX_t1(20009,1,1.5577178001403809).
posY_t1(20009,1,4.958014011383057).
posX_t1(20009,2,3.2543301582336426).
posY_t1(20009,2,-5.493769645690918).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30009
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30009,0,cube).
shape(30009,1,sphere).
shape(30009,2,cylinder).
%Timestep 0
posX_t0(30009,0,2.4807311499788343).
posY_t0(30009,0,-4.459889749221605).
posX_t0(30009,1,1.5577178001403809).
posY_t0(30009,1,4.958014011383057).
posX_t0(30009,2,3.2543301582336426).
posY_t0(30009,2,-5.493769645690918).
right_of_t0(30009,0,1).
right_of_t0(30009,2,1).
left_of_t0(30009,1,0).
left_of_t0(30009,1,2).
blocked_t0(30009,0,front).
blocked_t0(30009,2,behind).
good_cube(30009,0,false).
good_sphere(30009,1,true).
goal(30009,false).
move_t0(30009,0,left).
%Timestep 1
posX_t1(30009,0,1.7704023176977217).
posY_t1(30009,0,-5.170218581502717).
posX_t1(30009,1,1.5577178001403809).
posY_t1(30009,1,4.958014011383057).
posX_t1(30009,2,3.2543301582336426).
posY_t1(30009,2,-5.493769645690918).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 12
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(12,0,cube).
shape(12,1,sphere).
shape(12,2,cylinder).
%Timestep 0
posX_t0(12,0,1.7970571517944336).
posY_t0(12,0,-5.405144214630127).
posX_t0(12,1,-5.034622669219971).
posY_t0(12,1,-2.8447213172912598).
posX_t0(12,2,-0.0822870135307312).
posY_t0(12,2,-4.427548408508301).
right_of_t0(12,1,0).
right_of_t0(12,1,2).
right_of_t0(12,2,0).
left_of_t0(12,0,1).
left_of_t0(12,0,2).
left_of_t0(12,2,1).
good_cube(12,0,false).
good_sphere(12,1,false).
goal(12,false).
move_t0(12,0,left).
%Timestep 1
posX_t1(12,0,1.117260393141399).
posY_t1(12,0,-6.084940973283162).
posX_t1(12,1,-5.034622669219971).
posY_t1(12,1,-2.8447213172912598).
posX_t1(12,2,-0.0822870135307312).
posY_t1(12,2,-4.427548408508301).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10012
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10012,0,cube).
shape(10012,1,sphere).
shape(10012,2,cylinder).
%Timestep 0
posX_t0(10012,0,1.117260393141399).
posY_t0(10012,0,-6.084940973283162).
posX_t0(10012,1,-5.034622669219971).
posY_t0(10012,1,-2.8447213172912598).
posX_t0(10012,2,-0.0822870135307312).
posY_t0(10012,2,-4.427548408508301).
right_of_t0(10012,0,2).
right_of_t0(10012,1,0).
right_of_t0(10012,1,2).
left_of_t0(10012,0,1).
left_of_t0(10012,2,0).
left_of_t0(10012,2,1).
good_cube(10012,0,true).
good_sphere(10012,1,false).
goal(10012,false).
move_t0(10012,1,right).
%Timestep 1
posX_t1(10012,0,1.117260393141399).
posY_t1(10012,0,-6.084940973283162).
posX_t1(10012,1,-4.314645572817599).
posY_t1(10012,1,-2.1247442208888883).
posX_t1(10012,2,-0.0822870135307312).
posY_t1(10012,2,-4.427548408508301).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20012
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20012,0,cube).
shape(20012,1,sphere).
shape(20012,2,cylinder).
%Timestep 0
posX_t0(20012,0,1.117260393141399).
posY_t0(20012,0,-6.084940973283162).
posX_t0(20012,1,-4.314645572817599).
posY_t0(20012,1,-2.1247442208888883).
posX_t0(20012,2,-0.0822870135307312).
posY_t0(20012,2,-4.427548408508301).
right_of_t0(20012,0,2).
right_of_t0(20012,1,0).
right_of_t0(20012,1,2).
left_of_t0(20012,0,1).
left_of_t0(20012,2,0).
left_of_t0(20012,2,1).
good_cube(20012,0,true).
good_sphere(20012,1,false).
goal(20012,false).
move_t0(20012,1,right).
%Timestep 1
posX_t1(20012,0,1.117260393141399).
posY_t1(20012,0,-6.084940973283162).
posX_t1(20012,1,-3.6336627534808055).
posY_t1(20012,1,-1.4437614015520945).
posX_t1(20012,2,-0.0822870135307312).
posY_t1(20012,2,-4.427548408508301).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30012
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30012,0,cube).
shape(30012,1,sphere).
shape(30012,2,cylinder).
%Timestep 0
posX_t0(30012,0,1.117260393141399).
posY_t0(30012,0,-6.084940973283162).
posX_t0(30012,1,-3.6336627534808055).
posY_t0(30012,1,-1.4437614015520945).
posX_t0(30012,2,-0.0822870135307312).
posY_t0(30012,2,-4.427548408508301).
right_of_t0(30012,0,2).
right_of_t0(30012,1,2).
left_of_t0(30012,2,0).
left_of_t0(30012,2,1).
good_cube(30012,0,true).
good_sphere(30012,1,false).
goal(30012,false).
move_t0(30012,1,right).
%Timestep 1
posX_t1(30012,0,1.117260393141399).
posY_t1(30012,0,-6.084940973283162).
posX_t1(30012,1,-2.8015021464707406).
posY_t1(30012,1,-0.6116007945420296).
posX_t1(30012,2,-0.0822870135307312).
posY_t1(30012,2,-4.427548408508301).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 18
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(18,0,cube).
shape(18,1,sphere).
shape(18,2,cylinder).
%Timestep 0
posX_t0(18,0,-1.5041167736053467).
posY_t0(18,0,-1.8112951517105103).
posX_t0(18,1,1.0979212522506714).
posY_t0(18,1,4.275920391082764).
posX_t0(18,2,-1.8191182613372803).
posY_t0(18,2,-5.212826728820801).
right_of_t0(18,0,1).
right_of_t0(18,2,0).
right_of_t0(18,2,1).
left_of_t0(18,0,2).
left_of_t0(18,1,0).
left_of_t0(18,1,2).
good_cube(18,0,false).
good_sphere(18,1,true).
goal(18,false).
move_t0(18,0,left).
%Timestep 1
posX_t1(18,0,-2.3007414242106607).
posY_t1(18,0,-2.6079198023158243).
posX_t1(18,1,1.0979212522506714).
posY_t1(18,1,4.275920391082764).
posX_t1(18,2,-1.8191182613372803).
posY_t1(18,2,-5.212826728820801).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10018
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10018,0,cube).
shape(10018,1,sphere).
shape(10018,2,cylinder).
%Timestep 0
posX_t0(10018,0,-2.3007414242106607).
posY_t0(10018,0,-2.6079198023158243).
posX_t0(10018,1,1.0979212522506714).
posY_t0(10018,1,4.275920391082764).
posX_t0(10018,2,-1.8191182613372803).
posY_t0(10018,2,-5.212826728820801).
right_of_t0(10018,0,1).
right_of_t0(10018,2,0).
right_of_t0(10018,2,1).
left_of_t0(10018,0,2).
left_of_t0(10018,1,0).
left_of_t0(10018,1,2).
good_cube(10018,0,false).
good_sphere(10018,1,true).
goal(10018,false).
move_t0(10018,0,left).
%Timestep 1
posX_t1(10018,0,-3.08334974792738).
posY_t1(10018,0,-3.3905281260325437).
posX_t1(10018,1,1.0979212522506714).
posY_t1(10018,1,4.275920391082764).
posX_t1(10018,2,-1.8191182613372803).
posY_t1(10018,2,-5.212826728820801).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20018
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20018,0,cube).
shape(20018,1,sphere).
shape(20018,2,cylinder).
%Timestep 0
posX_t0(20018,0,-3.08334974792738).
posY_t0(20018,0,-3.3905281260325437).
posX_t0(20018,1,1.0979212522506714).
posY_t0(20018,1,4.275920391082764).
posX_t0(20018,2,-1.8191182613372803).
posY_t0(20018,2,-5.212826728820801).
right_of_t0(20018,0,1).
right_of_t0(20018,2,0).
right_of_t0(20018,2,1).
left_of_t0(20018,0,2).
left_of_t0(20018,1,0).
left_of_t0(20018,1,2).
good_cube(20018,0,false).
good_sphere(20018,1,true).
goal(20018,false).
move_t0(20018,0,left).
%Timestep 1
posX_t1(20018,0,-3.6601262572636464).
posY_t1(20018,0,-3.96730463536881).
posX_t1(20018,1,1.0979212522506714).
posY_t1(20018,1,4.275920391082764).
posX_t1(20018,2,-1.8191182613372803).
posY_t1(20018,2,-5.212826728820801).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 19
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(19,0,cube).
shape(19,1,sphere).
shape(19,2,cylinder).
%Timestep 0
posX_t0(19,0,-3.6975746154785156).
posY_t0(19,0,-2.7971646785736084).
posX_t0(19,1,3.341526746749878).
posY_t0(19,1,-1.3718339204788208).
posX_t0(19,2,0.4315657615661621).
posY_t0(19,2,5.36394739151001).
right_of_t0(19,0,1).
right_of_t0(19,0,2).
right_of_t0(19,1,2).
left_of_t0(19,1,0).
left_of_t0(19,2,0).
left_of_t0(19,2,1).
good_cube(19,0,true).
good_sphere(19,1,false).
goal(19,false).
move_t0(19,1,right).
%Timestep 1
posX_t1(19,0,-3.6975746154785156).
posY_t1(19,0,-2.7971646785736084).
posX_t1(19,1,3.953507297549258).
posY_t1(19,1,-0.7598533696794408).
posX_t1(19,2,0.4315657615661621).
posY_t1(19,2,5.36394739151001).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10019
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10019,0,cube).
shape(10019,1,sphere).
shape(10019,2,cylinder).
%Timestep 0
posX_t0(10019,0,-3.6975746154785156).
posY_t0(10019,0,-2.7971646785736084).
posX_t0(10019,1,3.953507297549258).
posY_t0(10019,1,-0.7598533696794408).
posX_t0(10019,2,0.4315657615661621).
posY_t0(10019,2,5.36394739151001).
right_of_t0(10019,0,1).
right_of_t0(10019,0,2).
right_of_t0(10019,1,2).
left_of_t0(10019,1,0).
left_of_t0(10019,2,0).
left_of_t0(10019,2,1).
good_cube(10019,0,true).
good_sphere(10019,1,false).
goal(10019,false).
move_t0(10019,1,right).
%Timestep 1
posX_t1(10019,0,-3.6975746154785156).
posY_t1(10019,0,-2.7971646785736084).
posX_t1(10019,1,4.535929576321512).
posY_t1(10019,1,-0.17743109090718678).
posX_t1(10019,2,0.4315657615661621).
posY_t1(10019,2,5.36394739151001).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20019
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20019,0,cube).
shape(20019,1,sphere).
shape(20019,2,cylinder).
%Timestep 0
posX_t0(20019,0,-3.6975746154785156).
posY_t0(20019,0,-2.7971646785736084).
posX_t0(20019,1,4.535929576321512).
posY_t0(20019,1,-0.17743109090718678).
posX_t0(20019,2,0.4315657615661621).
posY_t0(20019,2,5.36394739151001).
right_of_t0(20019,0,1).
right_of_t0(20019,0,2).
right_of_t0(20019,1,2).
left_of_t0(20019,1,0).
left_of_t0(20019,2,0).
left_of_t0(20019,2,1).
good_cube(20019,0,true).
good_sphere(20019,1,false).
goal(20019,false).
move_t0(20019,1,right).
%Timestep 1
posX_t1(20019,0,-3.6975746154785156).
posY_t1(20019,0,-2.7971646785736084).
posX_t1(20019,1,5.369906950841497).
posY_t1(20019,1,0.6565462836127991).
posX_t1(20019,2,0.4315657615661621).
posY_t1(20019,2,5.36394739151001).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30019
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30019,0,cube).
shape(30019,1,sphere).
shape(30019,2,cylinder).
%Timestep 0
posX_t0(30019,0,-3.6975746154785156).
posY_t0(30019,0,-2.7971646785736084).
posX_t0(30019,1,5.369906950841497).
posY_t0(30019,1,0.6565462836127991).
posX_t0(30019,2,0.4315657615661621).
posY_t0(30019,2,5.36394739151001).
right_of_t0(30019,0,1).
right_of_t0(30019,0,2).
left_of_t0(30019,1,0).
left_of_t0(30019,2,0).
good_cube(30019,0,true).
good_sphere(30019,1,false).
goal(30019,false).
move_t0(30019,1,right).
%Timestep 1
posX_t1(30019,0,-3.6975746154785156).
posY_t1(30019,0,-2.7971646785736084).
posX_t1(30019,1,6.049271943583956).
posY_t1(30019,1,1.3359112763552574).
posX_t1(30019,2,0.4315657615661621).
posY_t1(30019,2,5.36394739151001).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20,0,cube).
shape(20,1,sphere).
shape(20,2,cylinder).
%Timestep 0
posX_t0(20,0,1.2789599895477295).
posY_t0(20,0,4.138781547546387).
posX_t0(20,1,4.1493096351623535).
posY_t0(20,1,-0.9525533318519592).
posX_t0(20,2,-3.693817138671875).
posY_t0(20,2,2.290310859680176).
right_of_t0(20,1,0).
right_of_t0(20,2,0).
right_of_t0(20,2,1).
left_of_t0(20,0,1).
left_of_t0(20,0,2).
left_of_t0(20,1,2).
good_cube(20,0,false).
good_sphere(20,1,true).
goal(20,false).
move_t0(20,0,left).
%Timestep 1
posX_t1(20,0,0.569132312235315).
posY_t1(20,0,3.4289538702339724).
posX_t1(20,1,4.1493096351623535).
posY_t1(20,1,-0.9525533318519592).
posX_t1(20,2,-3.693817138671875).
posY_t1(20,2,2.290310859680176).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10020
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10020,0,cube).
shape(10020,1,sphere).
shape(10020,2,cylinder).
%Timestep 0
posX_t0(10020,0,0.569132312235315).
posY_t0(10020,0,3.4289538702339724).
posX_t0(10020,1,4.1493096351623535).
posY_t0(10020,1,-0.9525533318519592).
posX_t0(10020,2,-3.693817138671875).
posY_t0(10020,2,2.290310859680176).
right_of_t0(10020,1,0).
right_of_t0(10020,2,0).
right_of_t0(10020,2,1).
left_of_t0(10020,0,1).
left_of_t0(10020,0,2).
left_of_t0(10020,1,2).
good_cube(10020,0,false).
good_sphere(10020,1,true).
goal(10020,false).
move_t0(10020,0,left).
%Timestep 1
posX_t1(10020,0,-0.046505253216333364).
posY_t1(10020,0,2.813316304782324).
posX_t1(10020,1,4.1493096351623535).
posY_t1(10020,1,-0.9525533318519592).
posX_t1(10020,2,-3.693817138671875).
posY_t1(10020,2,2.290310859680176).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20020
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20020,0,cube).
shape(20020,1,sphere).
shape(20020,2,cylinder).
%Timestep 0
posX_t0(20020,0,-0.046505253216333364).
posY_t0(20020,0,2.813316304782324).
posX_t0(20020,1,4.1493096351623535).
posY_t0(20020,1,-0.9525533318519592).
posX_t0(20020,2,-3.693817138671875).
posY_t0(20020,2,2.290310859680176).
right_of_t0(20020,0,1).
right_of_t0(20020,2,0).
right_of_t0(20020,2,1).
left_of_t0(20020,0,2).
left_of_t0(20020,1,0).
left_of_t0(20020,1,2).
good_cube(20020,0,false).
good_sphere(20020,1,true).
goal(20020,false).
move_t0(20020,0,left).
%Timestep 1
posX_t1(20020,0,-0.7889260184545288).
posY_t1(20020,0,2.0708955395441286).
posX_t1(20020,1,4.1493096351623535).
posY_t1(20020,1,-0.9525533318519592).
posX_t1(20020,2,-3.693817138671875).
posY_t1(20020,2,2.290310859680176).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30020
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30020,0,cube).
shape(30020,1,sphere).
shape(30020,2,cylinder).
%Timestep 0
posX_t0(30020,0,-0.7889260184545288).
posY_t0(30020,0,2.0708955395441286).
posX_t0(30020,1,4.1493096351623535).
posY_t0(30020,1,-0.9525533318519592).
posX_t0(30020,2,-3.693817138671875).
posY_t0(30020,2,2.290310859680176).
right_of_t0(30020,0,1).
right_of_t0(30020,2,0).
right_of_t0(30020,2,1).
left_of_t0(30020,0,2).
left_of_t0(30020,1,0).
left_of_t0(30020,1,2).
good_cube(30020,0,false).
good_sphere(30020,1,true).
goal(30020,false).
move_t0(30020,0,left).
%Timestep 1
posX_t1(30020,0,-1.4034794385534486).
posY_t1(30020,0,1.456342119445209).
posX_t1(30020,1,4.1493096351623535).
posY_t1(30020,1,-0.9525533318519592).
posX_t1(30020,2,-3.693817138671875).
posY_t1(30020,2,2.290310859680176).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 40020
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(40020,0,cube).
shape(40020,1,sphere).
shape(40020,2,cylinder).
%Timestep 0
posX_t0(40020,0,-1.4034794385534486).
posY_t0(40020,0,1.456342119445209).
posX_t0(40020,1,4.1493096351623535).
posY_t0(40020,1,-0.9525533318519592).
posX_t0(40020,2,-3.693817138671875).
posY_t0(40020,2,2.290310859680176).
right_of_t0(40020,0,1).
right_of_t0(40020,2,0).
right_of_t0(40020,2,1).
left_of_t0(40020,0,2).
left_of_t0(40020,1,0).
left_of_t0(40020,1,2).
good_cube(40020,0,false).
good_sphere(40020,1,true).
goal(40020,false).
move_t0(40020,0,left).
%Timestep 1
posX_t1(40020,0,-2.2096272921465547).
posY_t1(40020,0,0.6501942658521029).
posX_t1(40020,1,4.1493096351623535).
posY_t1(40020,1,-0.9525533318519592).
posX_t1(40020,2,-3.693817138671875).
posY_t1(40020,2,2.290310859680176).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 50020
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(50020,0,cube).
shape(50020,1,sphere).
shape(50020,2,cylinder).
%Timestep 0
posX_t0(50020,0,-2.2096272921465547).
posY_t0(50020,0,0.6501942658521029).
posX_t0(50020,1,4.1493096351623535).
posY_t0(50020,1,-0.9525533318519592).
posX_t0(50020,2,-3.693817138671875).
posY_t0(50020,2,2.290310859680176).
right_of_t0(50020,0,1).
right_of_t0(50020,2,1).
left_of_t0(50020,1,0).
left_of_t0(50020,1,2).
blocked_t0(50020,2,front).
blocked_t0(50020,0,behind).
good_cube(50020,0,false).
good_sphere(50020,1,true).
goal(50020,false).
move_t0(50020,0,left).
%Timestep 1
posX_t1(50020,0,-2.9071479828379307).
posY_t1(50020,0,-0.04732642483927296).
posX_t1(50020,1,4.1493096351623535).
posY_t1(50020,1,-0.9525533318519592).
posX_t1(50020,2,-3.693817138671875).
posY_t1(50020,2,2.290310859680176).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 21
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(21,0,cube).
shape(21,1,sphere).
shape(21,2,cylinder).
%Timestep 0
posX_t0(21,0,3.7443430423736572).
posY_t0(21,0,-2.9517486095428467).
posX_t0(21,1,-3.024193286895752).
posY_t0(21,1,-5.489288330078125).
posX_t0(21,2,-4.731186866760254).
posY_t0(21,2,1.7708135843276978).
right_of_t0(21,1,0).
right_of_t0(21,1,2).
right_of_t0(21,2,0).
left_of_t0(21,0,1).
left_of_t0(21,0,2).
left_of_t0(21,2,1).
good_cube(21,0,false).
good_sphere(21,1,false).
goal(21,false).
move_t0(21,0,left).
%Timestep 1
posX_t1(21,0,3.017719495290841).
posY_t1(21,0,-3.678372156625663).
posX_t1(21,1,-3.024193286895752).
posY_t1(21,1,-5.489288330078125).
posX_t1(21,2,-4.731186866760254).
posY_t1(21,2,1.7708135843276978).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10021
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10021,0,cube).
shape(10021,1,sphere).
shape(10021,2,cylinder).
%Timestep 0
posX_t0(10021,0,3.017719495290841).
posY_t0(10021,0,-3.678372156625663).
posX_t0(10021,1,-3.024193286895752).
posY_t0(10021,1,-5.489288330078125).
posX_t0(10021,2,-4.731186866760254).
posY_t0(10021,2,1.7708135843276978).
right_of_t0(10021,1,0).
right_of_t0(10021,1,2).
right_of_t0(10021,2,0).
left_of_t0(10021,0,1).
left_of_t0(10021,0,2).
left_of_t0(10021,2,1).
good_cube(10021,0,false).
good_sphere(10021,1,false).
goal(10021,false).
move_t0(10021,0,left).
%Timestep 1
posX_t1(10021,0,2.425331290259791).
posY_t1(10021,0,-4.270760361656713).
posX_t1(10021,1,-3.024193286895752).
posY_t1(10021,1,-5.489288330078125).
posX_t1(10021,2,-4.731186866760254).
posY_t1(10021,2,1.7708135843276978).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20021
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20021,0,cube).
shape(20021,1,sphere).
shape(20021,2,cylinder).
%Timestep 0
posX_t0(20021,0,2.425331290259791).
posY_t0(20021,0,-4.270760361656713).
posX_t0(20021,1,-3.024193286895752).
posY_t0(20021,1,-5.489288330078125).
posX_t0(20021,2,-4.731186866760254).
posY_t0(20021,2,1.7708135843276978).
right_of_t0(20021,1,0).
right_of_t0(20021,1,2).
right_of_t0(20021,2,0).
left_of_t0(20021,0,1).
left_of_t0(20021,0,2).
left_of_t0(20021,2,1).
good_cube(20021,0,false).
good_sphere(20021,1,false).
goal(20021,false).
move_t0(20021,0,left).
%Timestep 1
posX_t1(20021,0,1.7216129080819988).
posY_t1(20021,0,-4.9744787438345055).
posX_t1(20021,1,-3.024193286895752).
posY_t1(20021,1,-5.489288330078125).
posX_t1(20021,2,-4.731186866760254).
posY_t1(20021,2,1.7708135843276978).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30021
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30021,0,cube).
shape(30021,1,sphere).
shape(30021,2,cylinder).
%Timestep 0
posX_t0(30021,0,1.7216129080819988).
posY_t0(30021,0,-4.9744787438345055).
posX_t0(30021,1,-3.024193286895752).
posY_t0(30021,1,-5.489288330078125).
posX_t0(30021,2,-4.731186866760254).
posY_t0(30021,2,1.7708135843276978).
right_of_t0(30021,0,2).
right_of_t0(30021,1,0).
right_of_t0(30021,1,2).
left_of_t0(30021,0,1).
left_of_t0(30021,2,0).
left_of_t0(30021,2,1).
good_cube(30021,0,true).
good_sphere(30021,1,false).
goal(30021,false).
move_t0(30021,1,right).
%Timestep 1
posX_t1(30021,0,1.7216129080819988).
posY_t1(30021,0,-4.9744787438345055).
posX_t1(30021,1,-2.4159660953683098).
posY_t1(30021,1,-4.881061138550683).
posX_t1(30021,2,-4.731186866760254).
posY_t1(30021,2,1.7708135843276978).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 40021
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(40021,0,cube).
shape(40021,1,sphere).
shape(40021,2,cylinder).
%Timestep 0
posX_t0(40021,0,1.7216129080819988).
posY_t0(40021,0,-4.9744787438345055).
posX_t0(40021,1,-2.4159660953683098).
posY_t0(40021,1,-4.881061138550683).
posX_t0(40021,2,-4.731186866760254).
posY_t0(40021,2,1.7708135843276978).
right_of_t0(40021,0,2).
right_of_t0(40021,1,0).
right_of_t0(40021,1,2).
left_of_t0(40021,0,1).
left_of_t0(40021,2,0).
left_of_t0(40021,2,1).
good_cube(40021,0,true).
good_sphere(40021,1,false).
goal(40021,false).
move_t0(40021,1,right).
%Timestep 1
posX_t1(40021,0,1.7216129080819988).
posY_t1(40021,0,-4.9744787438345055).
posX_t1(40021,1,-1.7016405530051593).
posY_t1(40021,1,-4.166735596187532).
posX_t1(40021,2,-4.731186866760254).
posY_t1(40021,2,1.7708135843276978).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 50021
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(50021,0,cube).
shape(50021,1,sphere).
shape(50021,2,cylinder).
%Timestep 0
posX_t0(50021,0,1.7216129080819988).
posY_t0(50021,0,-4.9744787438345055).
posX_t0(50021,1,-1.7016405530051593).
posY_t0(50021,1,-4.166735596187532).
posX_t0(50021,2,-4.731186866760254).
posY_t0(50021,2,1.7708135843276978).
right_of_t0(50021,0,2).
right_of_t0(50021,1,0).
right_of_t0(50021,1,2).
left_of_t0(50021,0,1).
left_of_t0(50021,2,0).
left_of_t0(50021,2,1).
good_cube(50021,0,true).
good_sphere(50021,1,false).
goal(50021,false).
move_t0(50021,1,right).
%Timestep 1
posX_t1(50021,0,1.7216129080819988).
posY_t1(50021,0,-4.9744787438345055).
posX_t1(50021,1,-0.9219627097228315).
posY_t1(50021,1,-3.3870577529052044).
posX_t1(50021,2,-4.731186866760254).
posY_t1(50021,2,1.7708135843276978).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 60021
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(60021,0,cube).
shape(60021,1,sphere).
shape(60021,2,cylinder).
%Timestep 0
posX_t0(60021,0,1.7216129080819988).
posY_t0(60021,0,-4.9744787438345055).
posX_t0(60021,1,-0.9219627097228315).
posY_t0(60021,1,-3.3870577529052044).
posX_t0(60021,2,-4.731186866760254).
posY_t0(60021,2,1.7708135843276978).
right_of_t0(60021,0,2).
right_of_t0(60021,1,0).
right_of_t0(60021,1,2).
left_of_t0(60021,0,1).
left_of_t0(60021,2,0).
left_of_t0(60021,2,1).
good_cube(60021,0,true).
good_sphere(60021,1,false).
goal(60021,false).
move_t0(60021,1,right).
%Timestep 1
posX_t1(60021,0,1.7216129080819988).
posY_t1(60021,0,-4.9744787438345055).
posX_t1(60021,1,-0.20820873912138405).
posY_t1(60021,1,-2.673303782303757).
posX_t1(60021,2,-4.731186866760254).
posY_t1(60021,2,1.7708135843276978).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 70021
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(70021,0,cube).
shape(70021,1,sphere).
shape(70021,2,cylinder).
%Timestep 0
posX_t0(70021,0,1.7216129080819988).
posY_t0(70021,0,-4.9744787438345055).
posX_t0(70021,1,-0.20820873912138405).
posY_t0(70021,1,-2.673303782303757).
posX_t0(70021,2,-4.731186866760254).
posY_t0(70021,2,1.7708135843276978).
right_of_t0(70021,0,1).
right_of_t0(70021,0,2).
left_of_t0(70021,1,0).
left_of_t0(70021,2,0).
good_cube(70021,0,true).
good_sphere(70021,1,false).
goal(70021,false).
move_t0(70021,1,right).
%Timestep 1
posX_t1(70021,0,1.7216129080819988).
posY_t1(70021,0,-4.9744787438345055).
posX_t1(70021,1,0.3963041874901331).
posY_t1(70021,1,-2.06879085569224).
posX_t1(70021,2,-4.731186866760254).
posY_t1(70021,2,1.7708135843276978).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 22
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(22,0,cube).
shape(22,1,sphere).
shape(22,2,cylinder).
%Timestep 0
posX_t0(22,0,-2.7266414165496826).
posY_t0(22,0,1.064864993095398).
posX_t0(22,1,2.720245599746704).
posY_t0(22,1,2.6638383865356445).
posX_t0(22,2,3.0418782234191895).
posY_t0(22,2,-4.339413642883301).
right_of_t0(22,0,1).
right_of_t0(22,0,2).
right_of_t0(22,2,1).
left_of_t0(22,1,0).
left_of_t0(22,1,2).
left_of_t0(22,2,0).
good_cube(22,0,true).
good_sphere(22,1,true).
goal(22,true).
%Timestep 1
posX_t1(22,0,-2.7266414165496826).
posY_t1(22,0,1.064864993095398).
posX_t1(22,1,2.720245599746704).
posY_t1(22,1,2.6638383865356445).
posX_t1(22,2,3.0418782234191895).
posY_t1(22,2,-4.339413642883301).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 23
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(23,0,cube).
shape(23,1,sphere).
shape(23,2,cylinder).
%Timestep 0
posX_t0(23,0,2.0541696548461914).
posY_t0(23,0,1.9906916618347168).
posX_t0(23,1,2.669872760772705).
posY_t0(23,1,4.588876247406006).
posX_t0(23,2,-2.827380895614624).
posY_t0(23,2,3.77880859375).
right_of_t0(23,0,1).
right_of_t0(23,2,0).
right_of_t0(23,2,1).
left_of_t0(23,0,2).
left_of_t0(23,1,0).
left_of_t0(23,1,2).
good_cube(23,0,false).
good_sphere(23,1,true).
goal(23,false).
move_t0(23,0,left).
%Timestep 1
posX_t1(23,0,1.4212626186131407).
posY_t1(23,0,1.3577846256016661).
posX_t1(23,1,2.669872760772705).
posY_t1(23,1,4.588876247406006).
posX_t1(23,2,-2.827380895614624).
posY_t1(23,2,3.77880859375).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10023
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10023,0,cube).
shape(10023,1,sphere).
shape(10023,2,cylinder).
%Timestep 0
posX_t0(10023,0,1.4212626186131407).
posY_t0(10023,0,1.3577846256016661).
posX_t0(10023,1,2.669872760772705).
posY_t0(10023,1,4.588876247406006).
posX_t0(10023,2,-2.827380895614624).
posY_t0(10023,2,3.77880859375).
right_of_t0(10023,0,1).
right_of_t0(10023,2,0).
right_of_t0(10023,2,1).
left_of_t0(10023,0,2).
left_of_t0(10023,1,0).
left_of_t0(10023,1,2).
good_cube(10023,0,false).
good_sphere(10023,1,true).
goal(10023,false).
move_t0(10023,0,left).
%Timestep 1
posX_t1(10023,0,0.853931124705679).
posY_t1(10023,0,0.7904531316942044).
posX_t1(10023,1,2.669872760772705).
posY_t1(10023,1,4.588876247406006).
posX_t1(10023,2,-2.827380895614624).
posY_t1(10023,2,3.77880859375).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20023
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20023,0,cube).
shape(20023,1,sphere).
shape(20023,2,cylinder).
%Timestep 0
posX_t0(20023,0,0.853931124705679).
posY_t0(20023,0,0.7904531316942044).
posX_t0(20023,1,2.669872760772705).
posY_t0(20023,1,4.588876247406006).
posX_t0(20023,2,-2.827380895614624).
posY_t0(20023,2,3.77880859375).
right_of_t0(20023,0,1).
right_of_t0(20023,2,0).
right_of_t0(20023,2,1).
left_of_t0(20023,0,2).
left_of_t0(20023,1,0).
left_of_t0(20023,1,2).
good_cube(20023,0,false).
good_sphere(20023,1,true).
goal(20023,false).
move_t0(20023,0,left).
%Timestep 1
posX_t1(20023,0,0.2006497443123033).
posY_t1(20023,0,0.1371717513008287).
posX_t1(20023,1,2.669872760772705).
posY_t1(20023,1,4.588876247406006).
posX_t1(20023,2,-2.827380895614624).
posY_t1(20023,2,3.77880859375).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 25
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(25,0,cube).
shape(25,1,sphere).
shape(25,2,cylinder).
%Timestep 0
posX_t0(25,0,1.4711809158325195).
posY_t0(25,0,-2.9543521404266357).
posX_t0(25,1,1.2360855340957642).
posY_t0(25,1,5.28458309173584).
posX_t0(25,2,1.348386526107788).
posY_t0(25,2,3.358701229095459).
right_of_t0(25,0,1).
right_of_t0(25,0,2).
right_of_t0(25,2,1).
left_of_t0(25,1,0).
left_of_t0(25,1,2).
left_of_t0(25,2,0).
good_cube(25,0,true).
good_sphere(25,1,true).
goal(25,true).
%Timestep 1
posX_t1(25,0,1.4711809158325195).
posY_t1(25,0,-2.9543521404266357).
posX_t1(25,1,1.2360855340957642).
posY_t1(25,1,5.28458309173584).
posX_t1(25,2,1.348386526107788).
posY_t1(25,2,3.358701229095459).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 29
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(29,0,cube).
shape(29,1,sphere).
shape(29,2,cylinder).
%Timestep 0
posX_t0(29,0,0.0654878169298172).
posY_t0(29,0,-4.0469207763671875).
posX_t0(29,1,-3.942063808441162).
posY_t0(29,1,1.9548792839050293).
posX_t0(29,2,0.1704125553369522).
posY_t0(29,2,1.9979825019836426).
right_of_t0(29,0,1).
right_of_t0(29,0,2).
right_of_t0(29,1,2).
left_of_t0(29,1,0).
left_of_t0(29,2,0).
left_of_t0(29,2,1).
good_cube(29,0,true).
good_sphere(29,1,false).
goal(29,false).
move_t0(29,1,right).
%Timestep 1
posX_t1(29,0,0.0654878169298172).
posY_t1(29,0,-4.0469207763671875).
posX_t1(29,1,-3.159949046590247).
posY_t1(29,1,2.7369940457559445).
posX_t1(29,2,0.1704125553369522).
posY_t1(29,2,1.9979825019836426).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10029
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10029,0,cube).
shape(10029,1,sphere).
shape(10029,2,cylinder).
%Timestep 0
posX_t0(10029,0,0.0654878169298172).
posY_t0(10029,0,-4.0469207763671875).
posX_t0(10029,1,-3.159949046590247).
posY_t0(10029,1,2.7369940457559445).
posX_t0(10029,2,0.1704125553369522).
posY_t0(10029,2,1.9979825019836426).
right_of_t0(10029,0,1).
right_of_t0(10029,0,2).
right_of_t0(10029,1,2).
left_of_t0(10029,1,0).
left_of_t0(10029,2,0).
left_of_t0(10029,2,1).
good_cube(10029,0,true).
good_sphere(10029,1,false).
goal(10029,false).
move_t0(10029,1,right).
%Timestep 1
posX_t1(10029,0,0.0654878169298172).
posY_t1(10029,0,-4.0469207763671875).
posX_t1(10029,1,-2.498569562313695).
posY_t1(10029,1,3.3983735300324964).
posX_t1(10029,2,0.1704125553369522).
posY_t1(10029,2,1.9979825019836426).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20029
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20029,0,cube).
shape(20029,1,sphere).
shape(20029,2,cylinder).
%Timestep 0
posX_t0(20029,0,0.0654878169298172).
posY_t0(20029,0,-4.0469207763671875).
posX_t0(20029,1,-2.498569562313695).
posY_t0(20029,1,3.3983735300324964).
posX_t0(20029,2,0.1704125553369522).
posY_t0(20029,2,1.9979825019836426).
right_of_t0(20029,0,1).
right_of_t0(20029,0,2).
right_of_t0(20029,1,2).
left_of_t0(20029,1,0).
left_of_t0(20029,2,0).
left_of_t0(20029,2,1).
good_cube(20029,0,true).
good_sphere(20029,1,false).
goal(20029,false).
move_t0(20029,1,right).
%Timestep 1
posX_t1(20029,0,0.0654878169298172).
posY_t1(20029,0,-4.0469207763671875).
posX_t1(20029,1,-1.7226345245276993).
posY_t1(20029,1,4.174308567818493).
posX_t1(20029,2,0.1704125553369522).
posY_t1(20029,2,1.9979825019836426).
