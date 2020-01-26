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
:- discontiguous shape/3.
:- discontiguous move_t0/3.
:- discontiguous move_right_of/3.
:- discontiguous posX_t0/3.
:- discontiguous posX_t1/3.
:- discontiguous posY_t0/3.
:- discontiguous posY_t1/3.
:- discontiguous blocked_t0/3.
:- discontiguous blocked_t1/3.
:- discontiguous front_of_t0/3.
:- discontiguous front_of_t1/3.
:- discontiguous right_of_t0/3.
:- discontiguous right_of_t1/3.
:- discontiguous behind_of_t0/3.
:- discontiguous behind_of_t1/3.
:- discontiguous left_of_t0/3.
:- discontiguous left_of_t1/3.

%Dynamics
:- dynamic shape/3.
:- dynamic move_t0/3.
:- dynamic move_right_of/3.
:- dynamic posX_t0/3.
:- dynamic posX_t1/3.
:- dynamic posY_t0/3.
:- dynamic posY_t1/3.
:- dynamic blocked_t0/3.
:- dynamic blocked_t1/3.
:- dynamic front_of_t0/3.
:- dynamic front_of_t1/3.
:- dynamic right_of_t0/3.
:- dynamic right_of_t1/3.
:- dynamic behind_of_t0/3.
:- dynamic behind_of_t1/3.
:- dynamic left_of_t0/3.
:- dynamic left_of_t1/3.

%Types
base(shape(w,i,sh)).
base(move_t0(w,i,dir)).
base(move_right_of(w,i,i)).
base(posX_t0(w,i,x)).
base(posX_t1(w,i,x)).
base(posY_t0(w,i,x)).
base(posY_t1(w,i,x)).
base(blocked_t0(w,i,dir)).
base(blocked_t1(w,i,dir)).
base(front_of_t0(w,i,i)).
base(front_of_t1(w,i,i)).
base(right_of_t0(w,i,i)).
base(right_of_t1(w,i,i)).
base(behind_of_t0(w,i,i)).
base(behind_of_t1(w,i,i)).
base(left_of_t0(w,i,i)).
base(left_of_t1(w,i,i)).

%Modes
mode(posX_t1,none,shape(+,+,-)).
mode(posX_t1,none,move_t0(+,+,-)).
mode(posX_t1,maxMod,(move_right_of(+,+,-),right_of_t0(+,-,+))).
mode(posX_t1,none,posX_t0(+,+,-)).
mode(posX_t1,none,blocked_t0(+,+,-)).
mode(posX_t1,minMod,(front_of_t0(+,-,+),shape(+,+,-))).
mode(posX_t1,none,right_of_t0(+,+,-)).
mode(posX_t1,minMod,(behind_of_t0(+,-,+),shape(+,+,-))).
mode(posX_t1,minMod,(left_of_t0(+,-,+),shape(+,+,-))).

mode(posY_t1,none,shape(+,+,-)).
mode(posY_t1,none,move_t0(+,+,-)).
mode(posY_t1,maxMod,(move_right_of(+,+,-),right_of_t0(+,-,+))).
mode(posY_t1,none,posY_t0(+,+,-)).
mode(posY_t1,none,blocked_t0(+,+,-)).
mode(posY_t1,minMod,(front_of_t0(+,-,+),shape(+,+,-))).
mode(posY_t1,none,right_of_t0(+,+,-)).
mode(posY_t1,minMod,(behind_of_t0(+,-,+),shape(+,+,-))).
mode(posY_t1,minMod,(left_of_t0(+,-,+),shape(+,+,-))).

mode(move_t0,none,blocked_t0(+,+,+)).
mode(move_t0,none,shape(+,+,-)).
mode(move_t0,minMod,(front_of_t0(+,-,+),shape(+,+,-))).
mode(move_t0,minMod,(behind_of_t0(+,-,+),shape(+,+,-))).
mode(move_t0,minMod,(left_of_t0(+,-,+),shape(+,+,-))).


%Aggregations
agg(none).
agg(minMod).
agg(maxMod).
agg(cnt).

%Threshold
thres(shape, 3, discrete, [cube,sphere,cylinder]).
thres(move_t0, 3, discrete, [right,behind,front,left]).
thres(move_right_of, 3, discrete, []).
thres(posX_t0, 3, continuous, []).
thres(posX_t1, 3, continuous, []).
thres(posY_t0, 3, continuous, []).
thres(posY_t1, 3, continuous, []).
thres(blocked_t0, 3, discrete, [right,behind,front,left]).
thres(blocked_t1, 3, discrete, [right,behind,front,left]).
thres(front_of_t0, 3, discrete, []).
thres(left_of_t0, 3, discrete, []).
thres(behind_of_t0, 3, discrete, []).
thres(right_of_t0, 3, discrete, []).
thres(front_of_t1, 3, discrete, []).
thres(left_of_t1, 3, discrete, []).
thres(behind_of_t1, 3, discrete, []).
thres(right_of_t1, 3, discrete, []).

%Targets
learn(posX_t1, 3, 3, continuous).
learn(posY_t1, 3, 3, continuous).
learn(move_t0,3,3,discrete).

%moving only if possible
%move_t0(W,I,Dir) :-  \+blocked_t0(W,I,Dir).

%%%%%%%%%%% Declarative Bias ends here %%%%%%%%%%%%

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 0
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(0,0,cube).
shape(0,1,sphere).
%Timestep 0
posX_t0(0,0,-3.031357765197754).
posY_t0(0,0,-1.926156759262085).
posX_t0(0,1,-0.2884683310985565).
posY_t0(0,1,1.301581621170044).
front_of_t0(0,1,0).
right_of_t0(0,0,1).
behind_of_t0(0,0,1).
left_of_t0(0,1,0).
blocked_t0(0,1,left).
move_t0(0,0,right).
move_right_of(0,0,1).
%Timestep 1
posX_t1(0,0,-2.3255119479831916).
posY_t1(0,0,-1.2203109420475227).
posX_t1(0,1,-0.2884683310985565).
posY_t1(0,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10000,0,cube).
shape(10000,1,sphere).
%Timestep 0
posX_t0(10000,0,-2.3255119479831916).
posY_t0(10000,0,-1.2203109420475227).
posX_t0(10000,1,-0.2884683310985565).
posY_t0(10000,1,1.301581621170044).
front_of_t0(10000,1,0).
right_of_t0(10000,0,1).
behind_of_t0(10000,0,1).
left_of_t0(10000,1,0).
blocked_t0(10000,0,front).
blocked_t0(10000,0,right).
blocked_t0(10000,1,left).
move_t0(10000,0,behind).
move_right_of(10000,0,1).
%Timestep 1
posX_t1(10000,0,-3.03270007370853).
posY_t1(10000,0,-0.5131226970991789).
posX_t1(10000,1,-0.2884683310985565).
posY_t1(10000,1,1.301581621170044).
%##################################################################
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111101,0,cube).
shape(1111101,1,sphere).
%Timestep 0
posX_t0(1111101,0,-2.3255119479831916).
posY_t0(1111101,0,-1.2203109420475227).
posX_t0(1111101,1,-0.2884683310985565).
posY_t0(1111101,1,1.301581621170044).
front_of_t0(1111101,1,0).
right_of_t0(1111101,0,1).
behind_of_t0(1111101,0,1).
left_of_t0(1111101,1,0).
blocked_t0(1111101,0,front).
blocked_t0(1111101,0,right).
blocked_t0(1111101,1,left).
move_t0(1111101,0,behind).
move_right_of(1111101,0,1).
%Timestep 1
posX_t1(1111101,0,-3.03270007370853).
posY_t1(1111101,0,-0.5131226970991789).
posX_t1(1111101,1,-0.2884683310985565).
posY_t1(1111101,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111102,0,cube).
shape(1111102,1,sphere).
%Timestep 0
posX_t0(1111102,0,-2.3255119479831916).
posY_t0(1111102,0,-1.2203109420475227).
posX_t0(1111102,1,-0.2884683310985565).
posY_t0(1111102,1,1.301581621170044).
front_of_t0(1111102,1,0).
right_of_t0(1111102,0,1).
behind_of_t0(1111102,0,1).
left_of_t0(1111102,1,0).
blocked_t0(1111102,0,front).
blocked_t0(1111102,0,right).
blocked_t0(1111102,1,left).
move_t0(1111102,0,behind).
move_right_of(1111102,0,1).
%Timestep 1
posX_t1(1111102,0,-3.03270007370853).
posY_t1(1111102,0,-0.5131226970991789).
posX_t1(1111102,1,-0.2884683310985565).
posY_t1(1111102,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111103,0,cube).
shape(1111103,1,sphere).
%Timestep 0
posX_t0(1111103,0,-2.3255119479831916).
posY_t0(1111103,0,-1.2203109420475227).
posX_t0(1111103,1,-0.2884683310985565).
posY_t0(1111103,1,1.301581621170044).
front_of_t0(1111103,1,0).
right_of_t0(1111103,0,1).
behind_of_t0(1111103,0,1).
left_of_t0(1111103,1,0).
blocked_t0(1111103,0,front).
blocked_t0(1111103,0,right).
blocked_t0(1111103,1,left).
move_t0(1111103,0,behind).
move_right_of(1111103,0,1).
%Timestep 1
posX_t1(1111103,0,-3.03270007370853).
posY_t1(1111103,0,-0.5131226970991789).
posX_t1(1111103,1,-0.2884683310985565).
posY_t1(1111103,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111104,0,cube).
shape(1111104,1,sphere).
%Timestep 0
posX_t0(1111104,0,-2.3255119479831916).
posY_t0(1111104,0,-1.2203109420475227).
posX_t0(1111104,1,-0.2884683310985565).
posY_t0(1111104,1,1.301581621170044).
front_of_t0(1111104,1,0).
right_of_t0(1111104,0,1).
behind_of_t0(1111104,0,1).
left_of_t0(1111104,1,0).
blocked_t0(1111104,0,front).
blocked_t0(1111104,0,right).
blocked_t0(1111104,1,left).
move_t0(1111104,0,behind).
move_right_of(1111104,0,1).
%Timestep 1
posX_t1(1111104,0,-3.03270007370853).
posY_t1(1111104,0,-0.5131226970991789).
posX_t1(1111104,1,-0.2884683310985565).
posY_t1(1111104,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111105,0,cube).
shape(1111105,1,sphere).
%Timestep 0
posX_t0(1111105,0,-2.3255119479831916).
posY_t0(1111105,0,-1.2203109420475227).
posX_t0(1111105,1,-0.2884683310985565).
posY_t0(1111105,1,1.301581621170044).
front_of_t0(1111105,1,0).
right_of_t0(1111105,0,1).
behind_of_t0(1111105,0,1).
left_of_t0(1111105,1,0).
blocked_t0(1111105,0,front).
blocked_t0(1111105,0,right).
blocked_t0(1111105,1,left).
move_t0(1111105,0,behind).
move_right_of(1111105,0,1).
%Timestep 1
posX_t1(1111105,0,-3.03270007370853).
posY_t1(1111105,0,-0.5131226970991789).
posX_t1(1111105,1,-0.2884683310985565).
posY_t1(1111105,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111106,0,cube).
shape(1111106,1,sphere).
%Timestep 0
posX_t0(1111106,0,-2.3255119479831916).
posY_t0(1111106,0,-1.2203109420475227).
posX_t0(1111106,1,-0.2884683310985565).
posY_t0(1111106,1,1.301581621170044).
front_of_t0(1111106,1,0).
right_of_t0(1111106,0,1).
behind_of_t0(1111106,0,1).
left_of_t0(1111106,1,0).
blocked_t0(1111106,0,front).
blocked_t0(1111106,0,right).
blocked_t0(1111106,1,left).
move_t0(1111106,0,behind).
move_right_of(1111106,0,1).
%Timestep 1
posX_t1(1111106,0,-3.03270007370853).
posY_t1(1111106,0,-0.5131226970991789).
posX_t1(1111106,1,-0.2884683310985565).
posY_t1(1111106,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111107,0,cube).
shape(1111107,1,sphere).
%Timestep 0
posX_t0(1111107,0,-2.3255119479831916).
posY_t0(1111107,0,-1.2203109420475227).
posX_t0(1111107,1,-0.2884683310985565).
posY_t0(1111107,1,1.301581621170044).
front_of_t0(1111107,1,0).
right_of_t0(1111107,0,1).
behind_of_t0(1111107,0,1).
left_of_t0(1111107,1,0).
blocked_t0(1111107,0,front).
blocked_t0(1111107,0,right).
blocked_t0(1111107,1,left).
move_t0(1111107,0,behind).
move_right_of(1111107,0,1).
%Timestep 1
posX_t1(1111107,0,-3.03270007370853).
posY_t1(1111107,0,-0.5131226970991789).
posX_t1(1111107,1,-0.2884683310985565).
posY_t1(1111107,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111108,0,cube).
shape(1111108,1,sphere).
%Timestep 0
posX_t0(1111108,0,-2.3255119479831916).
posY_t0(1111108,0,-1.2203109420475227).
posX_t0(1111108,1,-0.2884683310985565).
posY_t0(1111108,1,1.301581621170044).
front_of_t0(1111108,1,0).
right_of_t0(1111108,0,1).
behind_of_t0(1111108,0,1).
left_of_t0(1111108,1,0).
blocked_t0(1111108,0,front).
blocked_t0(1111108,0,right).
blocked_t0(1111108,1,left).
move_t0(1111108,0,behind).
move_right_of(1111108,0,1).
%Timestep 1
posX_t1(1111108,0,-3.03270007370853).
posY_t1(1111108,0,-0.5131226970991789).
posX_t1(1111108,1,-0.2884683310985565).
posY_t1(1111108,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111109,0,cube).
shape(1111109,1,sphere).
%Timestep 0
posX_t0(1111109,0,-2.3255119479831916).
posY_t0(1111109,0,-1.2203109420475227).
posX_t0(1111109,1,-0.2884683310985565).
posY_t0(1111109,1,1.301581621170044).
front_of_t0(1111109,1,0).
right_of_t0(1111109,0,1).
behind_of_t0(1111109,0,1).
left_of_t0(1111109,1,0).
blocked_t0(1111109,0,front).
blocked_t0(1111109,0,right).
blocked_t0(1111109,1,left).
move_t0(1111109,0,behind).
move_right_of(1111109,0,1).
%Timestep 1
posX_t1(1111109,0,-3.03270007370853).
posY_t1(1111109,0,-0.5131226970991789).
posX_t1(1111109,1,-0.2884683310985565).
posY_t1(1111109,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111110,0,cube).
shape(1111110,1,sphere).
%Timestep 0
posX_t0(1111110,0,-2.3255119479831916).
posY_t0(1111110,0,-1.2203109420475227).
posX_t0(1111110,1,-0.2884683310985565).
posY_t0(1111110,1,1.301581621170044).
front_of_t0(1111110,1,0).
right_of_t0(1111110,0,1).
behind_of_t0(1111110,0,1).
left_of_t0(1111110,1,0).
blocked_t0(1111110,0,front).
blocked_t0(1111110,0,right).
blocked_t0(1111110,1,left).
move_t0(1111110,0,behind).
move_right_of(1111110,0,1).
%Timestep 1
posX_t1(1111110,0,-3.03270007370853).
posY_t1(1111110,0,-0.5131226970991789).
posX_t1(1111110,1,-0.2884683310985565).
posY_t1(1111110,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111111,0,cube).
shape(1111111,1,sphere).
%Timestep 0
posX_t0(1111111,0,-2.3255119479831916).
posY_t0(1111111,0,-1.2203109420475227).
posX_t0(1111111,1,-0.2884683310985565).
posY_t0(1111111,1,1.301581621170044).
front_of_t0(1111111,1,0).
right_of_t0(1111111,0,1).
behind_of_t0(1111111,0,1).
left_of_t0(1111111,1,0).
blocked_t0(1111111,0,front).
blocked_t0(1111111,0,right).
blocked_t0(1111111,1,left).
move_t0(1111111,0,behind).
move_right_of(1111111,0,1).
%Timestep 1
posX_t1(1111111,0,-3.03270007370853).
posY_t1(1111111,0,-0.5131226970991789).
posX_t1(1111111,1,-0.2884683310985565).
posY_t1(1111111,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111112,0,cube).
shape(1111112,1,sphere).
%Timestep 0
posX_t0(1111112,0,-2.3255119479831916).
posY_t0(1111112,0,-1.2203109420475227).
posX_t0(1111112,1,-0.2884683310985565).
posY_t0(1111112,1,1.301581621170044).
front_of_t0(1111112,1,0).
right_of_t0(1111112,0,1).
behind_of_t0(1111112,0,1).
left_of_t0(1111112,1,0).
blocked_t0(1111112,0,front).
blocked_t0(1111112,0,right).
blocked_t0(1111112,1,left).
move_t0(1111112,0,behind).
move_right_of(1111112,0,1).
%Timestep 1
posX_t1(1111112,0,-3.03270007370853).
posY_t1(1111112,0,-0.5131226970991789).
posX_t1(1111112,1,-0.2884683310985565).
posY_t1(1111112,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111113,0,cube).
shape(1111113,1,sphere).
%Timestep 0
posX_t0(1111113,0,-2.3255119479831916).
posY_t0(1111113,0,-1.2203109420475227).
posX_t0(1111113,1,-0.2884683310985565).
posY_t0(1111113,1,1.301581621170044).
front_of_t0(1111113,1,0).
right_of_t0(1111113,0,1).
behind_of_t0(1111113,0,1).
left_of_t0(1111113,1,0).
blocked_t0(1111113,0,front).
blocked_t0(1111113,0,right).
blocked_t0(1111113,1,left).
move_t0(1111113,0,behind).
move_right_of(1111113,0,1).
%Timestep 1
posX_t1(1111113,0,-3.03270007370853).
posY_t1(1111113,0,-0.5131226970991789).
posX_t1(1111113,1,-0.2884683310985565).
posY_t1(1111113,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111114,0,cube).
shape(1111114,1,sphere).
%Timestep 0
posX_t0(1111114,0,-2.3255119479831916).
posY_t0(1111114,0,-1.2203109420475227).
posX_t0(1111114,1,-0.2884683310985565).
posY_t0(1111114,1,1.301581621170044).
front_of_t0(1111114,1,0).
right_of_t0(1111114,0,1).
behind_of_t0(1111114,0,1).
left_of_t0(1111114,1,0).
blocked_t0(1111114,0,front).
blocked_t0(1111114,0,right).
blocked_t0(1111114,1,left).
move_t0(1111114,0,behind).
move_right_of(1111114,0,1).
%Timestep 1
posX_t1(1111114,0,-3.03270007370853).
posY_t1(1111114,0,-0.5131226970991789).
posX_t1(1111114,1,-0.2884683310985565).
posY_t1(1111114,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111115,0,cube).
shape(1111115,1,sphere).
%Timestep 0
posX_t0(1111115,0,-2.3255119479831916).
posY_t0(1111115,0,-1.2203109420475227).
posX_t0(1111115,1,-0.2884683310985565).
posY_t0(1111115,1,1.301581621170044).
front_of_t0(1111115,1,0).
right_of_t0(1111115,0,1).
behind_of_t0(1111115,0,1).
left_of_t0(1111115,1,0).
blocked_t0(1111115,0,front).
blocked_t0(1111115,0,right).
blocked_t0(1111115,1,left).
move_t0(1111115,0,behind).
move_right_of(1111115,0,1).
%Timestep 1
posX_t1(1111115,0,-3.03270007370853).
posY_t1(1111115,0,-0.5131226970991789).
posX_t1(1111115,1,-0.2884683310985565).
posY_t1(1111115,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111116,0,cube).
shape(1111116,1,sphere).
%Timestep 0
posX_t0(1111116,0,-2.3255119479831916).
posY_t0(1111116,0,-1.2203109420475227).
posX_t0(1111116,1,-0.2884683310985565).
posY_t0(1111116,1,1.301581621170044).
front_of_t0(1111116,1,0).
right_of_t0(1111116,0,1).
behind_of_t0(1111116,0,1).
left_of_t0(1111116,1,0).
blocked_t0(1111116,0,front).
blocked_t0(1111116,0,right).
blocked_t0(1111116,1,left).
move_t0(1111116,0,behind).
move_right_of(1111116,0,1).
%Timestep 1
posX_t1(1111116,0,-3.03270007370853).
posY_t1(1111116,0,-0.5131226970991789).
posX_t1(1111116,1,-0.2884683310985565).
posY_t1(1111116,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111117,0,cube).
shape(1111117,1,sphere).
%Timestep 0
posX_t0(1111117,0,-2.3255119479831916).
posY_t0(1111117,0,-1.2203109420475227).
posX_t0(1111117,1,-0.2884683310985565).
posY_t0(1111117,1,1.301581621170044).
front_of_t0(1111117,1,0).
right_of_t0(1111117,0,1).
behind_of_t0(1111117,0,1).
left_of_t0(1111117,1,0).
blocked_t0(1111117,0,front).
blocked_t0(1111117,0,right).
blocked_t0(1111117,1,left).
move_t0(1111117,0,behind).
move_right_of(1111117,0,1).
%Timestep 1
posX_t1(1111117,0,-3.03270007370853).
posY_t1(1111117,0,-0.5131226970991789).
posX_t1(1111117,1,-0.2884683310985565).
posY_t1(1111117,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111118,0,cube).
shape(1111118,1,sphere).
%Timestep 0
posX_t0(1111118,0,-2.3255119479831916).
posY_t0(1111118,0,-1.2203109420475227).
posX_t0(1111118,1,-0.2884683310985565).
posY_t0(1111118,1,1.301581621170044).
front_of_t0(1111118,1,0).
right_of_t0(1111118,0,1).
behind_of_t0(1111118,0,1).
left_of_t0(1111118,1,0).
blocked_t0(1111118,0,front).
blocked_t0(1111118,0,right).
blocked_t0(1111118,1,left).
move_t0(1111118,0,behind).
move_right_of(1111118,0,1).
%Timestep 1
posX_t1(1111118,0,-3.03270007370853).
posY_t1(1111118,0,-0.5131226970991789).
posX_t1(1111118,1,-0.2884683310985565).
posY_t1(1111118,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111119,0,cube).
shape(1111119,1,sphere).
%Timestep 0
posX_t0(1111119,0,-2.3255119479831916).
posY_t0(1111119,0,-1.2203109420475227).
posX_t0(1111119,1,-0.2884683310985565).
posY_t0(1111119,1,1.301581621170044).
front_of_t0(1111119,1,0).
right_of_t0(1111119,0,1).
behind_of_t0(1111119,0,1).
left_of_t0(1111119,1,0).
blocked_t0(1111119,0,front).
blocked_t0(1111119,0,right).
blocked_t0(1111119,1,left).
move_t0(1111119,0,behind).
move_right_of(1111119,0,1).
%Timestep 1
posX_t1(1111119,0,-3.03270007370853).
posY_t1(1111119,0,-0.5131226970991789).
posX_t1(1111119,1,-0.2884683310985565).
posY_t1(1111119,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1111120,0,cube).
shape(1111120,1,sphere).
%Timestep 0
posX_t0(1111120,0,-2.3255119479831916).
posY_t0(1111120,0,-1.2203109420475227).
posX_t0(1111120,1,-0.2884683310985565).
posY_t0(1111120,1,1.301581621170044).
front_of_t0(1111120,1,0).
right_of_t0(1111120,0,1).
behind_of_t0(1111120,0,1).
left_of_t0(1111120,1,0).
blocked_t0(1111120,0,front).
blocked_t0(1111120,0,right).
blocked_t0(1111120,1,left).
move_t0(1111120,0,behind).
move_right_of(1111120,0,1).
%Timestep 1
posX_t1(1111120,0,-3.03270007370853).
posY_t1(1111120,0,-0.5131226970991789).
posX_t1(1111120,1,-0.2884683310985565).
posY_t1(1111120,1,1.301581621170044).
%##################################################################
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20000,0,cube).
shape(20000,1,sphere).
%Timestep 0
posX_t0(20000,0,-3.03270007370853).
posY_t0(20000,0,-0.5131226970991789).
posX_t0(20000,1,-0.2884683310985565).
posY_t0(20000,1,1.301581621170044).
front_of_t0(20000,0,1).
right_of_t0(20000,0,1).
behind_of_t0(20000,1,0).
left_of_t0(20000,1,0).
blocked_t0(20000,0,behind).
blocked_t0(20000,1,left).
move_t0(20000,0,right).
move_right_of(20000,0,1).
%Timestep 1
posX_t1(20000,0,-2.32057086064414).
posY_t1(20000,0,0.19900651596521113).
posX_t1(20000,1,-0.2884683310985565).
posY_t1(20000,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30000,0,cube).
shape(30000,1,sphere).
%Timestep 0
posX_t0(30000,0,-2.32057086064414).
posY_t0(30000,0,0.19900651596521113).
posX_t0(30000,1,-0.2884683310985565).
posY_t0(30000,1,1.301581621170044).
front_of_t0(30000,0,1).
right_of_t0(30000,0,1).
behind_of_t0(30000,1,0).
left_of_t0(30000,1,0).
blocked_t0(30000,0,front).
blocked_t0(30000,0,behind).
blocked_t0(30000,1,left).
move_t0(30000,0,right).
move_right_of(30000,0,1).
%Timestep 1
posX_t1(30000,0,-1.617569884238028).
posY_t1(30000,0,0.9020074923713232).
posX_t1(30000,1,-0.2884683310985565).
posY_t1(30000,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 40000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(40000,0,cube).
shape(40000,1,sphere).
%Timestep 0
posX_t0(40000,0,-1.617569884238028).
posY_t0(40000,0,0.9020074923713232).
posX_t0(40000,1,-0.2884683310985565).
posY_t0(40000,1,1.301581621170044).
front_of_t0(40000,0,1).
right_of_t0(40000,0,1).
behind_of_t0(40000,1,0).
left_of_t0(40000,1,0).
blocked_t0(40000,0,front).
blocked_t0(40000,0,right).
blocked_t0(40000,1,left).
move_t0(40000,0,behind).
move_right_of(40000,0,1).
%Timestep 1
posX_t1(40000,0,-2.328507385940385).
posY_t1(40000,0,1.6129451139287831).
posX_t1(40000,1,-0.2884683310985565).
posY_t1(40000,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 50000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(50000,0,cube).
shape(50000,1,sphere).
%Timestep 0
posX_t0(50000,0,-2.328507385940385).
posY_t0(50000,0,1.6129451139287831).
posX_t0(50000,1,-0.2884683310985565).
posY_t0(50000,1,1.301581621170044).
front_of_t0(50000,0,1).
right_of_t0(50000,0,1).
behind_of_t0(50000,1,0).
left_of_t0(50000,1,0).
blocked_t0(50000,0,behind).
blocked_t0(50000,0,left).
blocked_t0(50000,1,left).
move_t0(50000,0,right).
move_right_of(50000,0,1).
%Timestep 1
posX_t1(50000,0,-1.627861148768647).
posY_t1(50000,0,2.3135913511005213).
posX_t1(50000,1,-0.2884683310985565).
posY_t1(50000,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 60000
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(60000,0,cube).
shape(60000,1,sphere).
%Timestep 0
posX_t0(60000,0,-1.627861148768647).
posY_t0(60000,0,2.3135913511005213).
posX_t0(60000,1,-0.2884683310985565).
posY_t0(60000,1,1.301581621170044).
front_of_t0(60000,0,1).
right_of_t0(60000,0,1).
behind_of_t0(60000,1,0).
left_of_t0(60000,1,0).
blocked_t0(60000,0,front).
blocked_t0(60000,1,behind).
blocked_t0(60000,1,left).
move_t0(60000,0,right).
move_right_of(60000,0,1).
%Timestep 1
posX_t1(60000,0,-0.9165983673353094).
posY_t1(60000,0,3.024854132533859).
posX_t1(60000,1,-0.2884683310985565).
posY_t1(60000,1,1.301581621170044).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 1
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(1,0,cube).
shape(1,1,sphere).
%Timestep 0
posX_t0(1,0,1.4003678560256958).
posY_t0(1,0,3.3426334857940674).
posX_t0(1,1,-2.281648635864258).
posY_t0(1,1,-0.9816907644271851).
front_of_t0(1,0,1).
right_of_t0(1,1,0).
behind_of_t0(1,1,0).
left_of_t0(1,0,1).
blocked_t0(1,0,front).
blocked_t0(1,0,left).
move_right_of(1,0,1).
%Timestep 1
posX_t1(1,0,1.4003678560256958).
posY_t1(1,0,3.3426334857940674).
posX_t1(1,1,-2.281648635864258).
posY_t1(1,1,-0.9816907644271851).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 2
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2,0,cube).
shape(2,1,sphere).
%Timestep 0
posX_t0(2,0,0.4277612268924713).
posY_t0(2,0,1.8636765480041504).
posX_t0(2,1,3.0620532035827637).
posY_t0(2,1,-0.18584729731082916).
front_of_t0(2,0,1).
right_of_t0(2,0,1).
behind_of_t0(2,1,0).
left_of_t0(2,1,0).
blocked_t0(2,0,behind).
blocked_t0(2,1,behind).
move_t0(2,0,right).
move_right_of(2,0,1).
%Timestep 1
posX_t1(2,0,1.1337303976717048).
posY_t1(2,0,2.569645718783384).
posX_t1(2,1,3.0620532035827637).
posY_t1(2,1,-0.18584729731082916).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 3
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(3,0,cube).
shape(3,1,sphere).
%Timestep 0
posX_t0(3,0,-0.7748625874519348).
posY_t0(3,0,2.1817541122436523).
posX_t0(3,1,-0.08347566425800323).
posY_t0(3,1,1.2442266941070557).
front_of_t0(3,0,1).
behind_of_t0(3,1,0).
blocked_t0(3,0,front).
blocked_t0(3,1,front).
blocked_t0(3,0,behind).
blocked_t0(3,1,behind).
move_t0(3,0,right).
move_right_of(3,0,1).
%Timestep 1
posX_t1(3,0,-0.07325981984835428).
posY_t1(3,0,2.883356879847233).
posX_t1(3,1,-0.08347566425800323).
posY_t1(3,1,1.2442266941070557).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 4
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(4,0,cube).
shape(4,1,sphere).
%Timestep 0
posX_t0(4,0,-1.007344365119934).
posY_t0(4,0,2.8891236782073975).
posX_t0(4,1,0.9641307592391968).
posY_t0(4,1,2.8239293098449707).
front_of_t0(4,0,1).
right_of_t0(4,0,1).
behind_of_t0(4,1,0).
left_of_t0(4,1,0).
blocked_t0(4,0,front).
blocked_t0(4,1,left).
move_t0(4,0,right).
move_right_of(4,0,1).
%Timestep 1
posX_t1(4,0,-0.2970083969591093).
posY_t1(4,0,3.5994596463682225).
posX_t1(4,1,0.9641307592391968).
posY_t1(4,1,2.8239293098449707).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10004
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10004,0,cube).
shape(10004,1,sphere).
%Timestep 0
posX_t0(10004,0,-0.2970083969591093).
posY_t0(10004,0,3.5994596463682225).
posX_t0(10004,1,0.9641307592391968).
posY_t0(10004,1,2.8239293098449707).
front_of_t0(10004,0,1).
right_of_t0(10004,0,1).
behind_of_t0(10004,1,0).
left_of_t0(10004,1,0).
blocked_t0(10004,0,front).
blocked_t0(10004,1,behind).
blocked_t0(10004,1,left).
move_t0(10004,0,right).
move_right_of(10004,0,1).
%Timestep 1
posX_t1(10004,0,0.41676399535769093).
posY_t1(10004,0,4.3132320386850225).
posX_t1(10004,1,0.9641307592391968).
posY_t1(10004,1,2.8239293098449707).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 5
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(5,0,cube).
shape(5,1,sphere).
%Timestep 0
posX_t0(5,0,3.2426795959472656).
posY_t0(5,0,1.489911437034607).
posX_t0(5,1,-2.6966145038604736).
posY_t0(5,1,-0.3070101737976074).
front_of_t0(5,1,0).
right_of_t0(5,1,0).
behind_of_t0(5,0,1).
left_of_t0(5,0,1).
blocked_t0(5,1,front).
blocked_t0(5,0,behind).
blocked_t0(5,1,left).
move_right_of(5,0,1).
%Timestep 1
posX_t1(5,0,3.2426795959472656).
posY_t1(5,0,1.489911437034607).
posX_t1(5,1,-2.6966145038604736).
posY_t1(5,1,-0.3070101737976074).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 6
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(6,0,cube).
shape(6,1,sphere).
%Timestep 0
posX_t0(6,0,-2.9209089279174805).
posY_t0(6,0,-1.7604408264160156).
posX_t0(6,1,1.2880009412765503).
posY_t0(6,1,-2.191864252090454).
front_of_t0(6,0,1).
right_of_t0(6,0,1).
behind_of_t0(6,1,0).
left_of_t0(6,1,0).
blocked_t0(6,0,front).
blocked_t0(6,1,front).
blocked_t0(6,0,right).
blocked_t0(6,1,behind).
blocked_t0(6,0,left).
move_t0(6,0,behind).
move_right_of(6,0,1).
%Timestep 1
posX_t1(6,0,-3.628537387671212).
posY_t1(6,0,-1.052812247365044).
posX_t1(6,1,1.2880009412765503).
posY_t1(6,1,-2.191864252090454).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10006
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10006,0,cube).
shape(10006,1,sphere).
%Timestep 0
posX_t0(10006,0,-3.628537387671212).
posY_t0(10006,0,-1.052812247365044).
posX_t0(10006,1,1.2880009412765503).
posY_t0(10006,1,-2.191864252090454).
front_of_t0(10006,0,1).
right_of_t0(10006,0,1).
behind_of_t0(10006,1,0).
left_of_t0(10006,1,0).
blocked_t0(10006,1,front).
blocked_t0(10006,1,behind).
move_t0(10006,0,right).
move_right_of(10006,0,1).
%Timestep 1
posX_t1(10006,0,-2.9235800652564707).
posY_t1(10006,0,-0.3478549249503028).
posX_t1(10006,1,1.2880009412765503).
posY_t1(10006,1,-2.191864252090454).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20006
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20006,0,cube).
shape(20006,1,sphere).
%Timestep 0
posX_t0(20006,0,-2.9235800652564707).
posY_t0(20006,0,-0.3478549249503028).
posX_t0(20006,1,1.2880009412765503).
posY_t0(20006,1,-2.191864252090454).
front_of_t0(20006,0,1).
right_of_t0(20006,0,1).
behind_of_t0(20006,1,0).
left_of_t0(20006,1,0).
blocked_t0(20006,0,front).
blocked_t0(20006,1,front).
blocked_t0(20006,1,behind).
move_t0(20006,0,right).
move_right_of(20006,0,1).
%Timestep 1
posX_t1(20006,0,-2.2107343269605515).
posY_t1(20006,0,0.36499081334561645).
posX_t1(20006,1,1.2880009412765503).
posY_t1(20006,1,-2.191864252090454).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30006
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30006,0,cube).
shape(30006,1,sphere).
%Timestep 0
posX_t0(30006,0,-2.2107343269605515).
posY_t0(30006,0,0.36499081334561645).
posX_t0(30006,1,1.2880009412765503).
posY_t0(30006,1,-2.191864252090454).
front_of_t0(30006,0,1).
right_of_t0(30006,0,1).
behind_of_t0(30006,1,0).
left_of_t0(30006,1,0).
blocked_t0(30006,0,front).
blocked_t0(30006,1,front).
blocked_t0(30006,0,right).
blocked_t0(30006,0,behind).
blocked_t0(30006,1,behind).
move_t0(30006,0,left).
move_right_of(30006,0,1).
%Timestep 1
posX_t1(30006,0,-2.915287627326468).
posY_t1(30006,0,-0.3395624870203).
posX_t1(30006,1,1.2880009412765503).
posY_t1(30006,1,-2.191864252090454).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 40006
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(40006,0,cube).
shape(40006,1,sphere).
%Timestep 0
posX_t0(40006,0,-2.915287627326468).
posY_t0(40006,0,-0.3395624870203).
posX_t0(40006,1,1.2880009412765503).
posY_t0(40006,1,-2.191864252090454).
front_of_t0(40006,0,1).
right_of_t0(40006,0,1).
behind_of_t0(40006,1,0).
left_of_t0(40006,1,0).
blocked_t0(40006,0,front).
blocked_t0(40006,1,front).
blocked_t0(40006,1,behind).
move_t0(40006,0,behind).
move_right_of(40006,0,1).
%Timestep 1
posX_t1(40006,0,-3.6294653436023028).
posY_t1(40006,0,0.3746153496568969).
posX_t1(40006,1,1.2880009412765503).
posY_t1(40006,1,-2.191864252090454).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 50006
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(50006,0,cube).
shape(50006,1,sphere).
%Timestep 0
posX_t0(50006,0,-3.6294653436023028).
posY_t0(50006,0,0.3746153496568969).
posX_t0(50006,1,1.2880009412765503).
posY_t0(50006,1,-2.191864252090454).
front_of_t0(50006,0,1).
right_of_t0(50006,0,1).
behind_of_t0(50006,1,0).
left_of_t0(50006,1,0).
blocked_t0(50006,1,front).
blocked_t0(50006,0,right).
blocked_t0(50006,1,behind).
move_t0(50006,0,behind).
move_right_of(50006,0,1).
%Timestep 1
posX_t1(50006,0,-4.336823320659626).
posY_t1(50006,0,1.0819734459658599).
posX_t1(50006,1,1.2880009412765503).
posY_t1(50006,1,-2.191864252090454).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 60006
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(60006,0,cube).
shape(60006,1,sphere).
%Timestep 0
posX_t0(60006,0,-4.336823320659626).
posY_t0(60006,0,1.0819734459658599).
posX_t0(60006,1,1.2880009412765503).
posY_t0(60006,1,-2.191864252090454).
front_of_t0(60006,0,1).
right_of_t0(60006,0,1).
behind_of_t0(60006,1,0).
left_of_t0(60006,1,0).
blocked_t0(60006,1,front).
blocked_t0(60006,1,behind).
move_t0(60006,0,right).
move_right_of(60006,0,1).
%Timestep 1
posX_t1(60006,0,-3.6326076173672397).
posY_t1(60006,0,1.786189149258246).
posX_t1(60006,1,1.2880009412765503).
posY_t1(60006,1,-2.191864252090454).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 70006
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(70006,0,cube).
shape(70006,1,sphere).
%Timestep 0
posX_t0(70006,0,-3.6326076173672397).
posY_t0(70006,0,1.786189149258246).
posX_t0(70006,1,1.2880009412765503).
posY_t0(70006,1,-2.191864252090454).
front_of_t0(70006,0,1).
right_of_t0(70006,0,1).
behind_of_t0(70006,1,0).
left_of_t0(70006,1,0).
blocked_t0(70006,0,front).
blocked_t0(70006,1,front).
blocked_t0(70006,0,right).
blocked_t0(70006,1,behind).
move_t0(70006,0,behind).
move_right_of(70006,0,1).
%Timestep 1
posX_t1(70006,0,-4.332674395606767).
posY_t1(70006,0,2.486256045520209).
posX_t1(70006,1,1.2880009412765503).
posY_t1(70006,1,-2.191864252090454).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 80006
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(80006,0,cube).
shape(80006,1,sphere).
%Timestep 0
posX_t0(80006,0,-4.332674395606767).
posY_t0(80006,0,2.486256045520209).
posX_t0(80006,1,1.2880009412765503).
posY_t0(80006,1,-2.191864252090454).
front_of_t0(80006,0,1).
right_of_t0(80006,0,1).
behind_of_t0(80006,1,0).
left_of_t0(80006,1,0).
blocked_t0(80006,1,front).
blocked_t0(80006,1,behind).
move_t0(80006,0,right).
move_right_of(80006,0,1).
%Timestep 1
posX_t1(80006,0,-3.6232928936654485).
posY_t1(80006,0,3.1956375474615273).
posX_t1(80006,1,1.2880009412765503).
posY_t1(80006,1,-2.191864252090454).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 7
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(7,0,cube).
shape(7,1,sphere).
%Timestep 0
posX_t0(7,0,0.4127589464187622).
posY_t0(7,0,-0.9291863441467285).
posX_t0(7,1,-0.9765662550926208).
posY_t0(7,1,-2.4988644123077393).
right_of_t0(7,1,0).
left_of_t0(7,0,1).
move_right_of(7,0,1).
%Timestep 1
posX_t1(7,0,0.4127589464187622).
posY_t1(7,0,-0.9291863441467285).
posX_t1(7,1,-0.9765662550926208).
posY_t1(7,1,-2.4988644123077393).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 8
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(8,0,cube).
shape(8,1,sphere).
%Timestep 0
posX_t0(8,0,1.732832670211792).
posY_t0(8,0,1.8765389919281006).
posX_t0(8,1,0.33415958285331726).
posY_t0(8,1,1.0181440114974976).
front_of_t0(8,1,0).
right_of_t0(8,1,0).
behind_of_t0(8,0,1).
left_of_t0(8,0,1).
blocked_t0(8,1,front).
blocked_t0(8,1,right).
blocked_t0(8,0,behind).
blocked_t0(8,1,behind).
blocked_t0(8,0,left).
blocked_t0(8,1,left).
move_right_of(8,0,1).
%Timestep 1
posX_t1(8,0,1.732832670211792).
posY_t1(8,0,1.8765389919281006).
posX_t1(8,1,0.33415958285331726).
posY_t1(8,1,1.0181440114974976).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 9
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(9,0,cube).
shape(9,1,sphere).
%Timestep 0
posX_t0(9,0,-0.5077299475669861).
posY_t0(9,0,1.4806437492370605).
posX_t0(9,1,-1.7265626192092896).
posY_t0(9,1,1.2925671339035034).
front_of_t0(9,1,0).
right_of_t0(9,1,0).
behind_of_t0(9,0,1).
left_of_t0(9,0,1).
blocked_t0(9,1,right).
blocked_t0(9,0,behind).
blocked_t0(9,0,left).
move_right_of(9,0,1).
%Timestep 1
posX_t1(9,0,-0.5077299475669861).
posY_t1(9,0,1.4806437492370605).
posX_t1(9,1,-1.7265626192092896).
posY_t1(9,1,1.2925671339035034).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10,0,cube).
shape(10,1,sphere).
%Timestep 0
posX_t0(10,0,2.8008131980895996).
posY_t0(10,0,0.5908575057983398).
posX_t0(10,1,-0.8343574404716492).
posY_t0(10,1,-2.210059881210327).
front_of_t0(10,1,0).
right_of_t0(10,1,0).
behind_of_t0(10,0,1).
left_of_t0(10,0,1).
blocked_t0(10,1,behind).
move_right_of(10,0,1).
%Timestep 1
posX_t1(10,0,2.8008131980895996).
posY_t1(10,0,0.5908575057983398).
posX_t1(10,1,-0.8343574404716492).
posY_t1(10,1,-2.210059881210327).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 11
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(11,0,cube).
shape(11,1,sphere).
%Timestep 0
posX_t0(11,0,1.4476618766784668).
posY_t0(11,0,-2.3330681324005127).
posX_t0(11,1,0.7820436954498291).
posY_t0(11,1,2.094316244125366).
front_of_t0(11,1,0).
right_of_t0(11,0,1).
behind_of_t0(11,0,1).
left_of_t0(11,1,0).
blocked_t0(11,1,right).
blocked_t0(11,1,left).
move_t0(11,0,right).
move_right_of(11,0,1).
%Timestep 1
posX_t1(11,0,2.1598668627406243).
posY_t1(11,0,-1.6208631463383552).
posX_t1(11,1,0.7820436954498291).
posY_t1(11,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10011,0,cube).
shape(10011,1,sphere).
%Timestep 0
posX_t0(10011,0,2.1598668627406243).
posY_t0(10011,0,-1.6208631463383552).
posX_t0(10011,1,0.7820436954498291).
posY_t0(10011,1,2.094316244125366).
front_of_t0(10011,1,0).
right_of_t0(10011,0,1).
behind_of_t0(10011,0,1).
left_of_t0(10011,1,0).
blocked_t0(10011,1,right).
blocked_t0(10011,1,left).
move_t0(10011,0,right).
move_right_of(10011,0,1).
%Timestep 1
posX_t1(10011,0,2.861262381396028).
posY_t1(10011,0,-0.9194676276829514).
posX_t1(10011,1,0.7820436954498291).
posY_t1(10011,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20011,0,cube).
shape(20011,1,sphere).
%Timestep 0
posX_t0(20011,0,2.861262381396028).
posY_t0(20011,0,-0.9194676276829514).
posX_t0(20011,1,0.7820436954498291).
posY_t0(20011,1,2.094316244125366).
front_of_t0(20011,1,0).
right_of_t0(20011,0,1).
behind_of_t0(20011,0,1).
left_of_t0(20011,1,0).
blocked_t0(20011,0,right).
blocked_t0(20011,1,right).
blocked_t0(20011,1,left).
move_t0(20011,0,front).
move_right_of(20011,0,1).
%Timestep 1
posX_t1(20011,0,3.563672316094048).
posY_t1(20011,0,-1.6218776807984339).
posX_t1(20011,1,0.7820436954498291).
posY_t1(20011,1,2.094316244125366).
%###################################################################
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222201,0,cube).
shape(2222201,1,sphere).
%Timestep 0
posX_t0(2222201,0,2.861262381396028).
posY_t0(2222201,0,-0.9194676276829514).
posX_t0(2222201,1,0.7820436954498291).
posY_t0(2222201,1,2.094316244125366).
front_of_t0(2222201,1,0).
right_of_t0(2222201,0,1).
behind_of_t0(2222201,0,1).
left_of_t0(2222201,1,0).
blocked_t0(2222201,0,right).
blocked_t0(2222201,1,right).
blocked_t0(2222201,1,left).
move_t0(2222201,0,front).
move_right_of(2222201,0,1).
%Timestep 1
posX_t1(2222201,0,3.563672316094048).
posY_t1(2222201,0,-1.6218776807984339).
posX_t1(2222201,1,0.7820436954498291).
posY_t1(2222201,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222202,0,cube).
shape(2222202,1,sphere).
%Timestep 0
posX_t0(2222202,0,2.861262381396028).
posY_t0(2222202,0,-0.9194676276829514).
posX_t0(2222202,1,0.7820436954498291).
posY_t0(2222202,1,2.094316244125366).
front_of_t0(2222202,1,0).
right_of_t0(2222202,0,1).
behind_of_t0(2222202,0,1).
left_of_t0(2222202,1,0).
blocked_t0(2222202,0,right).
blocked_t0(2222202,1,right).
blocked_t0(2222202,1,left).
move_t0(2222202,0,front).
move_right_of(2222202,0,1).
%Timestep 1
posX_t1(2222202,0,3.563672316094048).
posY_t1(2222202,0,-1.6218776807984339).
posX_t1(2222202,1,0.7820436954498291).
posY_t1(2222202,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222203,0,cube).
shape(2222203,1,sphere).
%Timestep 0
posX_t0(2222203,0,2.861262381396028).
posY_t0(2222203,0,-0.9194676276829514).
posX_t0(2222203,1,0.7820436954498291).
posY_t0(2222203,1,2.094316244125366).
front_of_t0(2222203,1,0).
right_of_t0(2222203,0,1).
behind_of_t0(2222203,0,1).
left_of_t0(2222203,1,0).
blocked_t0(2222203,0,right).
blocked_t0(2222203,1,right).
blocked_t0(2222203,1,left).
move_t0(2222203,0,front).
move_right_of(2222203,0,1).
%Timestep 1
posX_t1(2222203,0,3.563672316094048).
posY_t1(2222203,0,-1.6218776807984339).
posX_t1(2222203,1,0.7820436954498291).
posY_t1(2222203,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222204,0,cube).
shape(2222204,1,sphere).
%Timestep 0
posX_t0(2222204,0,2.861262381396028).
posY_t0(2222204,0,-0.9194676276829514).
posX_t0(2222204,1,0.7820436954498291).
posY_t0(2222204,1,2.094316244125366).
front_of_t0(2222204,1,0).
right_of_t0(2222204,0,1).
behind_of_t0(2222204,0,1).
left_of_t0(2222204,1,0).
blocked_t0(2222204,0,right).
blocked_t0(2222204,1,right).
blocked_t0(2222204,1,left).
move_t0(2222204,0,front).
move_right_of(2222204,0,1).
%Timestep 1
posX_t1(2222204,0,3.563672316094048).
posY_t1(2222204,0,-1.6218776807984339).
posX_t1(2222204,1,0.7820436954498291).
posY_t1(2222204,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222205,0,cube).
shape(2222205,1,sphere).
%Timestep 0
posX_t0(2222205,0,2.861262381396028).
posY_t0(2222205,0,-0.9194676276829514).
posX_t0(2222205,1,0.7820436954498291).
posY_t0(2222205,1,2.094316244125366).
front_of_t0(2222205,1,0).
right_of_t0(2222205,0,1).
behind_of_t0(2222205,0,1).
left_of_t0(2222205,1,0).
blocked_t0(2222205,0,right).
blocked_t0(2222205,1,right).
blocked_t0(2222205,1,left).
move_t0(2222205,0,front).
move_right_of(2222205,0,1).
%Timestep 1
posX_t1(2222205,0,3.563672316094048).
posY_t1(2222205,0,-1.6218776807984339).
posX_t1(2222205,1,0.7820436954498291).
posY_t1(2222205,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222206,0,cube).
shape(2222206,1,sphere).
%Timestep 0
posX_t0(2222206,0,2.861262381396028).
posY_t0(2222206,0,-0.9194676276829514).
posX_t0(2222206,1,0.7820436954498291).
posY_t0(2222206,1,2.094316244125366).
front_of_t0(2222206,1,0).
right_of_t0(2222206,0,1).
behind_of_t0(2222206,0,1).
left_of_t0(2222206,1,0).
blocked_t0(2222206,0,right).
blocked_t0(2222206,1,right).
blocked_t0(2222206,1,left).
move_t0(2222206,0,front).
move_right_of(2222206,0,1).
%Timestep 1
posX_t1(2222206,0,3.563672316094048).
posY_t1(2222206,0,-1.6218776807984339).
posX_t1(2222206,1,0.7820436954498291).
posY_t1(2222206,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222207,0,cube).
shape(2222207,1,sphere).
%Timestep 0
posX_t0(2222207,0,2.861262381396028).
posY_t0(2222207,0,-0.9194676276829514).
posX_t0(2222207,1,0.7820436954498291).
posY_t0(2222207,1,2.094316244125366).
front_of_t0(2222207,1,0).
right_of_t0(2222207,0,1).
behind_of_t0(2222207,0,1).
left_of_t0(2222207,1,0).
blocked_t0(2222207,0,right).
blocked_t0(2222207,1,right).
blocked_t0(2222207,1,left).
move_t0(2222207,0,front).
move_right_of(2222207,0,1).
%Timestep 1
posX_t1(2222207,0,3.563672316094048).
posY_t1(2222207,0,-1.6218776807984339).
posX_t1(2222207,1,0.7820436954498291).
posY_t1(2222207,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222208,0,cube).
shape(2222208,1,sphere).
%Timestep 0
posX_t0(2222208,0,2.861262381396028).
posY_t0(2222208,0,-0.9194676276829514).
posX_t0(2222208,1,0.7820436954498291).
posY_t0(2222208,1,2.094316244125366).
front_of_t0(2222208,1,0).
right_of_t0(2222208,0,1).
behind_of_t0(2222208,0,1).
left_of_t0(2222208,1,0).
blocked_t0(2222208,0,right).
blocked_t0(2222208,1,right).
blocked_t0(2222208,1,left).
move_t0(2222208,0,front).
move_right_of(2222208,0,1).
%Timestep 1
posX_t1(2222208,0,3.563672316094048).
posY_t1(2222208,0,-1.6218776807984339).
posX_t1(2222208,1,0.7820436954498291).
posY_t1(2222208,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222209,0,cube).
shape(2222209,1,sphere).
%Timestep 0
posX_t0(2222209,0,2.861262381396028).
posY_t0(2222209,0,-0.9194676276829514).
posX_t0(2222209,1,0.7820436954498291).
posY_t0(2222209,1,2.094316244125366).
front_of_t0(2222209,1,0).
right_of_t0(2222209,0,1).
behind_of_t0(2222209,0,1).
left_of_t0(2222209,1,0).
blocked_t0(2222209,0,right).
blocked_t0(2222209,1,right).
blocked_t0(2222209,1,left).
move_t0(2222209,0,front).
move_right_of(2222209,0,1).
%Timestep 1
posX_t1(2222209,0,3.563672316094048).
posY_t1(2222209,0,-1.6218776807984339).
posX_t1(2222209,1,0.7820436954498291).
posY_t1(2222209,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222210,0,cube).
shape(2222210,1,sphere).
%Timestep 0
posX_t0(2222210,0,2.861262381396028).
posY_t0(2222210,0,-0.9194676276829514).
posX_t0(2222210,1,0.7820436954498291).
posY_t0(2222210,1,2.094316244125366).
front_of_t0(2222210,1,0).
right_of_t0(2222210,0,1).
behind_of_t0(2222210,0,1).
left_of_t0(2222210,1,0).
blocked_t0(2222210,0,right).
blocked_t0(2222210,1,right).
blocked_t0(2222210,1,left).
move_t0(2222210,0,front).
move_right_of(2222210,0,1).
%Timestep 1
posX_t1(2222210,0,3.563672316094048).
posY_t1(2222210,0,-1.6218776807984339).
posX_t1(2222210,1,0.7820436954498291).
posY_t1(2222210,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222211,0,cube).
shape(2222211,1,sphere).
%Timestep 0
posX_t0(2222211,0,2.861262381396028).
posY_t0(2222211,0,-0.9194676276829514).
posX_t0(2222211,1,0.7820436954498291).
posY_t0(2222211,1,2.094316244125366).
front_of_t0(2222211,1,0).
right_of_t0(2222211,0,1).
behind_of_t0(2222211,0,1).
left_of_t0(2222211,1,0).
blocked_t0(2222211,0,right).
blocked_t0(2222211,1,right).
blocked_t0(2222211,1,left).
move_t0(2222211,0,front).
move_right_of(2222211,0,1).
%Timestep 1
posX_t1(2222211,0,3.563672316094048).
posY_t1(2222211,0,-1.6218776807984339).
posX_t1(2222211,1,0.7820436954498291).
posY_t1(2222211,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222212,0,cube).
shape(2222212,1,sphere).
%Timestep 0
posX_t0(2222212,0,2.861262381396028).
posY_t0(2222212,0,-0.9194676276829514).
posX_t0(2222212,1,0.7820436954498291).
posY_t0(2222212,1,2.094316244125366).
front_of_t0(2222212,1,0).
right_of_t0(2222212,0,1).
behind_of_t0(2222212,0,1).
left_of_t0(2222212,1,0).
blocked_t0(2222212,0,right).
blocked_t0(2222212,1,right).
blocked_t0(2222212,1,left).
move_t0(2222212,0,front).
move_right_of(2222212,0,1).
%Timestep 1
posX_t1(2222212,0,3.563672316094048).
posY_t1(2222212,0,-1.6218776807984339).
posX_t1(2222212,1,0.7820436954498291).
posY_t1(2222212,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222213,0,cube).
shape(2222213,1,sphere).
%Timestep 0
posX_t0(2222213,0,2.861262381396028).
posY_t0(2222213,0,-0.9194676276829514).
posX_t0(2222213,1,0.7820436954498291).
posY_t0(2222213,1,2.094316244125366).
front_of_t0(2222213,1,0).
right_of_t0(2222213,0,1).
behind_of_t0(2222213,0,1).
left_of_t0(2222213,1,0).
blocked_t0(2222213,0,right).
blocked_t0(2222213,1,right).
blocked_t0(2222213,1,left).
move_t0(2222213,0,front).
move_right_of(2222213,0,1).
%Timestep 1
posX_t1(2222213,0,3.563672316094048).
posY_t1(2222213,0,-1.6218776807984339).
posX_t1(2222213,1,0.7820436954498291).
posY_t1(2222213,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222214,0,cube).
shape(2222214,1,sphere).
%Timestep 0
posX_t0(2222214,0,2.861262381396028).
posY_t0(2222214,0,-0.9194676276829514).
posX_t0(2222214,1,0.7820436954498291).
posY_t0(2222214,1,2.094316244125366).
front_of_t0(2222214,1,0).
right_of_t0(2222214,0,1).
behind_of_t0(2222214,0,1).
left_of_t0(2222214,1,0).
blocked_t0(2222214,0,right).
blocked_t0(2222214,1,right).
blocked_t0(2222214,1,left).
move_t0(2222214,0,front).
move_right_of(2222214,0,1).
%Timestep 1
posX_t1(2222214,0,3.563672316094048).
posY_t1(2222214,0,-1.6218776807984339).
posX_t1(2222214,1,0.7820436954498291).
posY_t1(2222214,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222215,0,cube).
shape(2222215,1,sphere).
%Timestep 0
posX_t0(2222215,0,2.861262381396028).
posY_t0(2222215,0,-0.9194676276829514).
posX_t0(2222215,1,0.7820436954498291).
posY_t0(2222215,1,2.094316244125366).
front_of_t0(2222215,1,0).
right_of_t0(2222215,0,1).
behind_of_t0(2222215,0,1).
left_of_t0(2222215,1,0).
blocked_t0(2222215,0,right).
blocked_t0(2222215,1,right).
blocked_t0(2222215,1,left).
move_t0(2222215,0,front).
move_right_of(2222215,0,1).
%Timestep 1
posX_t1(2222215,0,3.563672316094048).
posY_t1(2222215,0,-1.6218776807984339).
posX_t1(2222215,1,0.7820436954498291).
posY_t1(2222215,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222216,0,cube).
shape(2222216,1,sphere).
%Timestep 0
posX_t0(2222216,0,2.861262381396028).
posY_t0(2222216,0,-0.9194676276829514).
posX_t0(2222216,1,0.7820436954498291).
posY_t0(2222216,1,2.094316244125366).
front_of_t0(2222216,1,0).
right_of_t0(2222216,0,1).
behind_of_t0(2222216,0,1).
left_of_t0(2222216,1,0).
blocked_t0(2222216,0,right).
blocked_t0(2222216,1,right).
blocked_t0(2222216,1,left).
move_t0(2222216,0,front).
move_right_of(2222216,0,1).
%Timestep 1
posX_t1(2222216,0,3.563672316094048).
posY_t1(2222216,0,-1.6218776807984339).
posX_t1(2222216,1,0.7820436954498291).
posY_t1(2222216,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222217,0,cube).
shape(2222217,1,sphere).
%Timestep 0
posX_t0(2222217,0,2.861262381396028).
posY_t0(2222217,0,-0.9194676276829514).
posX_t0(2222217,1,0.7820436954498291).
posY_t0(2222217,1,2.094316244125366).
front_of_t0(2222217,1,0).
right_of_t0(2222217,0,1).
behind_of_t0(2222217,0,1).
left_of_t0(2222217,1,0).
blocked_t0(2222217,0,right).
blocked_t0(2222217,1,right).
blocked_t0(2222217,1,left).
move_t0(2222217,0,front).
move_right_of(2222217,0,1).
%Timestep 1
posX_t1(2222217,0,3.563672316094048).
posY_t1(2222217,0,-1.6218776807984339).
posX_t1(2222217,1,0.7820436954498291).
posY_t1(2222217,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222218,0,cube).
shape(2222218,1,sphere).
%Timestep 0
posX_t0(2222218,0,2.861262381396028).
posY_t0(2222218,0,-0.9194676276829514).
posX_t0(2222218,1,0.7820436954498291).
posY_t0(2222218,1,2.094316244125366).
front_of_t0(2222218,1,0).
right_of_t0(2222218,0,1).
behind_of_t0(2222218,0,1).
left_of_t0(2222218,1,0).
blocked_t0(2222218,0,right).
blocked_t0(2222218,1,right).
blocked_t0(2222218,1,left).
move_t0(2222218,0,front).
move_right_of(2222218,0,1).
%Timestep 1
posX_t1(2222218,0,3.563672316094048).
posY_t1(2222218,0,-1.6218776807984339).
posX_t1(2222218,1,0.7820436954498291).
posY_t1(2222218,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222219,0,cube).
shape(2222219,1,sphere).
%Timestep 0
posX_t0(2222219,0,2.861262381396028).
posY_t0(2222219,0,-0.9194676276829514).
posX_t0(2222219,1,0.7820436954498291).
posY_t0(2222219,1,2.094316244125366).
front_of_t0(2222219,1,0).
right_of_t0(2222219,0,1).
behind_of_t0(2222219,0,1).
left_of_t0(2222219,1,0).
blocked_t0(2222219,0,right).
blocked_t0(2222219,1,right).
blocked_t0(2222219,1,left).
move_t0(2222219,0,front).
move_right_of(2222219,0,1).
%Timestep 1
posX_t1(2222219,0,3.563672316094048).
posY_t1(2222219,0,-1.6218776807984339).
posX_t1(2222219,1,0.7820436954498291).
posY_t1(2222219,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(2222220,0,cube).
shape(2222220,1,sphere).
%Timestep 0
posX_t0(2222220,0,2.861262381396028).
posY_t0(2222220,0,-0.9194676276829514).
posX_t0(2222220,1,0.7820436954498291).
posY_t0(2222220,1,2.094316244125366).
front_of_t0(2222220,1,0).
right_of_t0(2222220,0,1).
behind_of_t0(2222220,0,1).
left_of_t0(2222220,1,0).
blocked_t0(2222220,0,right).
blocked_t0(2222220,1,right).
blocked_t0(2222220,1,left).
move_t0(2222220,0,front).
move_right_of(2222220,0,1).
%Timestep 1
posX_t1(2222220,0,3.563672316094048).
posY_t1(2222220,0,-1.6218776807984339).
posX_t1(2222220,1,0.7820436954498291).
posY_t1(2222220,1,2.094316244125366).
%###################################################################
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30011
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30011,0,cube).
shape(30011,1,sphere).
%Timestep 0
posX_t0(30011,0,3.563672316094048).
posY_t0(30011,0,-1.6218776807984339).
posX_t0(30011,1,0.7820436954498291).
posY_t0(30011,1,2.094316244125366).
front_of_t0(30011,1,0).
right_of_t0(30011,0,1).
behind_of_t0(30011,0,1).
left_of_t0(30011,1,0).
blocked_t0(30011,1,right).
blocked_t0(30011,1,left).
move_t0(30011,0,right).
move_right_of(30011,0,1).
%Timestep 1
posX_t1(30011,0,4.265928081130383).
posY_t1(30011,0,-0.9196219157620988).
posX_t1(30011,1,0.7820436954498291).
posY_t1(30011,1,2.094316244125366).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 12
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(12,0,cube).
shape(12,1,sphere).
%Timestep 0
posX_t0(12,0,-3.0465590953826904).
posY_t0(12,0,-1.8769738674163818).
posX_t0(12,1,0.7641057372093201).
posY_t0(12,1,0.5826629996299744).
front_of_t0(12,0,1).
right_of_t0(12,0,1).
behind_of_t0(12,1,0).
left_of_t0(12,1,0).
blocked_t0(12,0,front).
blocked_t0(12,0,right).
blocked_t0(12,1,behind).
move_t0(12,0,behind).
move_right_of(12,0,1).
%Timestep 1
posX_t1(12,0,-3.7557356933369292).
posY_t1(12,0,-1.1677971499039064).
posX_t1(12,1,0.7641057372093201).
posY_t1(12,1,0.5826629996299744).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10012
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10012,0,cube).
shape(10012,1,sphere).
%Timestep 0
posX_t0(10012,0,-3.7557356933369292).
posY_t0(10012,0,-1.1677971499039064).
posX_t0(10012,1,0.7641057372093201).
posY_t0(10012,1,0.5826629996299744).
front_of_t0(10012,0,1).
right_of_t0(10012,0,1).
behind_of_t0(10012,1,0).
left_of_t0(10012,1,0).
blocked_t0(10012,0,right).
blocked_t0(10012,1,behind).
move_t0(10012,0,behind).
move_right_of(10012,0,1).
%Timestep 1
posX_t1(10012,0,-4.458034342588928).
posY_t1(10012,0,-0.4654983822532064).
posX_t1(10012,1,0.7641057372093201).
posY_t1(10012,1,0.5826629996299744).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20012
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20012,0,cube).
shape(20012,1,sphere).
%Timestep 0
posX_t0(20012,0,-4.458034342588928).
posY_t0(20012,0,-0.4654983822532064).
posX_t0(20012,1,0.7641057372093201).
posY_t0(20012,1,0.5826629996299744).
front_of_t0(20012,0,1).
right_of_t0(20012,0,1).
behind_of_t0(20012,1,0).
left_of_t0(20012,1,0).
blocked_t0(20012,1,behind).
move_t0(20012,0,right).
move_right_of(20012,0,1).
%Timestep 1
posX_t1(20012,0,-3.757461177632258).
posY_t1(20012,0,0.23507478270346394).
posX_t1(20012,1,0.7641057372093201).
posY_t1(20012,1,0.5826629996299744).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30012
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30012,0,cube).
shape(30012,1,sphere).
%Timestep 0
posX_t0(30012,0,-3.757461177632258).
posY_t0(30012,0,0.23507478270346394).
posX_t0(30012,1,0.7641057372093201).
posY_t0(30012,1,0.5826629996299744).
front_of_t0(30012,0,1).
right_of_t0(30012,0,1).
behind_of_t0(30012,1,0).
left_of_t0(30012,1,0).
blocked_t0(30012,0,front).
blocked_t0(30012,1,behind).
move_t0(30012,0,right).
move_right_of(30012,0,1).
%Timestep 1
posX_t1(30012,0,-3.053069964293354).
posY_t1(30012,0,0.9394659960423681).
posX_t1(30012,1,0.7641057372093201).
posY_t1(30012,1,0.5826629996299744).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 40012
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(40012,0,cube).
shape(40012,1,sphere).
%Timestep 0
posX_t0(40012,0,-3.053069964293354).
posY_t0(40012,0,0.9394659960423681).
posX_t0(40012,1,0.7641057372093201).
posY_t0(40012,1,0.5826629996299744).
front_of_t0(40012,0,1).
right_of_t0(40012,0,1).
behind_of_t0(40012,1,0).
left_of_t0(40012,1,0).
blocked_t0(40012,0,front).
blocked_t0(40012,1,behind).
move_t0(40012,0,right).
move_right_of(40012,0,1).
%Timestep 1
posX_t1(40012,0,-2.3501241019948442).
posY_t1(40012,0,1.6424118583408778).
posX_t1(40012,1,0.7641057372093201).
posY_t1(40012,1,0.5826629996299744).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 50012
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(50012,0,cube).
shape(50012,1,sphere).
%Timestep 0
posX_t0(50012,0,-2.3501241019948442).
posY_t0(50012,0,1.6424118583408778).
posX_t0(50012,1,0.7641057372093201).
posY_t0(50012,1,0.5826629996299744).
front_of_t0(50012,0,1).
right_of_t0(50012,0,1).
behind_of_t0(50012,1,0).
left_of_t0(50012,1,0).
blocked_t0(50012,0,behind).
blocked_t0(50012,1,behind).
move_t0(50012,0,right).
move_right_of(50012,0,1).
%Timestep 1
posX_t1(50012,0,-1.6457188381527073).
posY_t1(50012,0,2.3468171221830145).
posX_t1(50012,1,0.7641057372093201).
posY_t1(50012,1,0.5826629996299744).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 60012
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(60012,0,cube).
shape(60012,1,sphere).
%Timestep 0
posX_t0(60012,0,-1.6457188381527073).
posY_t0(60012,0,2.3468171221830145).
posX_t0(60012,1,0.7641057372093201).
posY_t0(60012,1,0.5826629996299744).
front_of_t0(60012,0,1).
right_of_t0(60012,0,1).
behind_of_t0(60012,1,0).
left_of_t0(60012,1,0).
blocked_t0(60012,0,front).
blocked_t0(60012,0,behind).
blocked_t0(60012,1,behind).
move_t0(60012,0,right).
move_right_of(60012,0,1).
%Timestep 1
posX_t1(60012,0,-0.9320988014677918).
posY_t1(60012,0,3.0604371588679298).
posX_t1(60012,1,0.7641057372093201).
posY_t1(60012,1,0.5826629996299744).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 13
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(13,0,cube).
shape(13,1,sphere).
%Timestep 0
posX_t0(13,0,-1.148685336112976).
posY_t0(13,0,0.02735402062535286).
posX_t0(13,1,-2.114382743835449).
posY_t0(13,1,1.6101398468017578).
front_of_t0(13,1,0).
right_of_t0(13,0,1).
behind_of_t0(13,0,1).
left_of_t0(13,1,0).
blocked_t0(13,1,right).
blocked_t0(13,1,behind).
blocked_t0(13,0,left).
blocked_t0(13,1,left).
move_t0(13,0,right).
move_right_of(13,0,1).
%Timestep 1
posX_t1(13,0,-0.44346172311559895).
posY_t1(13,0,0.73257763362273).
posX_t1(13,1,-2.114382743835449).
posY_t1(13,1,1.6101398468017578).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 14
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(14,0,cube).
shape(14,1,sphere).
%Timestep 0
posX_t0(14,0,1.78511643409729).
posY_t0(14,0,2.4374117851257324).
posX_t0(14,1,3.238218307495117).
posY_t0(14,1,3.321824789047241).
front_of_t0(14,0,1).
right_of_t0(14,0,1).
behind_of_t0(14,1,0).
left_of_t0(14,1,0).
blocked_t0(14,0,right).
blocked_t0(14,1,left).
move_t0(14,0,front).
move_right_of(14,0,1).
%Timestep 1
posX_t1(14,0,2.493557485991067).
posY_t1(14,0,1.7289706137977228).
posX_t1(14,1,3.238218307495117).
posY_t1(14,1,3.321824789047241).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10014
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10014,0,cube).
shape(10014,1,sphere).
%Timestep 0
posX_t0(10014,0,2.493557485991067).
posY_t0(10014,0,1.7289706137977228).
posX_t0(10014,1,3.238218307495117).
posY_t0(10014,1,3.321824789047241).
front_of_t0(10014,1,0).
right_of_t0(10014,0,1).
behind_of_t0(10014,0,1).
left_of_t0(10014,1,0).
blocked_t0(10014,1,left).
move_t0(10014,0,right).
move_right_of(10014,0,1).
%Timestep 1
posX_t1(10014,0,3.1974721143418745).
posY_t1(10014,0,2.4328852421485303).
posX_t1(10014,1,3.238218307495117).
posY_t1(10014,1,3.321824789047241).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20014
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20014,0,cube).
shape(20014,1,sphere).
%Timestep 0
posX_t0(20014,0,3.1974721143418745).
posY_t0(20014,0,2.4328852421485303).
posX_t0(20014,1,3.238218307495117).
posY_t0(20014,1,3.321824789047241).
front_of_t0(20014,1,0).
right_of_t0(20014,0,1).
behind_of_t0(20014,0,1).
left_of_t0(20014,1,0).
blocked_t0(20014,0,right).
blocked_t0(20014,0,behind).
blocked_t0(20014,1,left).
move_t0(20014,0,front).
move_right_of(20014,0,1).
%Timestep 1
posX_t1(20014,0,3.906885393832884).
posY_t1(20014,0,1.7234718430593827).
posX_t1(20014,1,3.238218307495117).
posY_t1(20014,1,3.321824789047241).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30014
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30014,0,cube).
shape(30014,1,sphere).
%Timestep 0
posX_t0(30014,0,3.906885393832884).
posY_t0(30014,0,1.7234718430593827).
posX_t0(30014,1,3.238218307495117).
posY_t0(30014,1,3.321824789047241).
front_of_t0(30014,1,0).
right_of_t0(30014,0,1).
behind_of_t0(30014,0,1).
left_of_t0(30014,1,0).
blocked_t0(30014,1,left).
move_t0(30014,0,right).
move_right_of(30014,0,1).
%Timestep 1
posX_t1(30014,0,4.6103494628402775).
posY_t1(30014,0,2.426935912066776).
posX_t1(30014,1,3.238218307495117).
posY_t1(30014,1,3.321824789047241).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 15
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(15,0,cube).
shape(15,1,sphere).
%Timestep 0
posX_t0(15,0,-1.5730894804000854).
posY_t0(15,0,2.0857937335968018).
posX_t0(15,1,2.3746135234832764).
posY_t0(15,1,-1.0017435550689697).
front_of_t0(15,0,1).
right_of_t0(15,0,1).
behind_of_t0(15,1,0).
left_of_t0(15,1,0).
blocked_t0(15,1,front).
blocked_t0(15,1,behind).
blocked_t0(15,1,left).
move_t0(15,0,right).
move_right_of(15,0,1).
%Timestep 1
posX_t1(15,0,-0.8639642542519214).
posY_t1(15,0,2.7949189597449657).
posX_t1(15,1,2.3746135234832764).
posY_t1(15,1,-1.0017435550689697).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 16
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(16,0,cube).
shape(16,1,sphere).
%Timestep 0
posX_t0(16,0,0.49303093552589417).
posY_t0(16,0,-0.0684346854686737).
posX_t0(16,1,1.4066470861434937).
posY_t0(16,1,2.3124356269836426).
front_of_t0(16,1,0).
right_of_t0(16,0,1).
behind_of_t0(16,0,1).
left_of_t0(16,1,0).
move_t0(16,0,right).
move_right_of(16,0,1).
%Timestep 1
posX_t1(16,0,1.202588507522149).
posY_t1(16,0,0.6411228865275811).
posX_t1(16,1,1.4066470861434937).
posY_t1(16,1,2.3124356269836426).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10016
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10016,0,cube).
shape(10016,1,sphere).
%Timestep 0
posX_t0(10016,0,1.202588507522149).
posY_t0(10016,0,0.6411228865275811).
posX_t0(10016,1,1.4066470861434937).
posY_t0(10016,1,2.3124356269836426).
front_of_t0(10016,1,0).
right_of_t0(10016,0,1).
behind_of_t0(10016,0,1).
left_of_t0(10016,1,0).
blocked_t0(10016,0,front).
blocked_t0(10016,0,behind).
move_t0(10016,0,right).
move_right_of(10016,0,1).
%Timestep 1
posX_t1(10016,0,1.9166118519404434).
posY_t1(10016,0,1.3551462309458755).
posX_t1(10016,1,1.4066470861434937).
posY_t1(10016,1,2.3124356269836426).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20016
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20016,0,cube).
shape(20016,1,sphere).
%Timestep 0
posX_t0(20016,0,1.9166118519404434).
posY_t0(20016,0,1.3551462309458755).
posX_t0(20016,1,1.4066470861434937).
posY_t0(20016,1,2.3124356269836426).
front_of_t0(20016,1,0).
right_of_t0(20016,0,1).
behind_of_t0(20016,0,1).
left_of_t0(20016,1,0).
blocked_t0(20016,0,front).
blocked_t0(20016,1,front).
blocked_t0(20016,0,behind).
move_t0(20016,0,right).
move_right_of(20016,0,1).
%Timestep 1
posX_t1(20016,0,2.62785927370882).
posY_t1(20016,0,2.066393652714252).
posX_t1(20016,1,1.4066470861434937).
posY_t1(20016,1,2.3124356269836426).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 17
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(17,0,cube).
shape(17,1,sphere).
%Timestep 0
posX_t0(17,0,3.237443208694458).
posY_t0(17,0,-2.968951463699341).
posX_t0(17,1,2.845878839492798).
posY_t0(17,1,0.7965440154075623).
front_of_t0(17,1,0).
right_of_t0(17,0,1).
behind_of_t0(17,0,1).
left_of_t0(17,1,0).
blocked_t0(17,1,left).
move_t0(17,0,right).
move_right_of(17,0,1).
%Timestep 1
posX_t1(17,0,3.9463833096785113).
posY_t1(17,0,-2.2600113627152876).
posX_t1(17,1,2.845878839492798).
posY_t1(17,1,0.7965440154075623).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10017
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10017,0,cube).
shape(10017,1,sphere).
%Timestep 0
posX_t0(10017,0,3.9463833096785113).
posY_t0(10017,0,-2.2600113627152876).
posX_t0(10017,1,2.845878839492798).
posY_t0(10017,1,0.7965440154075623).
front_of_t0(10017,1,0).
right_of_t0(10017,0,1).
behind_of_t0(10017,0,1).
left_of_t0(10017,1,0).
blocked_t0(10017,1,left).
move_t0(10017,0,right).
move_right_of(10017,0,1).
%Timestep 1
posX_t1(10017,0,4.651009970409794).
posY_t1(10017,0,-1.5553847019840057).
posX_t1(10017,1,2.845878839492798).
posY_t1(10017,1,0.7965440154075623).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20017
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20017,0,cube).
shape(20017,1,sphere).
%Timestep 0
posX_t0(20017,0,4.651009970409794).
posY_t0(20017,0,-1.5553847019840057).
posX_t0(20017,1,2.845878839492798).
posY_t0(20017,1,0.7965440154075623).
front_of_t0(20017,1,0).
right_of_t0(20017,0,1).
behind_of_t0(20017,0,1).
left_of_t0(20017,1,0).
blocked_t0(20017,1,left).
move_t0(20017,0,right).
move_right_of(20017,0,1).
%Timestep 1
posX_t1(20017,0,5.358169416995718).
posY_t1(20017,0,-0.8482252553980807).
posX_t1(20017,1,2.845878839492798).
posY_t1(20017,1,0.7965440154075623).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 18
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(18,0,cube).
shape(18,1,sphere).
%Timestep 0
posX_t0(18,0,-1.3960236310958862).
posY_t0(18,0,-0.3450773060321808).
posX_t0(18,1,0.1499304473400116).
posY_t0(18,1,2.496302843093872).
front_of_t0(18,1,0).
right_of_t0(18,0,1).
behind_of_t0(18,0,1).
left_of_t0(18,1,0).
blocked_t0(18,0,right).
blocked_t0(18,1,left).
move_t0(18,0,front).
move_right_of(18,0,1).
%Timestep 1
posX_t1(18,0,-0.6844731419145414).
posY_t1(18,0,-1.0566279151719704).
posX_t1(18,1,0.1499304473400116).
posY_t1(18,1,2.496302843093872).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 10018
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(10018,0,cube).
shape(10018,1,sphere).
%Timestep 0
posX_t0(10018,0,-0.6844731419145414).
posY_t0(10018,0,-1.0566279151719704).
posX_t0(10018,1,0.1499304473400116).
posY_t0(10018,1,2.496302843093872).
front_of_t0(10018,1,0).
right_of_t0(10018,0,1).
behind_of_t0(10018,0,1).
left_of_t0(10018,1,0).
blocked_t0(10018,1,left).
move_t0(10018,0,right).
move_right_of(10018,0,1).
%Timestep 1
posX_t1(10018,0,0.02559530462786297).
posY_t1(10018,0,-0.346559468629566).
posX_t1(10018,1,0.1499304473400116).
posY_t1(10018,1,2.496302843093872).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20018
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20018,0,cube).
shape(20018,1,sphere).
%Timestep 0
posX_t0(20018,0,0.02559530462786297).
posY_t0(20018,0,-0.346559468629566).
posX_t0(20018,1,0.1499304473400116).
posY_t0(20018,1,2.496302843093872).
front_of_t0(20018,1,0).
right_of_t0(20018,0,1).
behind_of_t0(20018,0,1).
left_of_t0(20018,1,0).
blocked_t0(20018,0,front).
blocked_t0(20018,0,behind).
blocked_t0(20018,1,left).
move_t0(20018,0,right).
move_right_of(20018,0,1).
%Timestep 1
posX_t1(20018,0,0.7357686505237157).
posY_t1(20018,0,0.3636138772662867).
posX_t1(20018,1,0.1499304473400116).
posY_t1(20018,1,2.496302843093872).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 30018
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(30018,0,cube).
shape(30018,1,sphere).
%Timestep 0
posX_t0(30018,0,0.7357686505237157).
posY_t0(30018,0,0.3636138772662867).
posX_t0(30018,1,0.1499304473400116).
posY_t0(30018,1,2.496302843093872).
front_of_t0(30018,1,0).
right_of_t0(30018,0,1).
behind_of_t0(30018,0,1).
left_of_t0(30018,1,0).
blocked_t0(30018,1,left).
move_t0(30018,0,right).
move_right_of(30018,0,1).
%Timestep 1
posX_t1(30018,0,1.4484317269580842).
posY_t1(30018,0,1.0762769537006551).
posX_t1(30018,1,0.1499304473400116).
posY_t1(30018,1,2.496302843093872).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 40018
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(40018,0,cube).
shape(40018,1,sphere).
%Timestep 0
posX_t0(40018,0,1.4484317269580842).
posY_t0(40018,0,1.0762769537006551).
posX_t0(40018,1,0.1499304473400116).
posY_t0(40018,1,2.496302843093872).
front_of_t0(40018,1,0).
behind_of_t0(40018,0,1).
blocked_t0(40018,0,front).
blocked_t0(40018,1,left).
move_t0(40018,0,right).
move_right_of(40018,0,1).
%Timestep 1
posX_t1(40018,0,2.151971684438865).
posY_t1(40018,0,1.7798169111814364).
posX_t1(40018,1,0.1499304473400116).
posY_t1(40018,1,2.496302843093872).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 19
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(19,0,cube).
shape(19,1,sphere).
%Timestep 0
posX_t0(19,0,-0.6569451093673706).
posY_t0(19,0,2.0411341190338135).
posX_t0(19,1,0.9414913058280945).
posY_t0(19,1,-1.4343074560165405).
front_of_t0(19,0,1).
right_of_t0(19,1,0).
behind_of_t0(19,1,0).
left_of_t0(19,0,1).
blocked_t0(19,1,behind).
move_right_of(19,0,1).
%Timestep 1
posX_t1(19,0,-0.6569451093673706).
posY_t1(19,0,2.0411341190338135).
posX_t1(19,1,0.9414913058280945).
posY_t1(19,1,-1.4343074560165405).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 20
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
shape(20,0,cube).
shape(20,1,sphere).
%Timestep 0
posX_t0(20,0,0.46698638796806335).
posY_t0(20,0,1.0215617418289185).
posX_t0(20,1,0.908378005027771).
posY_t0(20,1,-2.369767189025879).
front_of_t0(20,0,1).
right_of_t0(20,1,0).
behind_of_t0(20,1,0).
left_of_t0(20,0,1).
blocked_t0(20,1,left).
move_right_of(20,0,1).
%Timestep 1
posX_t1(20,0,0.46698638796806335).
posY_t1(20,0,1.0215617418289185).
posX_t1(20,1,0.908378005027771).
posY_t1(20,1,-2.369767189025879).
