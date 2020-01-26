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

%%%%% Use this only if running in probabilistic mode %%%%%
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
:- discontiguous id/2.
:- discontiguous shape/3.
:- discontiguous color/3.
:- discontiguous material/3.
:- discontiguous size/3.
:- discontiguous move_t0/4.
:- discontiguous posX_t0/3.
:- discontiguous posX_t1/3.
:- discontiguous posY_t0/3.
:- discontiguous posY_t1/3.
:- discontiguous front_of_t0/3.
:- discontiguous front_of_t1/3.
:- discontiguous right_of_t0/3.
:- discontiguous right_of_t1/3.
:- discontiguous behind_of_t0/3.
:- discontiguous behind_of_t1/3.
:- discontiguous left_of_t0/3.
:- discontiguous left_of_t1/3.

%Dynamics
:- dynamic id/2.
:- dynamic shape/3.
:- dynamic color/3.
:- dynamic material/3.
:- dynamic size/3.
:- dynamic move_t0/4.
:- dynamic posX_t0/3.
:- dynamic posX_t1/3.
:- dynamic posY_t0/3.
:- dynamic posY_t1/3.
:- dynamic front_of_t0/3.
:- dynamic front_of_t1/3.
:- dynamic right_of_t0/3.
:- dynamic right_of_t1/3.
:- dynamic behind_of_t0/3.
:- dynamic behind_of_t1/3.
:- dynamic left_of_t0/3.
:- dynamic left_of_t1/3.

%Types
base(id(w,i)).
base(shape(w,i,sh)).
base(color(w,i,c)).
base(material(w,i,m)).
base(size(w,i,s)).
base(move_t0(w,i,dir,x)).
base(posX_t0(w,i,x)).
base(posX_t1(w,i,x)).
base(posY_t0(w,i,x)).
base(posY_t1(w,i,x)).
base(front_of_t0(w,i,i)).
base(front_of_t1(w,i,i)).
base(right_of_t0(w,i,i)).
base(right_of_t1(w,i,i)).
base(behind_of_t0(w,i,i)).
base(behind_of_t1(w,i,i)).
base(left_of_t0(w,i,i)).
base(left_of_t1(w,i,i)).

%Modes
mode(id,none,shape(+,+,+)).
mode(id,none,color(+,+,+)).
mode(id,none,material(+,+,+)).
mode(id,none,size(+,+,+)).
mode(id,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(id,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(id,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(id,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(id,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(id,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(id,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(id,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(id,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(id,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(id,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(id,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(id,none,posX_t0(+,+,-)).
mode(id,none,posX_t1(+,+,-)).
mode(id,none,posY_t0(+,+,-)).
mode(id,none,posY_t1(+,+,-)).
mode(id,none,front_of_t0(+,+,+)).
mode(id,none,front_of_t1(+,+,+)).
mode(id,none,right_of_t0(+,+,+)).
mode(id,none,right_of_t1(+,+,+)).
mode(id,none,behind_of_t0(+,+,+)).
mode(id,none,behind_of_t1(+,+,+)).
mode(id,none,left_of_t0(+,+,+)).
mode(id,none,left_of_t1(+,+,+)).

mode(shape,none,id(+,+)).
mode(shape,none,color(+,+,+)).
mode(shape,none,material(+,+,+)).
mode(shape,none,size(+,+,+)).
mode(shape,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(shape,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(shape,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(shape,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(shape,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(shape,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(shape,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(shape,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(shape,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(shape,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(shape,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(shape,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(shape,none,posX_t0(+,+,-)).
mode(shape,none,posX_t1(+,+,-)).
mode(shape,none,posY_t0(+,+,-)).
mode(shape,none,posY_t1(+,+,-)).
mode(shape,none,front_of_t0(+,+,+)).
mode(shape,none,front_of_t1(+,+,+)).
mode(shape,none,right_of_t0(+,+,+)).
mode(shape,none,right_of_t1(+,+,+)).
mode(shape,none,behind_of_t0(+,+,+)).
mode(shape,none,behind_of_t1(+,+,+)).
mode(shape,none,left_of_t0(+,+,+)).
mode(shape,none,left_of_t1(+,+,+)).

mode(color,none,id(+,+)).
mode(color,none,shape(+,+,+)).
mode(color,none,material(+,+,+)).
mode(color,none,size(+,+,+)).
mode(color,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(color,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(color,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(color,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(color,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(color,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(color,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(color,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(color,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(color,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(color,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(color,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(color,none,posX_t0(+,+,-)).
mode(color,none,posX_t1(+,+,-)).
mode(color,none,posY_t0(+,+,-)).
mode(color,none,posY_t1(+,+,-)).
mode(color,none,front_of_t0(+,+,+)).
mode(color,none,front_of_t1(+,+,+)).
mode(color,none,right_of_t0(+,+,+)).
mode(color,none,right_of_t1(+,+,+)).
mode(color,none,behind_of_t0(+,+,+)).
mode(color,none,behind_of_t1(+,+,+)).
mode(color,none,left_of_t0(+,+,+)).
mode(color,none,left_of_t1(+,+,+)).

mode(material,none,id(+,+)).
mode(material,none,shape(+,+,+)).
mode(material,none,color(+,+,+)).
mode(material,none,size(+,+,+)).
mode(material,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(material,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(material,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(material,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(material,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(material,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(material,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(material,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(material,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(material,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(material,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(material,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(material,none,posX_t0(+,+,-)).
mode(material,none,posX_t1(+,+,-)).
mode(material,none,posY_t0(+,+,-)).
mode(material,none,posY_t1(+,+,-)).
mode(material,none,front_of_t0(+,+,+)).
mode(material,none,front_of_t1(+,+,+)).
mode(material,none,right_of_t0(+,+,+)).
mode(material,none,right_of_t1(+,+,+)).
mode(material,none,behind_of_t0(+,+,+)).
mode(material,none,behind_of_t1(+,+,+)).
mode(material,none,left_of_t0(+,+,+)).
mode(material,none,left_of_t1(+,+,+)).

mode(size,none,id(+,+)).
mode(size,none,shape(+,+,+)).
mode(size,none,color(+,+,+)).
mode(size,none,material(+,+,+)).
mode(size,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(size,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(size,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(size,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(size,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(size,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(size,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(size,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(size,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(size,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(size,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(size,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(size,none,posX_t0(+,+,-)).
mode(size,none,posX_t1(+,+,-)).
mode(size,none,posY_t0(+,+,-)).
mode(size,none,posY_t1(+,+,-)).
mode(size,none,front_of_t0(+,+,+)).
mode(size,none,front_of_t1(+,+,+)).
mode(size,none,right_of_t0(+,+,+)).
mode(size,none,right_of_t1(+,+,+)).
mode(size,none,behind_of_t0(+,+,+)).
mode(size,none,behind_of_t1(+,+,+)).
mode(size,none,left_of_t0(+,+,+)).
mode(size,none,left_of_t1(+,+,+)).

mode(move_t0,none,id(+,+)).
mode(move_t0,none,shape(+,+,+)).
mode(move_t0,none,color(+,+,+)).
mode(move_t0,none,material(+,+,+)).
mode(move_t0,none,size(+,+,+)).
mode(move_t0,none,posX_t0(+,+,-)).
mode(move_t0,none,posX_t1(+,+,-)).
mode(move_t0,none,posY_t0(+,+,-)).
mode(move_t0,none,posY_t1(+,+,-)).
mode(move_t0,none,front_of_t0(+,+,+)).
mode(move_t0,none,front_of_t1(+,+,+)).
mode(move_t0,none,right_of_t0(+,+,+)).
mode(move_t0,none,right_of_t1(+,+,+)).
mode(move_t0,none,behind_of_t0(+,+,+)).
mode(move_t0,none,behind_of_t1(+,+,+)).
mode(move_t0,none,left_of_t0(+,+,+)).
mode(move_t0,none,left_of_t1(+,+,+)).
mode(move_t0,maxMode,(posX_t0(+,+,-),posX_t1(+,+,-))).
mode(move_t0,maxMode,(posY_t0(+,+,-),posY_t1(+,+,-))).

mode(move_t0,avg,(posX_t0(+,+,-),posX_t1(+,+,-))).
mode(move_t0,avg,(posY_t0(+,+,-),posY_t1(+,+,-))).
mode(move_t0,cnt,(posX_t0(+,+,-),posX_t1(+,+,-))).
mode(move_t0,cnt,(posY_t0(+,+,-),posY_t1(+,+,-))).

mode(posX_t0,none,id(+,+)).
mode(posX_t0,none,shape(+,+,+)).
mode(posX_t0,none,color(+,+,+)).
mode(posX_t0,none,material(+,+,+)).
mode(posX_t0,none,size(+,+,+)).
mode(posX_t0,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(posX_t0,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(posX_t0,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(posX_t0,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(posX_t0,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(posX_t0,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(posX_t0,none,posX_t1(+,+,-)).
mode(posX_t0,none,posY_t0(+,+,-)).
mode(posX_t0,none,posY_t1(+,+,-)).
mode(posX_t0,none,front_of_t0(+,+,+)).
mode(posX_t0,none,front_of_t1(+,+,+)).
mode(posX_t0,none,right_of_t0(+,+,+)).
mode(posX_t0,none,right_of_t1(+,+,+)).
mode(posX_t0,none,behind_of_t0(+,+,+)).
mode(posX_t0,none,behind_of_t1(+,+,+)).
mode(posX_t0,none,left_of_t0(+,+,+)).
mode(posX_t0,none,left_of_t1(+,+,+)).
mode(posX_t1,none,id(+,+)).
mode(posX_t1,none,shape(+,+,+)).
mode(posX_t1,none,color(+,+,+)).
mode(posX_t1,none,material(+,+,+)).
mode(posX_t1,none,size(+,+,+)).
mode(posX_t1,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(posX_t1,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(posX_t1,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(posX_t1,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(posX_t1,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(posX_t1,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(posX_t1,none,posX_t0(+,+,-)).
mode(posX_t1,none,posY_t0(+,+,-)).
mode(posX_t1,none,posY_t1(+,+,-)).
mode(posX_t1,none,front_of_t0(+,+,+)).
mode(posX_t1,none,front_of_t1(+,+,+)).
mode(posX_t1,none,right_of_t0(+,+,+)).
mode(posX_t1,none,right_of_t1(+,+,+)).
mode(posX_t1,none,behind_of_t0(+,+,+)).
mode(posX_t1,none,behind_of_t1(+,+,+)).
mode(posX_t1,none,left_of_t0(+,+,+)).
mode(posX_t1,none,left_of_t1(+,+,+)).

mode(posY_t0,none,id(+,+)).
mode(posY_t0,none,shape(+,+,+)).
mode(posY_t0,none,color(+,+,+)).
mode(posY_t0,none,material(+,+,+)).
mode(posY_t0,none,size(+,+,+)).
mode(posY_t0,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(posY_t0,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(posY_t0,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(posY_t0,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(posY_t0,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(posY_t0,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(posY_t0,none,posX_t0(+,+,-)).
mode(posY_t0,none,posX_t1(+,+,-)).
mode(posY_t0,none,posY_t1(+,+,-)).
mode(posY_t0,none,front_of_t0(+,+,+)).
mode(posY_t0,none,front_of_t1(+,+,+)).
mode(posY_t0,none,right_of_t0(+,+,+)).
mode(posY_t0,none,right_of_t1(+,+,+)).
mode(posY_t0,none,behind_of_t0(+,+,+)).
mode(posY_t0,none,behind_of_t1(+,+,+)).
mode(posY_t0,none,left_of_t0(+,+,+)).
mode(posY_t0,none,left_of_t1(+,+,+)).
mode(posY_t1,none,id(+,+)).
mode(posY_t1,none,shape(+,+,+)).
mode(posY_t1,none,color(+,+,+)).
mode(posY_t1,none,material(+,+,+)).
mode(posY_t1,none,size(+,+,+)).
mode(posY_t1,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(posY_t1,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(posY_t1,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(posY_t1,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(posY_t1,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(posY_t1,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(posY_t1,none,posX_t0(+,+,-)).
mode(posY_t1,none,posX_t1(+,+,-)).
mode(posY_t1,none,posY_t0(+,+,-)).
mode(posY_t1,none,front_of_t0(+,+,+)).
mode(posY_t1,none,front_of_t1(+,+,+)).
mode(posY_t1,none,right_of_t0(+,+,+)).
mode(posY_t1,none,right_of_t1(+,+,+)).
mode(posY_t1,none,behind_of_t0(+,+,+)).
mode(posY_t1,none,behind_of_t1(+,+,+)).
mode(posY_t1,none,left_of_t0(+,+,+)).
mode(posY_t1,none,left_of_t1(+,+,+)).

mode(front_of_t0,none,id(+,+)).
mode(front_of_t0,none,shape(+,+,+)).
mode(front_of_t0,none,color(+,+,+)).
mode(front_of_t0,none,material(+,+,+)).
mode(front_of_t0,none,size(+,+,+)).
mode(front_of_t0,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(front_of_t0,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(front_of_t0,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(front_of_t0,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(front_of_t0,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(front_of_t0,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(front_of_t0,none,posX_t0(+,+,-)).
mode(front_of_t0,none,posX_t1(+,+,-)).
mode(front_of_t0,none,posY_t0(+,+,-)).
mode(front_of_t0,none,posY_t1(+,+,-)).
mode(front_of_t0,none,front_of_t1(+,+,+)).
mode(front_of_t0,none,right_of_t0(+,+,+)).
mode(front_of_t0,none,right_of_t1(+,+,+)).
mode(front_of_t0,none,behind_of_t0(+,+,+)).
mode(front_of_t0,none,behind_of_t1(+,+,+)).
mode(front_of_t0,none,left_of_t0(+,+,+)).
mode(front_of_t0,none,left_of_t1(+,+,+)).
mode(front_of_t1,none,id(+,+)).
mode(front_of_t1,none,shape(+,+,+)).
mode(front_of_t1,none,color(+,+,+)).
mode(front_of_t1,none,material(+,+,+)).
mode(front_of_t1,none,size(+,+,+)).
mode(front_of_t1,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(front_of_t1,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(front_of_t1,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(front_of_t1,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(front_of_t1,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(front_of_t1,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(front_of_t1,none,posX_t0(+,+,-)).
mode(front_of_t1,none,posX_t1(+,+,-)).
mode(front_of_t1,none,posY_t0(+,+,-)).
mode(front_of_t1,none,posY_t1(+,+,-)).
mode(front_of_t1,none,front_of_t0(+,+,+)).
mode(front_of_t1,none,right_of_t0(+,+,+)).
mode(front_of_t1,none,right_of_t1(+,+,+)).
mode(front_of_t1,none,behind_of_t0(+,+,+)).
mode(front_of_t1,none,behind_of_t1(+,+,+)).
mode(front_of_t1,none,left_of_t0(+,+,+)).
mode(front_of_t1,none,left_of_t1(+,+,+)).

mode(right_of_t0,none,id(+,+)).
mode(right_of_t0,none,shape(+,+,+)).
mode(right_of_t0,none,color(+,+,+)).
mode(right_of_t0,none,material(+,+,+)).
mode(right_of_t0,none,size(+,+,+)).
mode(right_of_t0,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(right_of_t0,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(right_of_t0,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(right_of_t0,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(right_of_t0,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(right_of_t0,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(right_of_t0,none,posX_t0(+,+,-)).
mode(right_of_t0,none,posX_t1(+,+,-)).
mode(right_of_t0,none,posY_t0(+,+,-)).
mode(right_of_t0,none,posY_t1(+,+,-)).
mode(right_of_t0,none,front_of_t0(+,+,+)).
mode(right_of_t0,none,front_of_t1(+,+,+)).
mode(right_of_t0,none,right_of_t1(+,+,+)).
mode(right_of_t0,none,behind_of_t0(+,+,+)).
mode(right_of_t0,none,behind_of_t1(+,+,+)).
mode(right_of_t0,none,left_of_t0(+,+,+)).
mode(right_of_t0,none,left_of_t1(+,+,+)).
mode(right_of_t1,none,id(+,+)).
mode(right_of_t1,none,shape(+,+,+)).
mode(right_of_t1,none,color(+,+,+)).
mode(right_of_t1,none,material(+,+,+)).
mode(right_of_t1,none,size(+,+,+)).
mode(right_of_t1,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(right_of_t1,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(right_of_t1,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(right_of_t1,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(right_of_t1,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(right_of_t1,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(right_of_t1,none,posX_t0(+,+,-)).
mode(right_of_t1,none,posX_t1(+,+,-)).
mode(right_of_t1,none,posY_t0(+,+,-)).
mode(right_of_t1,none,posY_t1(+,+,-)).
mode(right_of_t1,none,front_of_t0(+,+,+)).
mode(right_of_t1,none,front_of_t1(+,+,+)).
mode(right_of_t1,none,right_of_t0(+,+,+)).
mode(right_of_t1,none,behind_of_t0(+,+,+)).
mode(right_of_t1,none,behind_of_t1(+,+,+)).
mode(right_of_t1,none,left_of_t0(+,+,+)).
mode(right_of_t1,none,left_of_t1(+,+,+)).

mode(behind_of_t0,none,id(+,+)).
mode(behind_of_t0,none,shape(+,+,+)).
mode(behind_of_t0,none,color(+,+,+)).
mode(behind_of_t0,none,material(+,+,+)).
mode(behind_of_t0,none,size(+,+,+)).
mode(behind_of_t0,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(behind_of_t0,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(behind_of_t0,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(behind_of_t0,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(behind_of_t0,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(behind_of_t0,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(behind_of_t0,none,posX_t0(+,+,-)).
mode(behind_of_t0,none,posX_t1(+,+,-)).
mode(behind_of_t0,none,posY_t0(+,+,-)).
mode(behind_of_t0,none,posY_t1(+,+,-)).
mode(behind_of_t0,none,front_of_t0(+,+,+)).
mode(behind_of_t0,none,front_of_t1(+,+,+)).
mode(behind_of_t0,none,right_of_t0(+,+,+)).
mode(behind_of_t0,none,right_of_t1(+,+,+)).
mode(behind_of_t0,none,behind_of_t1(+,+,+)).
mode(behind_of_t0,none,left_of_t0(+,+,+)).
mode(behind_of_t0,none,left_of_t1(+,+,+)).
mode(behind_of_t1,none,id(+,+)).
mode(behind_of_t1,none,shape(+,+,+)).
mode(behind_of_t1,none,color(+,+,+)).
mode(behind_of_t1,none,material(+,+,+)).
mode(behind_of_t1,none,size(+,+,+)).
mode(behind_of_t1,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(behind_of_t1,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(behind_of_t1,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(behind_of_t1,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(behind_of_t1,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(behind_of_t1,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(behind_of_t1,none,posX_t0(+,+,-)).
mode(behind_of_t1,none,posX_t1(+,+,-)).
mode(behind_of_t1,none,posY_t0(+,+,-)).
mode(behind_of_t1,none,posY_t1(+,+,-)).
mode(behind_of_t1,none,front_of_t0(+,+,+)).
mode(behind_of_t1,none,front_of_t1(+,+,+)).
mode(behind_of_t1,none,right_of_t0(+,+,+)).
mode(behind_of_t1,none,right_of_t1(+,+,+)).
mode(behind_of_t1,none,behind_of_t0(+,+,+)).
mode(behind_of_t1,none,left_of_t0(+,+,+)).
mode(behind_of_t1,none,left_of_t1(+,+,+)).

mode(left_of_t0,none,id(+,+)).
mode(left_of_t0,none,shape(+,+,+)).
mode(left_of_t0,none,color(+,+,+)).
mode(left_of_t0,none,material(+,+,+)).
mode(left_of_t0,none,size(+,+,+)).
mode(left_of_t0,maxMode,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(left_of_t0,maxMode,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(left_of_t0,avg,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(left_of_t0,avg,(move_t0(+,+,+,-),posY_t0(+,+-))).
mode(left_of_t0,cnt,(move_t0(+,+,+,-),posX_t0(+,+-))).
mode(left_of_t0,cnt,(move_t0(+,+,+,-),posY_t0(+,+-))).

mode(left_of_t0,none,posX_t0(+,+,-)).
mode(left_of_t0,none,posX_t1(+,+,-)).
mode(left_of_t0,none,posY_t0(+,+,-)).
mode(left_of_t0,none,posY_t1(+,+,-)).
mode(left_of_t0,none,front_of_t0(+,+,+)).
mode(left_of_t0,none,front_of_t1(+,+,+)).
mode(left_of_t0,none,right_of_t0(+,+,+)).
mode(left_of_t0,none,right_of_t1(+,+,+)).
mode(left_of_t0,none,behind_of_t0(+,+,+)).
mode(left_of_t0,none,behind_of_t1(+,+,+)).
mode(left_of_t0,none,left_of_t1(+,+,+)).
mode(left_of_t1,none,id(+,+)).
mode(left_of_t1,none,shape(+,+,+)).
mode(left_of_t1,none,color(+,+,+)).
mode(left_of_t1,none,material(+,+,+)).
mode(left_of_t1,none,size(+,+,+)).
mode(left_of_t1,maxMode,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(left_of_t1,maxMode,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(left_of_t1,avg,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(left_of_t1,avg,(move_t0(+,+,+,-),posY_t1(+,+-))).
mode(left_of_t1,cnt,(move_t0(+,+,+,-),posX_t1(+,+-))).
mode(left_of_t1,cnt,(move_t0(+,+,+,-),posY_t1(+,+-))).

mode(left_of_t1,none,posX_t0(+,+,-)).
mode(left_of_t1,none,posX_t1(+,+,-)).
mode(left_of_t1,none,posY_t0(+,+,-)).
mode(left_of_t1,none,posY_t1(+,+,-)).
mode(left_of_t1,none,front_of_t0(+,+,+)).
mode(left_of_t1,none,front_of_t1(+,+,+)).
mode(left_of_t1,none,right_of_t0(+,+,+)).
mode(left_of_t1,none,right_of_t1(+,+,+)).
mode(left_of_t1,none,behind_of_t0(+,+,+)).
mode(left_of_t1,none,behind_of_t1(+,+,+)).
mode(left_of_t1,none,left_of_t0(+,+,+)).

%Aggregations
agg(none).
agg(avg).
agg(maxMod).
agg(cnt).

%Threshold
thres(shape, 3, discrete, [cube,sphere,cylinder]).
thres(color, 3, discrete, [gray,red,blue,green,brown,purple,cyan,yellow]).
thres(material, 3, discrete, [rubber,metal]).
thres(size, 3, discrete, [small,large]).
thres(posX_t0, 3, continuous, []).
thres(posX_t1, 3, continuous, []).
thres(posY_t0, 3, continuous, []).
thres(posY_t1, 3, continuous, []).
thres(move_t0, 4, continuous, []).

%Targets
%learn(posX_t1, 3, 3, continuous).
%learn(posY_t1, 3, 3, continuous).
learn(move_t0, 4, 4, continuous).

%%%%%%%%%%% Declarative Bias ends here %%%%%%%%%%%%

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 13
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
id(13,0). 
id(13,1). 
id(13,2). 
id(13,3). 
shape(13,0,cube).
shape(13,1,sphere).
shape(13,2,cylinder).
shape(13,3,cylinder).
color(13,0,cyan).
color(13,1,brown).
color(13,2,brown).
color(13,3,yellow).
material(13,0,metal).
material(13,1,metal).
material(13,2,metal).
material(13,3,metal).
size(13,0,small).
size(13,1,large).
size(13,2,large).
size(13,3,small).
%Facts affected by time
%Timestep 0
posX_t0(13,0,-2.2046661376953125).
posY_t0(13,0,2.4037704467773438).
posX_t0(13,1,1.0693628787994385).
posY_t0(13,1,-2.656571626663208).
posX_t0(13,2,1.3123503923416138).
posY_t0(13,2,1.291458249092102).
posX_t0(13,3,-0.5256505012512207).
posY_t0(13,3,-2.4255144596099854).
front_of_t0(13,0,1).
front_of_t0(13,0,2).
front_of_t0(13,0,3).
front_of_t0(13,2,1).
front_of_t0(13,2,3).
front_of_t0(13,3,1).
right_of_t0(13,0,2).
right_of_t0(13,1,0).
right_of_t0(13,1,2).
right_of_t0(13,3,0).
right_of_t0(13,3,1).
right_of_t0(13,3,2).
behind_of_t0(13,1,0).
behind_of_t0(13,1,2).
behind_of_t0(13,1,3).
behind_of_t0(13,2,0).
behind_of_t0(13,3,0).
behind_of_t0(13,3,2).
left_of_t0(13,0,1).
left_of_t0(13,0,3).
left_of_t0(13,1,3).
left_of_t0(13,2,0).
left_of_t0(13,2,1).
left_of_t0(13,2,3).
move_t0(13,0,left,-2.8609774112701416).
%Timestep 1
posX_t1(13,0,-2.8609774112701416).
posY_t1(13,0,1.6492801904678345).
posX_t1(13,1,1.0693628787994385).
posY_t1(13,1,-2.656571626663208).
posX_t1(13,2,1.3123503923416138).
posY_t1(13,2,1.291458249092102).
posX_t1(13,3,-0.5256505012512207).
posY_t1(13,3,-2.4255144596099854).
front_of_t1(13,0,1).
front_of_t1(13,0,2).
front_of_t1(13,0,3).
front_of_t1(13,2,1).
front_of_t1(13,2,3).
front_of_t1(13,3,1).
right_of_t1(13,0,2).
right_of_t1(13,1,0).
right_of_t1(13,1,2).
right_of_t1(13,3,0).
right_of_t1(13,3,1).
right_of_t1(13,3,2).
behind_of_t1(13,1,0).
behind_of_t1(13,1,2).
behind_of_t1(13,1,3).
behind_of_t1(13,2,0).
behind_of_t1(13,3,0).
behind_of_t1(13,3,2).
left_of_t1(13,0,1).
left_of_t1(13,0,3).
left_of_t1(13,1,3).
left_of_t1(13,2,0).
left_of_t1(13,2,1).
left_of_t1(13,2,3).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 100013
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
id(100013,0). 
id(100013,1). 
id(100013,2). 
id(100013,3). 
shape(100013,0,cube).
shape(100013,1,sphere).
shape(100013,2,cylinder).
shape(100013,3,cylinder).
color(100013,0,cyan).
color(100013,1,brown).
color(100013,2,brown).
color(100013,3,yellow).
material(100013,0,metal).
material(100013,1,metal).
material(100013,2,metal).
material(100013,3,metal).
size(100013,0,small).
size(100013,1,large).
size(100013,2,large).
size(100013,3,small).
%Facts affected by time
%Timestep 0
posX_t0(100013,0,-2.8609774112701416).
posY_t0(100013,0,1.6492801904678345).
posX_t0(100013,1,1.0693628787994385).
posY_t0(100013,1,-2.656571626663208).
posX_t0(100013,2,1.3123503923416138).
posY_t0(100013,2,1.291458249092102).
posX_t0(100013,3,-0.5256505012512207).
posY_t0(100013,3,-2.4255144596099854).
front_of_t0(100013,0,1).
front_of_t0(100013,0,2).
front_of_t0(100013,0,3).
front_of_t0(100013,2,1).
front_of_t0(100013,2,3).
front_of_t0(100013,3,1).
right_of_t0(100013,0,2).
right_of_t0(100013,1,0).
right_of_t0(100013,1,2).
right_of_t0(100013,3,0).
right_of_t0(100013,3,1).
right_of_t0(100013,3,2).
behind_of_t0(100013,1,0).
behind_of_t0(100013,1,2).
behind_of_t0(100013,1,3).
behind_of_t0(100013,2,0).
behind_of_t0(100013,3,0).
behind_of_t0(100013,3,2).
left_of_t0(100013,0,1).
left_of_t0(100013,0,3).
left_of_t0(100013,1,3).
left_of_t0(100013,2,0).
left_of_t0(100013,2,1).
left_of_t0(100013,2,3).
move_t0(100013,0,left,-3.5172886848449707).
%Timestep 1
posX_t1(100013,0,-3.5172886848449707).
posY_t1(100013,0,0.8947899341583252).
posX_t1(100013,1,1.0693628787994385).
posY_t1(100013,1,-2.656571626663208).
posX_t1(100013,2,1.3123503923416138).
posY_t1(100013,2,1.291458249092102).
posX_t1(100013,3,-0.5256505012512207).
posY_t1(100013,3,-2.4255144596099854).
front_of_t1(100013,0,1).
front_of_t1(100013,0,2).
front_of_t1(100013,0,3).
front_of_t1(100013,2,1).
front_of_t1(100013,2,3).
front_of_t1(100013,3,1).
right_of_t1(100013,0,1).
right_of_t1(100013,0,2).
right_of_t1(100013,1,2).
right_of_t1(100013,3,0).
right_of_t1(100013,3,1).
right_of_t1(100013,3,2).
behind_of_t1(100013,1,0).
behind_of_t1(100013,1,2).
behind_of_t1(100013,1,3).
behind_of_t1(100013,2,0).
behind_of_t1(100013,3,0).
behind_of_t1(100013,3,2).
left_of_t1(100013,0,3).
left_of_t1(100013,1,0).
left_of_t1(100013,1,3).
left_of_t1(100013,2,0).
left_of_t1(100013,2,1).
left_of_t1(100013,2,3).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 200013
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
id(200013,0). 
id(200013,1). 
id(200013,2). 
id(200013,3). 
shape(200013,0,cube).
shape(200013,1,sphere).
shape(200013,2,cylinder).
shape(200013,3,cylinder).
color(200013,0,cyan).
color(200013,1,brown).
color(200013,2,brown).
color(200013,3,yellow).
material(200013,0,metal).
material(200013,1,metal).
material(200013,2,metal).
material(200013,3,metal).
size(200013,0,small).
size(200013,1,large).
size(200013,2,large).
size(200013,3,small).
%Facts affected by time
%Timestep 0
posX_t0(200013,0,-3.5172886848449707).
posY_t0(200013,0,0.8947899341583252).
posX_t0(200013,1,1.0693628787994385).
posY_t0(200013,1,-2.656571626663208).
posX_t0(200013,2,1.3123503923416138).
posY_t0(200013,2,1.291458249092102).
posX_t0(200013,3,-0.5256505012512207).
posY_t0(200013,3,-2.4255144596099854).
front_of_t0(200013,0,1).
front_of_t0(200013,0,2).
front_of_t0(200013,0,3).
front_of_t0(200013,2,1).
front_of_t0(200013,2,3).
front_of_t0(200013,3,1).
right_of_t0(200013,0,1).
right_of_t0(200013,0,2).
right_of_t0(200013,1,2).
right_of_t0(200013,3,0).
right_of_t0(200013,3,1).
right_of_t0(200013,3,2).
behind_of_t0(200013,1,0).
behind_of_t0(200013,1,2).
behind_of_t0(200013,1,3).
behind_of_t0(200013,2,0).
behind_of_t0(200013,3,0).
behind_of_t0(200013,3,2).
left_of_t0(200013,0,3).
left_of_t0(200013,1,0).
left_of_t0(200013,1,3).
left_of_t0(200013,2,0).
left_of_t0(200013,2,1).
left_of_t0(200013,2,3).
move_t0(200013,0,left,-4.1735999584198).
%Timestep 1
posX_t1(200013,0,-4.1735999584198).
posY_t1(200013,0,0.14029967784881592).
posX_t1(200013,1,1.0693628787994385).
posY_t1(200013,1,-2.656571626663208).
posX_t1(200013,2,1.3123503923416138).
posY_t1(200013,2,1.291458249092102).
posX_t1(200013,3,-0.5256505012512207).
posY_t1(200013,3,-2.4255144596099854).
front_of_t1(200013,0,1).
front_of_t1(200013,0,2).
front_of_t1(200013,0,3).
front_of_t1(200013,2,1).
front_of_t1(200013,2,3).
front_of_t1(200013,3,1).
right_of_t1(200013,0,1).
right_of_t1(200013,0,2).
right_of_t1(200013,0,3).
right_of_t1(200013,1,2).
right_of_t1(200013,3,1).
right_of_t1(200013,3,2).
behind_of_t1(200013,1,0).
behind_of_t1(200013,1,2).
behind_of_t1(200013,1,3).
behind_of_t1(200013,2,0).
behind_of_t1(200013,3,0).
behind_of_t1(200013,3,2).
left_of_t1(200013,1,0).
left_of_t1(200013,1,3).
left_of_t1(200013,2,0).
left_of_t1(200013,2,1).
left_of_t1(200013,2,3).
left_of_t1(200013,3,0).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 300013
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
id(300013,0). 
id(300013,1). 
id(300013,2). 
id(300013,3). 
shape(300013,0,cube).
shape(300013,1,sphere).
shape(300013,2,cylinder).
shape(300013,3,cylinder).
color(300013,0,cyan).
color(300013,1,brown).
color(300013,2,brown).
color(300013,3,yellow).
material(300013,0,metal).
material(300013,1,metal).
material(300013,2,metal).
material(300013,3,metal).
size(300013,0,small).
size(300013,1,large).
size(300013,2,large).
size(300013,3,small).
%Facts affected by time
%Timestep 0
posX_t0(300013,0,-4.1735999584198).
posY_t0(300013,0,0.14029967784881592).
posX_t0(300013,1,1.0693628787994385).
posY_t0(300013,1,-2.656571626663208).
posX_t0(300013,2,1.3123503923416138).
posY_t0(300013,2,1.291458249092102).
posX_t0(300013,3,-0.5256505012512207).
posY_t0(300013,3,-2.4255144596099854).
front_of_t0(300013,0,1).
front_of_t0(300013,0,2).
front_of_t0(300013,0,3).
front_of_t0(300013,2,1).
front_of_t0(300013,2,3).
front_of_t0(300013,3,1).
right_of_t0(300013,0,1).
right_of_t0(300013,0,2).
right_of_t0(300013,0,3).
right_of_t0(300013,1,2).
right_of_t0(300013,3,1).
right_of_t0(300013,3,2).
behind_of_t0(300013,1,0).
behind_of_t0(300013,1,2).
behind_of_t0(300013,1,3).
behind_of_t0(300013,2,0).
behind_of_t0(300013,3,0).
behind_of_t0(300013,3,2).
left_of_t0(300013,1,0).
left_of_t0(300013,1,3).
left_of_t0(300013,2,0).
left_of_t0(300013,2,1).
left_of_t0(300013,2,3).
left_of_t0(300013,3,0).
move_t0(300013,1,right,1.7256741523742676).
%Timestep 1
posX_t1(300013,0,-4.1735999584198).
posY_t1(300013,0,0.14029967784881592).
posX_t1(300013,1,1.7256741523742676).
posY_t1(300013,1,-1.9020813703536987).
posX_t1(300013,2,1.3123503923416138).
posY_t1(300013,2,1.291458249092102).
posX_t1(300013,3,-0.5256505012512207).
posY_t1(300013,3,-2.4255144596099854).
front_of_t1(300013,0,1).
front_of_t1(300013,0,2).
front_of_t1(300013,0,3).
front_of_t1(300013,2,1).
front_of_t1(300013,2,3).
front_of_t1(300013,3,1).
right_of_t1(300013,0,1).
right_of_t1(300013,0,2).
right_of_t1(300013,0,3).
right_of_t1(300013,1,2).
right_of_t1(300013,3,1).
right_of_t1(300013,3,2).
behind_of_t1(300013,1,0).
behind_of_t1(300013,1,2).
behind_of_t1(300013,1,3).
behind_of_t1(300013,2,0).
behind_of_t1(300013,3,0).
behind_of_t1(300013,3,2).
left_of_t1(300013,1,0).
left_of_t1(300013,1,3).
left_of_t1(300013,2,0).
left_of_t1(300013,2,1).
left_of_t1(300013,2,3).
left_of_t1(300013,3,0).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 400013
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
id(400013,0). 
id(400013,1). 
id(400013,2). 
id(400013,3). 
shape(400013,0,cube).
shape(400013,1,sphere).
shape(400013,2,cylinder).
shape(400013,3,cylinder).
color(400013,0,cyan).
color(400013,1,brown).
color(400013,2,brown).
color(400013,3,yellow).
material(400013,0,metal).
material(400013,1,metal).
material(400013,2,metal).
material(400013,3,metal).
size(400013,0,small).
size(400013,1,large).
size(400013,2,large).
size(400013,3,small).
%Facts affected by time
%Timestep 0
posX_t0(400013,0,-4.1735999584198).
posY_t0(400013,0,0.14029967784881592).
posX_t0(400013,1,1.7256741523742676).
posY_t0(400013,1,-1.9020813703536987).
posX_t0(400013,2,1.3123503923416138).
posY_t0(400013,2,1.291458249092102).
posX_t0(400013,3,-0.5256505012512207).
posY_t0(400013,3,-2.4255144596099854).
front_of_t0(400013,0,1).
front_of_t0(400013,0,2).
front_of_t0(400013,0,3).
front_of_t0(400013,2,1).
front_of_t0(400013,2,3).
front_of_t0(400013,3,1).
right_of_t0(400013,0,1).
right_of_t0(400013,0,2).
right_of_t0(400013,0,3).
right_of_t0(400013,1,2).
right_of_t0(400013,3,1).
right_of_t0(400013,3,2).
behind_of_t0(400013,1,0).
behind_of_t0(400013,1,2).
behind_of_t0(400013,1,3).
behind_of_t0(400013,2,0).
behind_of_t0(400013,3,0).
behind_of_t0(400013,3,2).
left_of_t0(400013,1,0).
left_of_t0(400013,1,3).
left_of_t0(400013,2,0).
left_of_t0(400013,2,1).
left_of_t0(400013,2,3).
left_of_t0(400013,3,0).
move_t0(400013,1,right,2.3819854259490967).
%Timestep 1
posX_t1(400013,0,-4.1735999584198).
posY_t1(400013,0,0.14029967784881592).
posX_t1(400013,1,2.3819854259490967).
posY_t1(400013,1,-1.1475911140441895).
posX_t1(400013,2,1.3123503923416138).
posY_t1(400013,2,1.291458249092102).
posX_t1(400013,3,-0.5256505012512207).
posY_t1(400013,3,-2.4255144596099854).
front_of_t1(400013,0,1).
front_of_t1(400013,0,2).
front_of_t1(400013,0,3).
front_of_t1(400013,2,1).
front_of_t1(400013,2,3).
front_of_t1(400013,3,1).
right_of_t1(400013,0,1).
right_of_t1(400013,0,2).
right_of_t1(400013,0,3).
right_of_t1(400013,1,2).
right_of_t1(400013,3,1).
right_of_t1(400013,3,2).
behind_of_t1(400013,1,0).
behind_of_t1(400013,1,2).
behind_of_t1(400013,1,3).
behind_of_t1(400013,2,0).
behind_of_t1(400013,3,0).
behind_of_t1(400013,3,2).
left_of_t1(400013,1,0).
left_of_t1(400013,1,3).
left_of_t1(400013,2,0).
left_of_t1(400013,2,1).
left_of_t1(400013,2,3).
left_of_t1(400013,3,0).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 500013
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
id(500013,0). 
id(500013,1). 
id(500013,2). 
id(500013,3). 
shape(500013,0,cube).
shape(500013,1,sphere).
shape(500013,2,cylinder).
shape(500013,3,cylinder).
color(500013,0,cyan).
color(500013,1,brown).
color(500013,2,brown).
color(500013,3,yellow).
material(500013,0,metal).
material(500013,1,metal).
material(500013,2,metal).
material(500013,3,metal).
size(500013,0,small).
size(500013,1,large).
size(500013,2,large).
size(500013,3,small).
%Facts affected by time
%Timestep 0
posX_t0(500013,0,-4.1735999584198).
posY_t0(500013,0,0.14029967784881592).
posX_t0(500013,1,2.3819854259490967).
posY_t0(500013,1,-1.1475911140441895).
posX_t0(500013,2,1.3123503923416138).
posY_t0(500013,2,1.291458249092102).
posX_t0(500013,3,-0.5256505012512207).
posY_t0(500013,3,-2.4255144596099854).
front_of_t0(500013,0,1).
front_of_t0(500013,0,2).
front_of_t0(500013,0,3).
front_of_t0(500013,2,1).
front_of_t0(500013,2,3).
front_of_t0(500013,3,1).
right_of_t0(500013,0,1).
right_of_t0(500013,0,2).
right_of_t0(500013,0,3).
right_of_t0(500013,1,2).
right_of_t0(500013,3,1).
right_of_t0(500013,3,2).
behind_of_t0(500013,1,0).
behind_of_t0(500013,1,2).
behind_of_t0(500013,1,3).
behind_of_t0(500013,2,0).
behind_of_t0(500013,3,0).
behind_of_t0(500013,3,2).
left_of_t0(500013,1,0).
left_of_t0(500013,1,3).
left_of_t0(500013,2,0).
left_of_t0(500013,2,1).
left_of_t0(500013,2,3).
left_of_t0(500013,3,0).
move_t0(500013,1,right,3.038296699523926).
%Timestep 1
posX_t1(500013,0,-4.1735999584198).
posY_t1(500013,0,0.14029967784881592).
posX_t1(500013,1,3.038296699523926).
posY_t1(500013,1,-0.3931008577346802).
posX_t1(500013,2,1.3123503923416138).
posY_t1(500013,2,1.291458249092102).
posX_t1(500013,3,-0.5256505012512207).
posY_t1(500013,3,-2.4255144596099854).
front_of_t1(500013,0,1).
front_of_t1(500013,0,2).
front_of_t1(500013,0,3).
front_of_t1(500013,2,1).
front_of_t1(500013,2,3).
front_of_t1(500013,3,1).
right_of_t1(500013,0,1).
right_of_t1(500013,0,2).
right_of_t1(500013,0,3).
right_of_t1(500013,3,1).
right_of_t1(500013,3,2).
behind_of_t1(500013,1,0).
behind_of_t1(500013,1,2).
behind_of_t1(500013,1,3).
behind_of_t1(500013,2,0).
behind_of_t1(500013,3,0).
behind_of_t1(500013,3,2).
left_of_t1(500013,1,0).
left_of_t1(500013,1,3).
left_of_t1(500013,2,0).
left_of_t1(500013,2,3).
left_of_t1(500013,3,0).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%World 600013
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%Facts unaffected by time
id(600013,0). 
id(600013,1). 
id(600013,2). 
id(600013,3). 
shape(600013,0,cube).
shape(600013,1,sphere).
shape(600013,2,cylinder).
shape(600013,3,cylinder).
color(600013,0,cyan).
color(600013,1,brown).
color(600013,2,brown).
color(600013,3,yellow).
material(600013,0,metal).
material(600013,1,metal).
material(600013,2,metal).
material(600013,3,metal).
size(600013,0,small).
size(600013,1,large).
size(600013,2,large).
size(600013,3,small).
%Facts affected by time
%Timestep 0
posX_t0(600013,0,-4.1735999584198).
posY_t0(600013,0,0.14029967784881592).
posX_t0(600013,1,3.038296699523926).
posY_t0(600013,1,-0.3931008577346802).
posX_t0(600013,2,1.3123503923416138).
posY_t0(600013,2,1.291458249092102).
posX_t0(600013,3,-0.5256505012512207).
posY_t0(600013,3,-2.4255144596099854).
front_of_t0(600013,0,1).
front_of_t0(600013,0,2).
front_of_t0(600013,0,3).
front_of_t0(600013,2,1).
front_of_t0(600013,2,3).
front_of_t0(600013,3,1).
right_of_t0(600013,0,1).
right_of_t0(600013,0,2).
right_of_t0(600013,0,3).
right_of_t0(600013,3,1).
right_of_t0(600013,3,2).
behind_of_t0(600013,1,0).
behind_of_t0(600013,1,2).
behind_of_t0(600013,1,3).
behind_of_t0(600013,2,0).
behind_of_t0(600013,3,0).
behind_of_t0(600013,3,2).
left_of_t0(600013,1,0).
left_of_t0(600013,1,3).
left_of_t0(600013,2,0).
left_of_t0(600013,2,3).
left_of_t0(600013,3,0).
move_t0(600013,1,right,3.694607973098755).
%Timestep 1
posX_t1(600013,0,-4.1735999584198).
posY_t1(600013,0,0.14029967784881592).
posX_t1(600013,1,3.694607973098755).
posY_t1(600013,1,0.3613893985748291).
posX_t1(600013,2,1.3123503923416138).
posY_t1(600013,2,1.291458249092102).
posX_t1(600013,3,-0.5256505012512207).
posY_t1(600013,3,-2.4255144596099854).
front_of_t1(600013,0,1).
front_of_t1(600013,0,2).
front_of_t1(600013,0,3).
front_of_t1(600013,2,1).
front_of_t1(600013,2,3).
front_of_t1(600013,3,1).
right_of_t1(600013,0,1).
right_of_t1(600013,0,2).
right_of_t1(600013,0,3).
right_of_t1(600013,2,1).
right_of_t1(600013,3,1).
right_of_t1(600013,3,2).
behind_of_t1(600013,1,0).
behind_of_t1(600013,1,2).
behind_of_t1(600013,1,3).
behind_of_t1(600013,2,0).
behind_of_t1(600013,3,0).
behind_of_t1(600013,3,2).
left_of_t1(600013,1,0).
left_of_t1(600013,1,2).
left_of_t1(600013,1,3).
left_of_t1(600013,2,0).
left_of_t1(600013,2,3).
left_of_t1(600013,3,0).
