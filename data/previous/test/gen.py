import random

def get_bias():
  return '''%%% -*- Mode: Prolog; -*-
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
mode(posX_t1,none,move_right_of(+,+,-)).
mode(posX_t1,none,posX_t0(+,+,-)).
mode(posX_t1,none,blocked_t0(+,+,-)).
mode(posX_t1,none,right_of_t0(+,+,-)).

mode(posY_t1,none,shape(+,+,-)).
mode(posY_t1,none,move_t0(+,+,-)).
mode(posY_t1,none,move_right_of(+,+,-)).
mode(posY_t1,none,posY_t0(+,+,-)).
mode(posY_t1,none,blocked_t0(+,+,-)).
mode(posY_t1,none,right_of_t0(+,+,-)).


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

%moving only if possible
%move_t0(W,I,Dir) :-  \+blocked_t0(W,I,Dir).


right_of_t0(W,I1,I2) :- 
  I1 \== I2,
  posX_t0(W,I1,X1),
  posX_t0(W,I2,X2),
  X2 > X1.
left_of_t0(W,I1,I2) :- 
  I1 \== I2,
  posX_t0(W,I1,X1),
  posX_t0(W,I2,X2),
  X2 < X1.\n\n'''
  
def move_right_worlds():
  result = ''
  
  shape0 = 'shape(%d,0,cube).\n'
  shape1 = 'shape(%d,1,sphere).\n'
  posX_t0 = 'posX_t0(%d,%d,%f).\n'
  posY_t0 = 'posY_t0(%d,%d,%f).\n'
  move = 'move_right_of(%d,0,1).\n'
  posX_t1 = 'posX_t1(%d,%d,%f).\n'
  posY_t1 = 'posY_t1(%d,%d,%f).\n'

  x0 = random.uniform(-3,3)
  y0 = random.uniform(-3,3)
  
  x1 = x0 + random.uniform(1.1,3.0)
  y1 = random.uniform(-3,3)
  
  for i in range(100):
    result += shape0 % (i)
    result += shape1 % (i)
    result += posX_t0 % (i,0,x0)
    result += posY_t0 % (i,0,y0)
    result += posX_t0 % (i,1,x1)
    result += posY_t0 % (i,1,y1)
    result += move % (i)
    result += posX_t1 % (i,0,x0 + 1)
    result += posY_t1 % (i,0,y0)
    result += posX_t1 % (i,0,x1)
    result += posY_t1 % (i,0,y1)
    result += '\n'
  return result

def move_front_worlds():
  result = ''
  
  shape0 = 'shape(%d,0,cube).\n'
  shape1 = 'shape(%d,1,sphere).\n'
  posX_t0 = 'posX_t0(%d,%d,%f).\n'
  posY_t0 = 'posX_t0(%d,%d,%f).\n'
  blocked = 'blocked_t0(%d,0,right).\n'
  move = 'move_right_of(%d,0,1).\n'
  posX_t1 = 'posX_t1(%d,%d,%f).\n'
  posY_t1 = 'posX_t1(%d,%d,%f).\n'

  x0 = random.uniform(-3,3)
  y0 = random.uniform(-3,3)
  
  x1 = x0 + random.uniform(1.1,3.0)
  y1 = random.uniform(-3,3)
  
  for i in range(100,200):
    result += shape0 % (i)
    result += shape1 % (i)
    result += posX_t0 % (i,0,x0)
    result += posY_t0 % (i,0,y0)
    result += posX_t0 % (i,1,x1)
    result += posY_t0 % (i,1,y1)
    result += blocked % (i)
    result += move % (i)
    result += posX_t1 % (i,0,x0)
    result += posY_t1 % (i,0,y0 - 1)
    result += posX_t1 % (i,0,x1)
    result += posY_t1 % (i,0,y1)
    result += '\n'
  return result

def move_behind_worlds():
  result = ''
  
  shape0 = 'shape(%d,0,cube).\n'
  shape1 = 'shape(%d,1,sphere).\n'
  posX_t0 = 'posX_t0(%d,%d,%f).\n'
  posY_t0 = 'posX_t0(%d,%d,%f).\n'
  blocked1 = 'blocked_t0(%d,0,right).\n'
  blocked2= 'blocked_t0(%d,0,front).\n'
  move = 'move_right_of(%d,0,1).\n'
  posX_t1 = 'posX_t1(%d,%d,%f).\n'
  posY_t1 = 'posX_t1(%d,%d,%f).\n'

  x0 = random.uniform(-3,3)
  y0 = random.uniform(-3,3)
  
  x1 = x0 + random.uniform(1.1,3.0)
  y1 = random.uniform(-3,3)
  
  for i in range(200,300):
    result += shape0 % (i)
    result += shape1 % (i)
    result += posX_t0 % (i,0,x0)
    result += posY_t0 % (i,0,y0)
    result += posX_t0 % (i,1,x1)
    result += posY_t0 % (i,1,y1)
    result += blocked1 % (i)
    result += blocked2 % (i)
    result += move % (i)
    result += posX_t1 % (i,0,x0)
    result += posY_t1 % (i,0,y0 + 1)
    result += posX_t1 % (i,0,x1)
    result += posY_t1 % (i,0,y1)
    result += '\n'
  return result

def no_move_worlds():
  result = ''
  
  shape0 = 'shape(%d,0,cube).\n'
  shape1 = 'shape(%d,1,sphere).\n'
  posX_t0 = 'posX_t0(%d,%d,%f).\n'
  posY_t0 = 'posX_t0(%d,%d,%f).\n'
  move = 'move_right_of(%d,0,1).\n'
  posX_t1 = 'posX_t1(%d,%d,%f).\n'
  posY_t1 = 'posX_t1(%d,%d,%f).\n'

  x1 = random.uniform(-3,3)
  y1 = random.uniform(-3,3)
  
  x0 = x1 + random.uniform(1.1,3.0)
  y0 = random.uniform(-3,3)
  
  for i in range(300,400):
    result += shape0 % (i)
    result += shape1 % (i)
    result += posX_t0 % (i,0,x0)
    result += posY_t0 % (i,0,y0)
    result += posX_t0 % (i,1,x1)
    result += posY_t0 % (i,1,y1)
    result += move % (i)
    result += posX_t1 % (i,0,x0)
    result += posY_t1 % (i,0,y0)
    result += posX_t1 % (i,0,x1)
    result += posY_t1 % (i,0,y1)
    result += '\n'
  return result

def main():
  with open('worlds_facts.pl', 'w') as f:
    f.write(get_bias())
    f.write(move_right_worlds()) 
    f.write(move_front_worlds())
    f.write(move_behind_worlds())
    f.write(no_move_worlds())   

if __name__ == '__main__':
  main()













  
