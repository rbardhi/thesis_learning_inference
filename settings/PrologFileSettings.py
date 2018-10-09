'''
Created on Jun 19, 2018

@author: nitesh
'''

# ############### University ###############
# DECLARATIVE_BIAS = ''':- ['../program/aggregationFunctions'].
# :- discontiguous rating/2.
# :- discontiguous diff/2.
# :- discontiguous course/1.
# :- discontiguous grade/3.
# :- discontiguous sat/3.
# :- discontiguous registration/2.
# :- discontiguous capability/3.
# :- discontiguous salary/3.
# :- discontiguous ra/2.
# :- discontiguous ranking/2.
# :- discontiguous intelligence/2.
# :- discontiguous student/1.
# :- discontiguous teachingability/2.
# :- discontiguous popularity/2.
# :- discontiguous prof/1.
#  
# %Types
# base(rating(c, r)).
# base(diff(c, d)).
# base(course(c)).
# base(grade(c,s,g)).
# base(sat(c,s,x)).
# base(registration(c,s)).
# base(capability(p,s,x1)).
# base(salary(p,s,x2)).
# base(ra(p,s)).
# %base(ranking(s,x3)).
# base(intelligence(s,i)).
# base(student(s)).
# base(teachingability(p,t)).
# base(popularity(p,x4)).
# base(prof(p)).
#  
# %%Modes
# %mode(maxMod, (registration(-,+),rating(+,-))).
# %mode(maxMod, (registration(-,+),diff(+,-))).
# %mode(maxMod, grade(-,+,-)).
# %mode(avg, sat(-,+,-)).
# %mode(avg, capability(-,+,-)).
# %mode(maxMod, salary(-,+,-)).
# %mode(maxMod, (ra(-,+),teachingability(+,-))).
# %mode(maxMod, (ra(-,+),popularity(+,-))).
# %mode(none, intelligence(+,-)).
# %%mode(none, ranking(+,-)).
#  
# %Modes for grade
# mode(none, rating(+,-)).
# mode(none, diff(+,-)).
# mode(none, grade(+,+,-)).
# mode(none, sat(+,+,-)).
# mode(avg, capability(-,+,-)).
# mode(maxMod, salary(-,+,-)).
# mode(maxMod, (ra(-,+),teachingability(+,-))).
# mode(maxMod, (ra(-,+),popularity(+,-))).
# %mode(none, intelligence(+,-)).
# %mode(none, ranking(+,-)).
#  
# %Modes for rating
# mode(none, rating(+,-)).
# mode(none, diff(+,-)).
# %mode(maxMod, (registration(+,-), grade(+,+,-))).
# mode(avg, (registration(+,-), sat(+,+,-))).
# %mode(avg, (registration(+,-), intelligence(+,-))).
# %mode(none, ranking(+,-)).
#  
# %Aggregations
# agg(none).
# agg(avg).
# agg(maxMod).
#  
# %Threshold
# thres(rating, 2, discrete, [low, high]).
# thres(diff, 2, discrete, [low, high]).
# thres(grade, 3, discrete, [a, b, c, d]).
# thres(sat, 3, continuous, []).
# thres(capability, 3, continuous, []).
# thres(salary, 3, discrete, [low, med, high]).
# thres(teachingability, 2, discrete, [mid, high]).
# thres(popularity, 2, discrete, [low, high]).
# thres(intelligence, 2, continuous, []).
# %thres(ranking, 2, continuous, []).
#  
# %Target
# %learn(intelligence, 2, 2, continuous).
# learn(grade, 3, 3, discrete).
# %learn(rating, 2, 2, discrete).
#  
# '''

############### Hepatitis Std (Most Recent) ###############
DECLARATIVE_BIAS = '''%%% -*- Mode: Prolog; -*-
:- use_module(library(lists)).

count(P,Count) :- findall(1,P,L), length(L,Count).

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
avg(X, P, Avg) :- findall(X,P,L), count(P,Cnt), listavg(L, Cnt, Avg).

:- discontiguous fibros/2.
:- discontiguous activity/2.
:- discontiguous bio/1.
:- discontiguous sex/2.
:- discontiguous age/2.
:- discontiguous type/2.
:- discontiguous dispat/1.
:- discontiguous indis/1.
:- discontiguous got/2.
:- discontiguous gpt/2.
:- discontiguous alb/2.
:- discontiguous tbil/2.
:- discontiguous dbil/2.
:- discontiguous che/2.
:- discontiguous ttt/2.
:- discontiguous ztt/2.
:- discontiguous tcho/2.
:- discontiguous tp/2.
:- discontiguous dur/2.
:- discontiguous inf/1.
:- discontiguous rel11/2.
:- discontiguous rel12/2.
:- discontiguous rel13/2.
  
%Types
base(fibros(b, fib)).
base(activity(b, act)).
base(bio(b)).
base(sex(m, s)).
base(age(m, a)).
base(type(m, t)).
base(dispat(m)).
base(indis(in)).
base(got(in, go)).
base(gpt(in, gp)).
base(alb(in, al)).
base(tbil(in, tb)).
base(dbil(in, db)).
base(che(in, ch)).
base(ttt(in, tt)).
base(ztt(in, zt)).
base(tcho(in, tc)).
base(tp(in, t1)).
base(dur(a, du)).
base(inf(a)).
base(rel11(b, m)).
base(rel12(in, m)).
base(rel13(a, m)).
  
  
%Modes for learning type
mode(avg, (rel11(-,+),fibros(+,-))).
mode(min, (rel11(-,+),fibros(+,-))).
mode(max, (rel11(-,+),fibros(+,-))).
mode(minMod, (rel11(-,+),activity(+,-))).
mode(maxMod, (rel11(-,+),activity(+,-))).
mode(none, sex(+,-)).
mode(none, age(+,-)).
mode(none, type(+,-)).
mode(avg, (rel12(-,+),got(+,-))).
mode(min, (rel12(-,+),got(+,-))).
mode(max, (rel12(-,+),got(+,-))).
mode(avg, (rel12(-,+),gpt(+,-))).
mode(min, (rel12(-,+),gpt(+,-))).
mode(max, (rel12(-,+),gpt(+,-))).
mode(maxMod, (rel12(-,+),alb(+,-))).
mode(minMod, (rel12(-,+),alb(+,-))).
mode(maxMod, (rel12(-,+),tbil(+,-))).
mode(minMod, (rel12(-,+),tbil(+,-))).
mode(maxMod, (rel12(-,+),dbil(+,-))).
mode(minMod, (rel12(-,+),dbil(+,-))).
mode(avg, (rel12(-,+),che(+,-))).
mode(min, (rel12(-,+),che(+,-))).
mode(max, (rel12(-,+),che(+,-))).
mode(avg, (rel12(-,+),ttt(+,-))).
mode(min, (rel12(-,+),ttt(+,-))).
mode(max, (rel12(-,+),ttt(+,-))).
mode(avg, (rel12(-,+),ztt(+,-))).
mode(min, (rel12(-,+),ztt(+,-))).
mode(max, (rel12(-,+),ztt(+,-))).
mode(minMod, (rel12(-,+),tcho(+,-))).
mode(maxMod, (rel12(-,+),tcho(+,-))).
mode(avg, (rel12(-,+),tp(+,-))).
mode(min, (rel12(-,+),tp(+,-))).
mode(max, (rel12(-,+),tp(+,-))).
mode(minMod, (rel13(-,+),dur(+,-))).
mode(maxMod, (rel13(-,+),dur(+,-))).
 
 
%%Modes for learning activity
%mode(none, fibros(+,-)).
%mode(minMod, (rel11(+,-), sex(+,-))).
%mode(min, (rel11(+,-), age(+,-))).
%mode(avg, (rel11(+,-), age(+,-))).
%mode(minMod, (rel11(+,-), type(+,-))).
%mode(maxMod, (rel11(+,-), sex(+,-))).
%mode(max, (rel11(+,-), age(+,-))).
%mode(maxMod, (rel11(+,-), type(+,-))).
 
 
%%Modes for learning ztt
%mode(minMod, (rel12(-,+), sex(+,-))).
%mode(min, (rel12(-,+), age(+,-))).
%mode(avg, (rel12(-,+), age(+,-))).
%mode(maxMod, (rel12(-,+), sex(+,-))).
%mode(max, (rel12(-,+), age(+,-))).
%mode(none, got(+,-)).
%mode(none, gpt(+,-)).
%mode(none, alb(+,-)).
%mode(none, tbil(+,-)).
%mode(none, dbil(+,-)).
%mode(none, che(+,-)).
%mode(none, ttt(+,-)).
%mode(none, ztt(+,-)).
%mode(none, tcho(+,-)).
%mode(none, tp(+,-)).
 
 
%%Modes for learning tcho
%mode(minMod, (rel12(-,+), sex(+,-))).
%mode(min, (rel12(-,+), age(+,-))).
%mode(avg, (rel12(-,+), age(+,-))).
%mode(maxMod, (rel12(-,+), sex(+,-))).
%mode(max, (rel12(-,+), age(+,-))).
%mode(none, got(+,-)).
%mode(none, gpt(+,-)).
%mode(none, alb(+,-)).
%mode(none, tbil(+,-)).
%mode(none, dbil(+,-)).
%mode(none, che(+,-)).
%mode(none, ttt(+,-)).
%mode(none, ztt(+,-)).
%mode(none, tcho(+,-)).
%mode(none, tp(+,-)).
 
 
%%Modes for learning dur
%mode(minMod, (rel13(+,-), sex(+,-))).
%mode(min, (rel13(+,-), age(+,-))).
%mode(avg, (rel13(+,-), age(+,-))).
%mode(maxMod, (rel13(+,-), sex(+,-))).
%mode(max, (rel13(+,-), age(+,-))).
 
%Aggregations
agg(none).
agg(avg).
agg(min).
agg(max).
agg(minMod).
agg(maxMod).
  
%Threshold
thres(fibros, 2, continuous, []).
thres(activity, 2, discrete, [zero, one, two, three, four]).
thres(sex, 2, discrete, [m, f]).
thres(age, 2, continuous, []).
thres(type, 2, discrete, [b, c]).
thres(got, 2, continuous, []).
thres(gpt, 2, continuous, []).
thres(alb, 2, discrete, [low, high]).
thres(tbil, 2, discrete, [low, high]).
thres(dbil, 2, discrete, [low, high]).
thres(che, 2, continuous, []).
thres(ttt, 2, continuous, []).
thres(ztt, 2, continuous, []).
thres(tcho, 2, discrete, [tcho0, tcho1, tcho2, tcho3]).
thres(tp, 2, continuous, []).
thres(dur, 2, discrete, [dur0, dur1, dur2, dur3, dur4]).
  
%Target
learn(type, 2, 2, discrete).
%learn(activity, 2, 2, discrete).
%learn(ztt, 2, 2, continuous).
%learn(tcho, 2, 2, discrete).
%learn(dur, 2, 2, discrete).
  
%Facts
'''


# ############### Hepatitis Std ###############
# DECLARATIVE_BIAS = ''':- ['../program/aggregationFunctions'].
# :- discontiguous fibros/2.
# :- discontiguous activity/2.
# :- discontiguous bio/1.
# :- discontiguous sex/2.
# :- discontiguous age/2.
# :- discontiguous type/2.
# :- discontiguous dispat/1.
# :- discontiguous indis/1.
# :- discontiguous got/2.
# :- discontiguous gpt/2.
# :- discontiguous alb/2.
# :- discontiguous tbil/2.
# :- discontiguous dbil/2.
# :- discontiguous che/2.
# :- discontiguous ttt/2.
# :- discontiguous ztt/2.
# :- discontiguous tcho/2.
# :- discontiguous tp/2.
# :- discontiguous dur/2.
# :- discontiguous inf/1.
# :- discontiguous rel11/2.
# :- discontiguous rel12/2.
# :- discontiguous rel13/2.
# 
# %Types
# base(fibros(b, fib)).
# base(activity(b, act)).
# base(bio(b)).
# base(sex(m, s)).
# base(age(m, a)).
# base(type(m, t)).
# base(dispat(m)).
# base(indis(in)).
# base(got(in, go)).
# base(gpt(in, gp)).
# base(alb(in, al)).
# base(tbil(in, tb)).
# base(dbil(in, db)).
# base(che(in, ch)).
# base(ttt(in, tt)).
# base(ztt(in, zt)).
# base(tcho(in, tc)).
# base(tp(in, t1)).
# base(dur(a, du)).
# base(inf(a)).
# base(rel11(b, m)).
# base(rel12(in, m)).
# base(rel13(a, m)).
# 
# 
# %Modes
# mode(avg, (rel11(-,+),fibros(+,-))).
# mode(min, (rel11(-,+),fibros(+,-))).
# mode(max, (rel11(-,+),fibros(+,-))).
# mode(minMod, (rel11(-,+),activity(+,-))).
# mode(maxMod, (rel11(-,+),activity(+,-))).
# mode(none, sex(+,-)).
# mode(none, age(+,-)).
# mode(none, type(+,-)).
# mode(avg, (rel12(-,+),got(+,-))).
# mode(min, (rel12(-,+),got(+,-))).
# mode(max, (rel12(-,+),got(+,-))).
# mode(avg, (rel12(-,+),gpt(+,-))).
# mode(min, (rel12(-,+),gpt(+,-))).
# mode(max, (rel12(-,+),gpt(+,-))).
# mode(max, (rel12(-,+),alb(+,-))).
# mode(min, (rel12(-,+),alb(+,-))).
# mode(avg, (rel12(-,+),alb(+,-))).
# mode(max, (rel12(-,+),tbil(+,-))).
# mode(min, (rel12(-,+),tbil(+,-))).
# mode(avg, (rel12(-,+),tbil(+,-))).
# mode(max, (rel12(-,+),dbil(+,-))).
# mode(min, (rel12(-,+),dbil(+,-))).
# mode(avg, (rel12(-,+),dbil(+,-))).
# mode(avg, (rel12(-,+),che(+,-))).
# mode(min, (rel12(-,+),che(+,-))).
# mode(max, (rel12(-,+),che(+,-))).
# mode(avg, (rel12(-,+),ttt(+,-))).
# mode(min, (rel12(-,+),ttt(+,-))).
# mode(max, (rel12(-,+),ttt(+,-))).
# mode(avg, (rel12(-,+),ztt(+,-))).
# mode(min, (rel12(-,+),ztt(+,-))).
# mode(max, (rel12(-,+),ztt(+,-))).
# mode(avg, (rel12(-,+),tcho(+,-))).
# mode(min, (rel12(-,+),tcho(+,-))).
# mode(max, (rel12(-,+),tcho(+,-))).
# mode(avg, (rel12(-,+),tp(+,-))).
# mode(min, (rel12(-,+),tp(+,-))).
# mode(max, (rel12(-,+),tp(+,-))).
# mode(avg, (rel13(-,+),dur(+,-))).
# mode(min, (rel13(-,+),dur(+,-))).
# mode(max, (rel13(-,+),dur(+,-))).
# 
# 
# %Aggregations
# agg(none).
# agg(avg).
# agg(min).
# agg(max).
# agg(minMod).
# agg(maxMod).
# 
# %Threshold
# thres(fibros, 2, continuous, []).
# thres(activity, 2, discrete, [zero, one, two, three, four]).
# thres(sex, 2, discrete, [m, f]).
# thres(age, 2, continuous, []).
# thres(type, 2, discrete, [b, c]).
# thres(got, 2, continuous, []).
# thres(gpt, 2, continuous, []).
# thres(alb, 2, continuous, []).
# thres(tbil, 2, continuous, []).
# thres(dbil, 2, continuous, []).
# thres(che, 2, continuous, []).
# thres(ttt, 2, continuous, []).
# thres(ztt, 2, continuous, []).
# thres(tcho, 2, continuous, []).
# thres(tp, 2, continuous, []).
# thres(dur, 2, continuous, []).
# 
# 
# %Target
# learn(type, 2, 2, discrete).
# 
# %Facts
# '''
