/** <module> Generation of scheduling problem solutions

@author Dylan Meysmans <dmeysman@vub.ac.be>
@license MIT
@version 0.1.0
*/

:- module(isSolution, [isSolution/1,
                       cores/1,
                       tasks/1,
                       test_schedules/3,
                       ordered_tasks/1,
                       select_many/3]).

:- use_module(library(lists)).

%! isSolution(+S:solution) is semidet.
%! isSolution(-S:solution) is nondet.
%
% Succeeds if S is a solution to the scheduling problem, not taking into account dependencies.
% This is necessary for finding all solutions to task scheduling problem instances with independent tasks.
% You need not instantiate S, if you do not, it is instantiated to such a solution.
isSolution(solution(Ss)) :-
  cores(Cs),
  tasks(Ts),
  test_schedules(Cs, Ts, Ss).

%! cores(+Cs:list) is semidet.
%! cores(-Cs:list) is det.
%
% Succeeds if Cs is the list of cores that the system can schedule tasks on.
% You need not instantiate Cs, if you do not, it is instantiated to the list of cores.
cores(Cs) :-
  findall(C, user:core(C), Cs).

%! tasks(+Ts:list) is semidet.
%! tasks(-Ts:list) is det.
%
% Succeeds if Ts is the list of tasks that the system needs schedule.
% You need not instantiate Ts, if you do not, it is instantiated to the list of tasks.
tasks(Ts) :-
  findall(T, user:task(T), Ts).

%! test_schedules(+Cs:list, +Ts:list, +Ss:list) is semidet.
%! test_schedules(+Cs:list, +Ts:list, -Ss:list) is nondet.
%
% Succeeds if Ss is a list of schedules using Cs and Ts.
% You need not instantiate Ss, if you do not, it is instantiated to such a list of schedules.
test_schedules([], [], []).
test_schedules([C|Cs], Ts, [schedule(C,Us)|Ss]) :-
  ordered_tasks(Us),
  select_many(Us, Ts, Vs),
  test_schedules(Cs, Vs, Ss).

%! ordered_tasks(+Ts:list) is semidet.
%! ordered_tasks(-Ts:list) is nondet.
%
% Succeeds if Ts is a list of tasks which satisfy @</2 pairwise.
% You need not instantiate Ts, if you do not, it is instantiated to such a list of tasks.
ordered_tasks([]).
ordered_tasks([_|[]]).
ordered_tasks([T|[U|Ts]]) :-
  user:task(T),
  user:task(U),
  T @< U,
  ordered_tasks([U|Ts]).

%! select_many(+Ys:list, +Xs:list, -Zs:list) is semidet.
%
% Instantiates Zs to Xs with all first occurences of elements of Ys removed.
% Fails if an element of Ys does not occur in Xs or when Xs is [].
select_many([], Xs, Xs).
select_many([Y|Ys], Xs, Zs) :-
  selectchk(Y, Xs, As),
  select_many(Ys, As, Zs).
