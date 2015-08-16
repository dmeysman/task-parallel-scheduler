/** <module> Generation of scheduling problem solutions

@author Dylan Meysmans <dmeysman@vub.ac.be>
@license MIT
@version 0.1.0
*/

:- module(isSolution, [isSolution/1,
                       generate_schedules/3,
                       ordered_tasks/2,
                       depends_on_any/2,
                       depends_on/2,
                       selectchk_many/3]).

:- use_module(library(lists)).

:- use_module(utilities, [cores/1,
                          tasks/1,
                          depends_on/2]).

%! isSolution(+S:solution) is semidet.
%! isSolution(-S:solution) is nondet.
%
% Succeeds if S is a solution to the scheduling problem, accounting for dependencies.
% You need not instantiate S, if you do not, it is instantiated to such a solution.
isSolution(solution(Ss)) :-
  cores(Cs),
  tasks(Ts),
  generate_schedules(Cs, Ts, Ss).

%! generate_schedules(+Cs:list, +Ts:list, +Ss:list) is semidet.
%! generate_schedules(+Cs:list, +Ts:list, -Ss:list) is nondet.
%
% Succeeds if Ss is a list of schedules using Cs and Ts, accounting for dependencies.
% You need not instantiate Ss, if you do not, it is instantiated to such a list of schedules.
generate_schedules([], [], []).
generate_schedules([C|Cs], Ts, [schedule(C,Us)|Ss]) :-
  ordered_tasks(Us, Ts),
  selectchk_many(Us, Ts, Vs),
  generate_schedules(Cs, Vs, Ss).

%! ordered_tasks(+Ts:list, +Us:list) is semidet.
%! ordered_tasks(+Ts:list, -Us:list) is nondet.
%
% Succeeds if Us is a subset of tasks of Ts which respect the reflexive and transitive closure
% of the dependency relation. You need not instantiate Ts, if you do not,
% it is instantiated to such a list of tasks.
ordered_tasks([], _).
ordered_tasks([T|Ts], Us) :-
  select(T, Us, Vs),
  ordered_tasks(Ts, Vs),
  not(depends_on_any(T, Ts)).

%! depends_on_any(+T, +Ds:list) is semidet.
%
% Succeeds if T directly or indirectly depends on any element of Ds.
depends_on_any(_, []) :-
  fail.
depends_on_any(T, [D|_]) :-
  depends_on(T, D).
depends_on_any(T, [D|Ds]) :-
  not(depends_on(T, D)),
  depends_on_any(T, Ds).

%! selectchk_many(+Ys:list, +Xs:list, -Zs:list) is semidet.
%
% Instantiates Zs to Xs with all first occurences of elements of Ys removed.
% Fails if an element of Ys does not occur in Xs or when Xs is [].
selectchk_many([], Xs, Xs).
selectchk_many([Y|Ys], Xs, Zs) :-
  selectchk(Y, Xs, As),
  selectchk_many(Ys, As, Zs).
