/** <module> Scheduling problem solution optimization

@author Dylan Meysmans <dmeysman@vub.ac.be>
@license MIT
@version 0.1.0
*/

:- module(find_optimal, [find_optimal/1,
                         upper_bound/1,
                         update_optimal/2]).

:- dynamic user:optimal/2.

:- use_module(execution_time, [execution_time/2]).
:- use_module(isSolution, [isSolution/1]).

%! find_optimal(-S:solution) is det.
%
% Instantiates S to the optimal solution for a scheduling problem.
find_optimal(_) :-
  upper_bound(B),
  assert(user:optimal(nil, B)),
  isSolution(S),
  execution_time(S, ET),
  update_optimal(S, ET),
  fail.
find_optimal(S) :-
  user:optimal(S, _),
  retract(user:optimal(_, _)).

%! upper_bound(+B:int) is semidet.
%! upper_bound(-B:int) is det.
%
% Succeeds if B is the upper bound on all execution times for the scheduler.
% You need not instantiate B, if you do not, it is instantiated to the upper bound.
% The upper bound is arbitrary, but large enough for our purposes.
upper_bound(1000000000).

%! update_optimal(+S:solution, +ET:int) is det.
%
% Updates the optimal solution for a scheduling problem, if it is improved.
update_optimal(S, ET) :-
  user:optimal(_, OET),
  ET < OET,
  retract(user:optimal(_, _)),
  assert(user:optimal(S, ET)).
update_optimal(_, _).
