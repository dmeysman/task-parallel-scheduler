/** <module> Speed-up of parallel scheduling problem solutions

@author Dylan Meysmans <dmeysman@vub.ac.be>
@license MIT
@version 0.1.0
*/

:- module(speedup, [speedup/2,
                    find_optimal_sequential/1,
                    find_sequential_on_core/3,
                    update_optimal_sequential/1]).

:- dynamic user:optimal_sequential/1.

:- use_module(library(lists)).

:- use_module(execution_time, [execution_time/2]).
:- use_module(utilities,  [cores/1,
                           tasks/1,
                           augmented_less/2]).

%! speedup(+S:solution, -Speedup:float) is semidet.
%
% Instantiates Speedup to the speed-up S provides over the optimal sequential execution time
% for a scheduling problem.
speedup(S, Speedup) :-
  execution_time(S, PET),
  find_optimal_sequential(SET),
  Speedup is SET / PET.

%! find_optimal_sequential(-ET) is det.
%
% Instantiates ET to the optimal sequential execution time for a scheduling problem.
% We note that this predicate does not sort tasks topologically, because this has no effect
% on sequential execution times.
find_optimal_sequential(ET) :-
	assert(user:optimal_sequential(infinity)),
	cores(Cs),
	tasks(Ts),
	select(C, Cs, _),
	find_sequential_on_core(C, Ts, ET),
	update_optimal_sequential(ET),
	fail.
find_optimal_sequential(ET) :-
	user:optimal_sequential(ET),
	retract(user:optimal_sequential(_)).

%! find_sequential_on_core(+C, +Ts:list, -ET:int) is det.
%
% Instantiates ET to the sequential execution time of Ts on C.
find_sequential_on_core(C, Ts, ET) :-
  find_sequential_on_core(C, Ts, 0, ET).

find_sequential_on_core(_, [], ET, ET).
find_sequential_on_core(C, [T|Ts], AET, ET) :-
  process_cost(T, C, ETT),
  NET is AET + ETT,
	find_sequential_on_core(C, Ts, NET, ET).

%! update_optimal_sequential(+ET:int) is det.
%
% Updates the optimal sequential execution time for a scheduling problem, if it is improved.
update_optimal_sequential(ET) :-
	user:optimal_sequential(OET),
	augmented_less(ET, OET),
	retract(user:optimal_sequential(_)),
	assert(user:optimal_sequential(ET)).
update_optimal_sequential(_).
