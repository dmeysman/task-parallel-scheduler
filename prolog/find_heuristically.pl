/** <module> Heuristic scheduling problem solution generation

@author Dylan Meysmans <dmeysman@vub.ac.be>
@license MIT
@version 0.1.0
*/

:- module(find_heuristically, [find_heuristically/1,
                               create_empty_schedules/2,
                               compare_tasks/3,
                               schedule_tasks/4,
                               fastest_core_for/4,
                               fastest_core_for_node/4,
                               zip_with_augmented_process_costs/4,
                               load/3,
                               dependent_tasks/2,
                               total_communication_cost/5,
                               communication_costs/5,
                               zip_with_process_costs/4,
                               schedule_task/4]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(sort)).

:- use_module(execution_time, [communication_cost/4]).
:- use_module(speedup, [find_sequential_on_core/3]).
:- use_module(utilities, [cores/1,
                          tasks/1,
                          depends_on/2,
                          schedule_for_task_core/3]).

%! find_heuristically(-S:solution) is multi.
%
% Instantiates S to a heuristic solution for a scheduling problem.
find_heuristically(solution(Ss)) :-
  cores(Cs),
  create_empty_schedules(Cs, ESs),
  tasks(Ts),
  predsort(compare_tasks, Ts, Us),
  schedule_tasks(Us, Cs, ESs, Ss).

%! create_empty_schedules(+Cs:list, -Ss:list) is det.
%
% Instantiates Ss to the list with an empty schedule for every core.
create_empty_schedules([], []).
create_empty_schedules([C|Cs], [schedule(C,[])|Ss]) :-
  create_empty_schedules(Cs, Ss).

%! compare_tasks(-Delta, +T, +D) is det.
%
% Instantiates Delta to the ordering of T and D, based on the inverse dependency relation.
compare_tasks(<, T, D) :-
  depends_on(T, D).
compare_tasks(>, T, D) :-
  not(depends_on(T, D)).

%! schedule_tasks(+Ts:list, +Cs:list, -Ss:list, -NewSs:list) is multi.
%
% Schedules all tasks from Ts on all cores from Cs to create Ss.
schedule_tasks([], _, Ss, Ss).
schedule_tasks([T|Ts], Cs, Ss, FinalSs) :-
  fastest_core_for(T, Cs, C, Ss),
  schedule_task(T, C, Ss, NewSs),
  schedule_tasks(Ts, Cs, NewSs, FinalSs).

%! fastest_core_for(+T, +Cs:list, -C, +Ss:list) is det.
%
% Instantiates C to the core which can execute T the fastest given the schedules
% under construction in Ss.
fastest_core_for(T, Cs, C, Ss) :-
  depends_on(T, _),
  depends_on(_, T),
  fastest_core_for_node(T, Cs, C, Ss).
fastest_core_for(T, Cs, C, Ss) :-
  depends_on(T, _),
  not(depends_on(_, T)),
  fastest_core_for_sink(T, Cs, C, Ss).
fastest_core_for(T, Cs, C, Ss) :-
  not(depends_on(T, _)),
  depends_on(_, T),
  fastest_core_for_sink(T, Cs, C, Ss).
fastest_core_for(T, Cs, C, Ss) :-
  not(depends_on(T, _)),
  not(depends_on(_, T)),
  fastest_core_for_sink(T, Cs, C, Ss).

%! fastest_core_for_node(+T, +Cs:list, -C, +Ss:list) is det.
%
% Instantiates C to the core which can execute T the fastest given the schedules
% under construction in Ss and the fact that T is not a source, nor a sink.
fastest_core_for_node(T, Cs, C, Ss) :-
  zip_with_augmented_process_costs(T, Cs, Ss, CCs),
  keysort(CCs, SCCs),
  pairs_values(SCCs, SCs),
  nth0(0, SCs, C).

%! zip_with_augmented_process_costs(+T, +Cs:list, +Ss:list, -CCs:list) is det.
%
% Instantiates CCs to the list of pairs of cores and process costs of T on these cores,
% augmented with communication costs to transport data from the core to T's dependencies' cores.
zip_with_augmented_process_costs(_, [], _, []).
zip_with_augmented_process_costs(T, [C|Cs], Ss, [Cost-C|CCs]) :-
  user:process_cost(T, C, ProcessCost),
  load(C, Ss, Load),
  dependent_tasks(T, Ds),
  total_communication_cost(T, C, Ds, Ss, CommunicationCost),
  Cost is Load + ProcessCost + CommunicationCost,
  zip_with_augmented_process_costs(T, Cs, Ss, CCs).

%! load(+C, +Ss:list, -Load:int) is det.
%
% Determines the load of C&thinsp;: the sequential execution time of all tasks scheduled on it.
load(C, Ss, Load) :-
  memberchk(schedule(C,Ts), Ss),
  find_sequential_on_core(C, Ts, Load).

%! dependent_tasks(+T, +Ds:list) is semidet.
%! dependent_tasks(+T, -Ds:list) is det.
%
% Succeeds if Ds is the list of tasks which depend on T.
% You need not instantiate Ds, if you do not, it is instantiated to this list.
dependent_tasks(T, Ds) :-
  findall(D, user:depends_on(D, T, _), Ds).

%! total_communication_cost(+T, +C, +Ds:list, +Ss:list, -Cost:int) is det.
%
% Instantiates Cost to the total communication cost towards Ds' cores if T is scheduled on C.
total_communication_cost(T, C, Ds, Ss, Cost) :-
  communication_costs(T, C, Ds, Ss, Costs),
  sum_list(Costs, Cost).

%! communication_costs(+T, +C, +Ds:list, +Ss:list, -Costs:list) is det.
%
% Instantiates Costs to the list of communication costs towards each of Ds' cores
% if T is scheduled on C.
communication_costs(_, _, [], _, []).
communication_costs(T, C, [D|Ds], Ss, [Cost|Costs]) :-
  schedule_for_task_core(D, Ss, schedule(E,_)),
  user:depends_on(D, T, Data),
  communication_cost(C, E, Data, Cost),
  communication_costs(T, C, Ds, Ss, Costs).

%! fastest_core_for_sink(+T, +Cs:list, -C, +Ss:list) is det.
%
% Instantiates C to the core which can execute T the fastest given the schedules
% under construction in Ss and the fact that T is a source or a sink.
fastest_core_for_sink(T, Cs, C, Ss) :-
  zip_with_process_costs(T, Cs, Ss, CCs),
  keysort(CCs, SCCs),
  pairs_values(SCCs, SCs),
  nth0(0, SCs, C).

%! zip_with_process_costs(+T, +Cs:list, +Ss:list, -CCs:list) is det.
%
% Instantiates CCs to the list of pairs of cores and process costs of T on these cores,
% augmented with communication costs to transport data from the core to T's dependencies' cores.
zip_with_process_costs(_, [], _, []).
zip_with_process_costs(T, [C|Cs], Ss, [Cost-C|CCs]) :-
  load(C, Ss, Load),
  user:process_cost(T, C, ProcessCost),
  Cost is Load + ProcessCost,
  zip_with_process_costs(T, Cs, Ss, CCs).

%! schedule_task(+T, +C, +Ss:list, -NewSs:list) is det.
%
% Instantiates NewSs to Ss with T scheduled on C.
schedule_task(T, C, [schedule(C,Ts)|Ss], [schedule(C,[T|Ts])|Ss]).
schedule_task(T, C, [schedule(D,Ts)|Ss], [schedule(D,Ts)|NewSs]) :-
  C \== D,
  schedule_task(T, C, Ss, NewSs).
