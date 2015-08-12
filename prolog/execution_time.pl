:- module(execution_time, [execution_time/2,
                           sinks/1,
                           start_times/3,
                           start_time/3,
                           start_time_independent/3,
                           tasks_on_core/3,
                           process_costs/3,
                           end_times_dependencies/4,
                           dependencies/2,
                           communication_cost/4,
                           end_time/3,
                           task_on_core/3]).

/** <module> Execution time for scheduling problem solutions

@author Dylan Meysmans
@license MIT
*/

:- use_module(library(lists)).

%! execution_time(+S:solution, -ET:int) is det.
%
% Instantiates ET to the execution time of S.
execution_time(solution(Ss), ET) :-
  sinks(Ts),
  start_times(Ts, Ss, STs),
  process_costs(Ts, Ss, PCs),
  pairwise_sum(STs, PCs, ETs),
  max_member(ET, ETs).

%! sinks(+Ts:list) is det.
%! sinks(-Ts:list) is det.
%
% Succeeds if Ts is the list of all tasks on which no other task depends.
% You need not instantiate Ts, if you do not, it is instantiated to this list.
sinks(Ts) :-
  findall(T, (user:task(T), not(depends_on(_, T, _))), Ts).

%! start_times(+Ts:list, +Ss:list, -STs:list) is det.
%
% Instantiates STs to the list of start times of Ts in Ss.
start_times([], _, _).
start_times([T|Ts], Ss, [ST|STs]) :-
  start_time(T, Ss, ST),
  start_times(Ts, Ss, STs).

%! start_time(+T, +Ss:list, -ST:int) is det.
%
% Instantiates ST to the earliest possible start time of T in Ss, accounting for dependencies.
start_time(T, Ss, ST) :-
  dependencies(T, Ds),
  end_times_dependencies(Ds, T, Ss, ETs),
  start_time_independent(T, Ss, STI),
  max_member(ST, [STI|ETs]).

%! start_time_independent(+T, +Ss:list, -ST:int) is det.
%
% Instantiates ST to the start time of T in Ss, not accounting for dependencies.
start_time_independent(T, Ss, 0) :-
 task_on_core(T, Ss, C),
 tasks_on_core(C, Ss, [T|_]).
start_time_independent(T, Ss, ST) :-
 task_on_core(T, Ss, C),
 tasks_on_core(C, Ss, Ts),
 nextto(U, T, Ts),
 end_time(U, Ss, ST).

%! tasks_on_core(+C, +Ss:list, -Ts:list) is det.
%
% Instantiates Ts to the list of tasks on C in Ss.
tasks_on_core(C, [schedule(C,Ts)|_], Ts).
tasks_on_core(C, [schedule(D,_)|Ss], Ts) :-
 C \== D,
 tasks_on_core(C, Ss, Ts).

%! process_costs(+Ts:list, +Ss:list, -PCs:list) is det.
%
% Instantiates PCs to the process costs of Ts in Ss.
process_costs([], _, _).
process_costs([T|Ts], Ss, [PC|PCs]) :-
 task_on_core(T, Ss, C),
 process_cost(T, C, PC),
 process_costs(Ts, Ss, PCs).

%! end_times_dependencies(+Ds:list, +T, +Ss:list, -ETs:list) is det.
%
% Instantiates ETs to the sums of the end times of Ds and the communication costs
% between the cores on which T and Ds are scheduled in Ss.
end_times_dependencies(Ds, T, Ss, ETs) :-
  end_times_dependencies(Ds, T, Ss, [], ETs).

end_times_dependencies([], _, _, ETs, ETs).
end_times_dependencies([D|Ds], T, Ss, AETs, ETs) :-
  start_time(D, Ss, ST),
  task_on_core(D, Ss, C),
  process_cost(D, C, Cost),
  depends_on(T, D, Data),
  task_on_core(T, Ss, E),
  communication_cost(C, E, Data, CommunicationCost),
  CET is ST + Cost + CommunicationCost,
  end_times_dependencies(Ds, T, Ss, [CET|AETs], ETs).

pairwise_sum([], [_|_], []) :-
  !. % green cut - prevents duplicates when the input lists are of equal length.
pairwise_sum([_|_], [], []) :-
  !. % green cut - prevents duplicates when the input lists are of equal length.
pairwise_sum([M|Ms], [N|Ns], [S|Ss]) :-
  S is M + N,
  pairwise_sum(Ms, Ns, Ss).

%! dependencies(+T, -Ds) is det.
%! dependencies(-T, +Ds) is det.
%
% Succeeds if Ds is the list of tasks on which T depends.
% You need not instantiate T, if you do not, you need instantiate Ds.
dependencies(T, Ds) :-
  findall(D, depends_on(T, D, _), Ds).

%! communication_cost(+C, +D, +Data:int, -Cost:int) is det.
%
% Instantiates Cost to the cost of sending Data megabytes from C to D.
communication_cost(C, C, _, 0).
communication_cost(C, D, Data, Cost) :-
  C \== D,
  channel(C, D, Latency, Bandwidth),
  Cost is Latency + (Data / Bandwidth).

%! end_time(+T, +Ss:list, -ET) is det.
%
% Instantiates ET to the end time of T in Ss.
end_time(T, Ss, ET) :-
  start_time(T, Ss, ST),
  task_on_core(T, Ss, C),
  process_cost(T, C, Cost),
  ET is ST + Cost.

%! task_on_core(+T, +Ss:list, -C) is det.
%
% Succeeds if T is scheduled on C in Ss.
task_on_core(T, [schedule(C, Ts)|_], C) :-
 memberchk(T, Ts).
task_on_core(T, [_|Ss], C) :-
 task_on_core(T, Ss, C).
