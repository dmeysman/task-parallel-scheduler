/** <module> Execution time for task scheduling problem solutions

@author Dylan Meysmans <dmeysman@vub.ac.be>
@license MIT
@version 0.1.0
*/

:- module(execution_time, [execution_time/2,
                           sinks/1,
                           start_times/3,
                           start_time/4,
                           dependencies/2,
                           start_time_independent/4,
                           schedule_for_task_core/3,
                           process_costs/3,
                           end_times_dependencies/5,
                           pairwise_sum/3,
                           communication_cost/4,
                           end_time/4]).

:- use_module(library(lists)).

%! execution_time(+S:solution, -ET:int) is semidet.
%
% Instantiates ET to the execution time of S, if S is a valid solution to the task scheduling problem.
execution_time(solution(Ss), ET) :-
  sinks(Ts),
  start_times(Ts, Ss, STs),
  process_costs(Ts, Ss, PCs),
  pairwise_sum(STs, PCs, ETs),
  max_member(ET, ETs).

%! sinks(+Ts:list) is semidet.
%! sinks(-Ts:list) is det.
%
% Succeeds if Ts is the list of all tasks on which no other task depends.
% You need not instantiate Ts, if you do not, it is instantiated to this list.
sinks(Ts) :-
  findall(T, (user:task(T), not(user:depends_on(_, T, _))), Ts).

%! start_times(+Ts:list, +Ss:list, -STs:list) is semidet.
%
% Instantiates STs to the list of start times of Ts in Ss.
start_times([], _, _).
start_times([T|Ts], Ss, [ST|STs]) :-
  schedule_for_task_core(T, Ss, S),
  start_time(T, S, Ss, ST),
  start_times(Ts, Ss, STs).

%! start_time(+T, +S:schedule, +Ss:list, -ST:int) is semidet.
%
% Instantiates ST to the earliest possible start time of T in Ss, accounting for dependencies.
% To avoid the cost of memberchk/2 on Ss, S is the schedule which contains T.
start_time(T, S, Ss, ST) :-
  dependencies(T, Ds),
  end_times_dependencies(Ds, T, S, Ss, ETs),
  start_time_independent(T, S, Ss, STI),
  max_member(ST, [STI|ETs]).

%! dependencies(+T, +Ds:list) is semidet.
%! dependencies(+T, -Ds:list) is det.
%
% Succeeds if Ds is the list of tasks on which T depends.
% You need not instantiate Ds, if you do not, it is instantiated to this list.
dependencies(T, Ds) :-
  findall(D, user:depends_on(T, D, _), Ds).

%! start_time_independent(+T, +S:schedule, +Ss:list, -ST:int) is semidet.
%
% Instantiates ST to the start time of T in Ss, not accounting for dependencies.
% To avoid the cost of memberchk/2 on Ss, S is the schedule which contains T.
start_time_independent(T, schedule(_,[T|_]), _, 0).
start_time_independent(T, schedule(C,Ts), Ss, ET) :-
  nextto(U, T, Ts),
  end_time(U, schedule(C,Ts), Ss, ET).

%! schedule_for_task_core(+T, +Ss:list, -S:schedule) is semidet.
%
% Instantiates S to the schedule for the core on which T is scheduled in Ss.
schedule_for_task_core(T, Ss, schedule(C,Ts)) :-
  member(schedule(C,Ts), Ss),
  memberchk(T, Ts).

%! process_costs(+Ts:list, +Ss:list, -PCs:list) is semidet.
%
% Instantiates PCs to the process costs of Ts in Ss.
process_costs([], _, _).
process_costs([T|Ts], Ss, [PC|PCs]) :-
  schedule_for_task_core(T, Ss, schedule(C,_)),
  process_cost(T, C, PC),
  process_costs(Ts, Ss, PCs).

%! end_times_dependencies(+Ds:list, +T, +S:schedule, +Ss:list, -ETs:list) is semidet.
%
% Instantiates ETs to the sums of the end times of Ds and the communication costs
% between the cores on which T and Ds are scheduled in Ss.
% To avoid the cost of memberchk/2 on Ss, S is the schedule which contains T.
end_times_dependencies(Ds, T, S, Ss, ETs) :-
  end_times_dependencies(Ds, T, S, Ss, [], ETs).

end_times_dependencies([], _, _, _, ETs, ETs).
end_times_dependencies([D|Ds], T, schedule(E,Us), Ss, AETs, ETs) :-
  schedule_for_task_core(D, Ss, schedule(C,Ts)),
  start_time(D, schedule(C,Ts), Ss, ST),
  process_cost(D, C, Cost),
  depends_on(T, D, Data),
  communication_cost(C, E, Data, CommunicationCost),
  ET is ST + Cost + CommunicationCost,
  end_times_dependencies(Ds, T, schedule(E,Us), Ss, [ET|AETs], ETs).

%! pairwise_sum(+Ms:list, +Ns:list, -Ss:list) is semidet.
%
% Instantiates Ss to the list of sums of elements drawn pairwise from Ms and Ns.
pairwise_sum([], [], []).
pairwise_sum([M|Ms], [N|Ns], [S|Ss]) :-
  ground(M),
  ground(N),
  S is M + N,
  pairwise_sum(Ms, Ns, Ss).

%! communication_cost(+C, +D, +Data:int, -Cost:int) is semidet.
%
% Instantiates Cost to the cost of sending Data megabytes from C to D.
communication_cost(C, C, _, 0).
communication_cost(C, D, Data, Cost) :-
  C \== D,
  channel(C, D, Latency, Bandwidth),
  Cost is Latency + (Data / Bandwidth).

%! end_time(+T, +S:schedule, +Ss:list, -ET) is semidet.
%
% Instantiates ET to the end time of T in Ss.
% To avoid the cost of memberchk/2 on Ss, S is the schedule which contains T.
end_time(T, schedule(C,Ts), Ss, ET) :-
  start_time(T, schedule(C,Ts), Ss, ST),
  process_cost(T, C, Cost),
  ET is ST + Cost.
