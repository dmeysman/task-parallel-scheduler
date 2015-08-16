/** <module> Common utilities for the task-parallel scheduler

@author Dylan Meysmans <dmeysman@vub.ac.be>
@license MIT
@version 0.1.0
*/

:- module(utilities, [cores/1,
                      tasks/1,
                      depends_on/2,
                      schedule_for_task_core/3,
                      augmented_less/2]).

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

%! depends_on(+T, +D) is semidet.
%
% Succeeds if T directly or indirectly depends on D.
% Represents the reflexive and transitive closure of the dependency relation.
depends_on(T, T).
depends_on(T, D) :-
  T \== D,
  user:depends_on(T, D, _).
depends_on(T, D) :-
  T \== D,
  not(user:depends_on(T, D, _)),
  user:depends_on(T, V, _),
  depends_on(V, D).

%! schedule_for_task_core(+T, +Ss:list, -S:schedule) is semidet.
%
% Instantiates S to the schedule for the core on which T is scheduled in Ss.
schedule_for_task_core(T, Ss, schedule(C,Ts)) :-
  member(schedule(C,Ts), Ss),
  memberchk(T, Ts).

%! augmented_less(+M, +N) is semidet.
%
% Succeeds if </2 succeeds for M and N, unless M is the atom infinity.
augmented_less(infinity, _) :-
  fail.
augmented_less(M, infinity) :-
  M \== infinity.
augmented_less(M, N) :-
  M \== infinity,
  N \== infinity,
  M < N.
