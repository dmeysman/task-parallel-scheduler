/** <module> Human-readable output of scheduling problem solutions

@author Dylan Meysmans <dmeysman@vub.ac.be>
@license MIT
@version 0.1.0
*/

:- module(pretty_print, [pretty_print/1,
                         write_tasks/1,
                         write_list/1]).

%! pretty_print(+S:solution) is det.
%
% Outputs S in human-readable form, if it is a scheduling problem solution.
pretty_print(solution(Ss)) :-
  writeln('# Solution'),
  pretty_print(Ss).

%! pretty_print(+Ss:list) is det.
%
% Outputs Ss in human-readable form, if it is a list of schedules.
pretty_print([]).
pretty_print([schedule(C,Ts)|Ss]) :-
  write('  - '),
  write_tasks(Ts),
  write(' scheduled on core '),
  write(C),
  writeln('.'),
  pretty_print(Ss).

%! write_tasks(+Ts:list) is det.
%
% Outputs Ts as a list of tasks.
write_tasks([]) :-
  write('There are no tasks').
write_tasks([T]) :-
  write('Only task '),
  write_list([T]),
  write(' is').
write_tasks([T,U|Ts]) :-
  write('Tasks '),
  write_list([T,U|Ts]),
  write(' are').

%! write_list(+Xs:list) is det.
%
% Outputs a generic list as its natural language (English) representation.
write_list([X]) :-
  write(X).
write_list([X,Y]) :-
  write(X),
  write(' and '),
  write(Y).
write_list([X,Y,Z|Xs]) :-
  write(X),
  write(', '),
  write_list([Y,Z|Xs]).
