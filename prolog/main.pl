/** <module> Task-parallel scheduler

@author Dylan Meysmans <dmeysman@vub.ac.be>
@license MIT
@version 0.1.0
*/

:- module(main, [execution_time/2,
                 find_heuristically/1,
                 find_optimal/1,
                 isSolution/1,
                 pretty_print/1,
                 speedup/2]).

:- use_module(execution_time, [execution_time/2]).
:- use_module(find_heuristically, [find_heuristically/1]).
:- use_module(find_optimal, [find_optimal/1]).
:- use_module(isSolution, [isSolution/1]).
:- use_module(pretty_print, [pretty_print/1]).
:- use_module(speedup, [speedup/2]).
