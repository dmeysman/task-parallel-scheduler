# Task-Parallel Scheduler

## Usage

In order to execute the task-parallel scheduler, simply change your working directory
to that of the scheduler and load its `main` module in [SWI-Prolog](http://www.swi-prolog.org) through [use_module/2](http://www.swi-prolog.org/pldoc/man?predicate=use_module/2)&thinsp;:

```
1 ?- working_directory(_, 'task-parallel-scheduler').
true.

2 ?- use_module(prolog/main).
true.
```

This imports the following predicates into the user namespace&thinsp;:

- execution_time/2 which computes the execution time for a given scheduling problem solution;
- find_heuristically/1 which heuristically generates a scheduling problem solution;
- find_optimal/1 which generates the optimal scheduling problem solution, only for small instances;
- isSolution/1 which generates a valid solution or checks if a given solution is valid;
- pretty_print/1 which outputs a solution in human-readable form; *and*
- speedup/2 which calculates the speed-up factor a parallel solution provides compared to sequential scheduling.

All other predicates are imported prefixed with the name of the module to which they belong.

## Documentation

You can automatically generate the documentation for this pack using [doc_save/2](http://www.swi-prolog.org/pldoc/doc_for?object=pldoc_files:doc_save/2), as it contains structured comments:

```
3 ?- doc_save('prolog', [format(html),if(true),recursive(true),title('Task-Parallel Scheduler')]).
true.
```

You can then access it by navigating to the `prolog/doc` directory in your internet browser.
