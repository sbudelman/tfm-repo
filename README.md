# tfm-repo

This repo features a GRASP implementation to solve the Job Shop Problem JSP. These files complement my thesis _A production scheduling service for manufacturing short-run series on machining shops_, as part of the master program on engineering management at Universitat politecnica de Catalunya UPC ESEIAAT.

On its classical form the JSP is defined by _n_ jobs and _m_ machines, each job having one task per machine. These tasks have ana ssociated duration and need to follow an specific sequence inside the job and machines can only perform one task at a time. The problem is to find the sequence of all tasks so that the makespan is the shortest possible[1].

![alt text](http://latex.codecogs.com/svg.latex?1+sin(x))

Another variant also addressed in this implementation consists on assigning due dates and relative weights (priorities) to each job and minimize instead the total weighted tardiness TWT[2]. The TWT is computed as the sum of the product of weights and the difference between the due date and completion date of each job.

A disjunctive graph formulation of JSP[3] has been choosen for convenience when defining neighborhoods during the local search stage and also because it often provides a more intuitive visualization of the solutions. Besides, it allows the application of graph theory procedures such as topological sorting[4] and the assesment of longest path between two nodes[5], essential for defining the solver heuristics.

GRASP stands for Greedy Randomized Adaptive Search Procedures and it is a metaheuristics that has been proved to be proficient at solving the JSP and some of its variants [reference due]. The following pseudo-code outlines it:



As mentioned, the procedure (and of course this very implementation) can be decomposed in two major stages: construction and improvement phases.

During the construction phase, a new solution is generated following

