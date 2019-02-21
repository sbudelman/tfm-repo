/* Data Table 1. Tasks consist of Job, Machine, Duration data */

set TASKS dimen 3;
param p {TASKS};

/* Data Table 2 */
set TASKORDER within {TASKS,TASKS};

param BigM := 1 + sum {(i,j,m) in TASKS} p[i,j,m];

/* Task start time */
var start {TASKS} >=0;         
var makespan >=0;

/* ----------------------------------------------------------------- */

minimize objective: makespan;

/* ----------------------------------------------------------------- */
s.t. TechnologicalSequence {(h,k,n,i,j,m) in TASKORDER}:
   start[h,k,n] + p[h,k,n] <= start[i,j,m];

/* Eliminate conflicts if tasks are require the same machine */
/* y[h,k,m,i,j] = 1 if task h is scheduled before task i on machine m*/

var y{(h,k,m) in TASKS,(i,j,m) in TASKS: h < i} binary;

s.t. MachineConflictA {(h,k,m) in TASKS,(i,j,m) in TASKS: h < i}:
   start[h,k,m] + p[h,k,m] <= start[i,j,m] + BigM*(1-y[h,k,m,i,j]);
   
s.t. MachineConflictB {(h,k,m) in TASKS,(i,j,m) in TASKS: h < i}:
   start[i,j,m] + p[i,j,m] <= start[h,k,m] + BigM*y[h,k,m,i,j];
   
s.t. Makespan {(i,j,m) in TASKS}:
  makespan >= start[i,j,m] + p[i,j,m];