/* Data Table 1. Tasks consist of Job, Machine, Duration data */

set TASKS dimen 2;
param p {TASKS};

/* Data Table 2 */
set TASKORDER within {TASKS,TASKS};

/* JOBS and MACHINES are inferred from the data tables*/
set JOBS := setof {(j,m) in TASKS} j;
set MACHINES := setof {(j,m) in TASKS} m;

param BigM := 1 + sum {(j,m) in TASKS} p[j,m];

/* Start time operation in machine i for job j */
var start {TASKS} >=0;         
var makespan >=0;

/* ----------------------------------------------------------------- */

minimize objective: makespan;

/* ----------------------------------------------------------------- */
s.t. TechnologicalSequence {(k,n,j,m) in TASKORDER}:
   start[k,n] + p[k,n] <= start[j,m];

/* Eliminate conflicts if tasks are require the same machine */
/* y[i,m,j] = 1 if Job i is scheduled before job j on machine m*/

var y{(i,m) in TASKS,(j,m) in TASKS: i < j} binary;

s.t. MachineConflictA {(i,m) in TASKS,(j,m) in TASKS: i < j}:
   start[i,m] + p[i,m] <= start[j,m] + BigM*(1-y[i,m,j]);
   
s.t. MachineConflictB {(i,m) in TASKS,(j,m) in TASKS: i < j}:
   start[j,m] + p[j,m] <= start[i,m] + BigM*y[i,m,j];
   
s.t. Makespan {(j,m) in TASKS}:
  makespan >= start[j,m] + p[j,m];