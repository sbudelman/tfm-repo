# https://github.com/jckantor/MathProg-Solver/blob/master/examples/JobShop.mod

/* Data Table 1. Tasks consist of Job, Machine, Duration data*/

set TASKS dimen 2;
param p {TASKS};

/* Data Table 2 */
set TASKORDER within {TASKS,TASKS};

/* JOBS and MACHINES are inferred from the data tables*/
set JOBS := setof {(j,m) in TASKS} j;
set MACHINES := setof {(j,m) in TASKS} m;

param w {JOBS} integer;        # Weight of job j
param d {JOBS} integer;        # Due time of job j

param BigM := 1 + sum {(j,m) in TASKS} p[j,m];

var start {TASKS} >=0;         # Start time operation in machine i for job j
var t {JOBS} >=0;              # Tardiness for job j

# -----------------------------------------------------------------

minimize objective: sum{j in JOBS} w[j]*t[j];

# -----------------------------------------------------------------
s.t. TechnologicalSequence {(k,n,j,m) in TASKORDER}:
   start[k,n] + p[k,n] <= start[j,m];

/* Eliminate conflicts if tasks are require the same machine */
/* y[i,m,j] = 1 if Job i is scheduled before job j on machine m*/

var y{(i,m) in TASKS,(j,m) in TASKS: i < j} binary;

s.t. MachineConflictA {(i,m) in TASKS,(j,m) in TASKS: i < j}:
   start[i,m] + p[i,m] <= start[j,m] + BigM*(1-y[i,m,j]);
   
s.t. MachineConflictB {(i,m) in TASKS,(j,m) in TASKS: i < j}:
   start[j,m] + p[j,m] <= start[i,m] + BigM*y[i,m,j];
   
s.t. Tardiness {j in JOBS, (j,m) in TASKS}:
   t[j] >= start[j,m] + p[j,m] - d[j];
data;

/* Job shop data from Christelle Gueret, Christian Prins,  Marc Sevaux,
"Applications of Optimization with Xpress-MP," Chapter 5, Dash Optimization, 2000. */
  
/* Jobs are broken down into a list of tasks (j,m), each task described by
job name j, machine name m, and duration dur[j,m] */
  
param: TASKS: p :=
1	1	21
1	0	53
1	4	95
1	3	55
1	2	34
2	0	21
2	3	52
2	4	16
2	2	26
2	1	71
3	3	39
3	4	98
3	1	42
3	2	31
3	0	12
4	1	77
4	0	55
4	4	79
4	2	66
4	3	77
5	0	83
5	3	34
5	2	64
5	1	19
5	4	37
6	1	54
6	2	43
6	4	79
6	0	92
6	3	62
7	3	69
7	4	77
7	1	87
7	2	87
7	0	93
8	2	38
8	0	60
8	1	41
8	3	24
8	4	83
9	3	17
9	1	49
9	4	25
9	0	44
9	2	98
10	4	77
10	3	79
10	2	43
10	1	75
10	0	96;

/* List task orderings (k,n,j,m) where task (k,n) must proceed task (j,n) */
  
set TASKORDER :=
1	1	1	0
1	0	1	4
1	4	1	3
1	3	1	2
			
2	0	2	3
2	3	2	4
2	4	2	2
2	2	2	1
			
3	3	3	4
3	4	3	1
3	1	3	2
3	2	3	0
			
4	1	4	0
4	0	4	4
4	4	4	2
4	2	4	3
			
5	0	5	3
5	3	5	2
5	2	5	1
5	1	5	4
			
6	1	6	2
6	2	6	4
6	4	6	0
6	0	6	3
			
7	3	7	4
7	4	7	1
7	1	7	2
7	2	7	0
			
8	2	8	0
8	0	8	1
8	1	8	3
8	3	8	4
			
9	3	9	1
9	1	9	4
9	4	9	0
9	0	9	2
			
10	4	10	3
10	3	10	2
10	2	10	1
10	1	10	0;

/* List of weights w[j] */

param w:= 
1	4
2	4
3	2
4	2
5	2
6	2
7	2
8	2
9	1
10	1;

/* List of due times d[j] */

param d:= 
1	335
2	242
3	289
4	460
5	308
6	429
7	537
8	320
9	303
10	481;

end;
