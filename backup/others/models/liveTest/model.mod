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
  
/* ----------------------------------------------------------------- */# -----------------------------------------------------------------

data;

param: TASKS: p :=

3 1 0 53
1 1 1 21
2 1 2 34
4 1 3 55
5 1 4 95
7 2 0 21
10 2 1 71
8 2 2 26
9 2 3 52
6 2 4 16
11 3 0 12
14 3 1 42
12 3 2 31
13 3 3 39
15 3 4 98
16 4 0 55
18 4 1 77
17 4 2 66
19 4 3 77
20 4 4 79
25 5 0 83
21 5 1 19
24 5 2 64
22 5 3 34
23 5 4 37
30 6 0 92
27 6 1 54
26 6 2 43
28 6 3 62
29 6 4 79
35 7 0 93
33 7 1 87
34 7 2 87
31 7 3 69
32 7 4 77
39 8 0 60
38 8 1 41
37 8 2 38
36 8 3 24
40 8 4 83
43 9 0 44
44 9 1 49
45 9 2 98
41 9 3 17
42 9 4 25
50 10 0 96
47 10 1 75
46 10 2 43
49 10 3 79
48 10 4 96
;
set TASKORDER :=

3 1 0 2 1 2
2 1 2 1 1 1
4 1 3 3 1 0
5 1 4 4 1 3
7 2 0 6 2 4
10 2 1 9 2 3
8 2 2 7 2 0
9 2 3 8 2 2
14 3 1 13 3 3
12 3 2 11 3 0
13 3 3 12 3 2
15 3 4 14 3 1
18 4 1 17 4 2
17 4 2 16 4 0
19 4 3 18 4 1
20 4 4 19 4 3
25 5 0 24 5 2
24 5 2 23 5 4
22 5 3 21 5 1
23 5 4 22 5 3
30 6 0 29 6 4
27 6 1 26 6 2
28 6 3 27 6 1
29 6 4 28 6 3
35 7 0 34 7 2
33 7 1 32 7 4
34 7 2 33 7 1
32 7 4 31 7 3
39 8 0 38 8 1
38 8 1 37 8 2
37 8 2 36 8 3
40 8 4 39 8 0
43 9 0 42 9 4
44 9 1 43 9 0
45 9 2 44 9 1
42 9 4 41 9 3
50 10 0 49 10 3
47 10 1 46 10 2
49 10 3 48 10 4
48 10 4 47 10 1
;
end;
