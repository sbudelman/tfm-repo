/* instance la01

+++++++++++++++++++++++++++++
  Lawrence 10x5 instance (Table 3, instance 1); also called (setf1) or (F1)
10 5
1 21 0 53 4 95 3 55 2 34
0 21 3 52 4 16 2 26 1 71
3 39 4 98 1 42 2 31 0 12
1 77 0 55 4 79 2 66 3 77
0 83 3 34 2 64 1 19 4 37
1 54 2 43 4 79 0 92 3 62
3 69 4 77 1 87 2 87 0 93
2 38 0 60 1 41 3 24 4 83
3 17 1 49 4 25 0 44 2 98
4 77 3 79 2 43 1 75 0 96
+++++++++++++++++++++++++++++
*/

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
