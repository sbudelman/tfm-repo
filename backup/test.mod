# mrp.mod
# AMPL: Data for mrp formulation

set PP;              # Set of SKU Numbers
set TT;              # Set of Time Buckets 

param P integer := card(PP); # Number of SKUs
param T integer := card(TT); # Number of Time Buckets 
param M  >= 0;               # Large Number

param LT {PP} integer;      # Lead Time 
param R  {PP,PP} integer;   # number of i to make one j
param D {PP, TT} integer;   # External Demand for an item in a period
param I {PP} integer;       # Beginning Inventory 
param LS {PP} integer;      # Lot Size


var d {PP, TT} binary;      # production indicator
var x {PP, TT} >=0;         # number of SKUs to produce 

# -----------------------------------------------------------------

minimize objective: sum {i in PP, t in TT} 
                     (T-t+1) * x[i,t];

# -----------------------------------------------------------------
subject to MaterialRequirement {i in PP, t in TT}:
  (sum {s in TT: s <= t-LT[i] } x[i,s] )
   + I[i] 
   - sum {s in TT: s<=t} 
         (D[i,s] + sum {j in PP} R[i,j]* x[j,s]) 
   >= 0;

subject to LotSize {i in PP, t in TT}:
   x[i,t] - 
   d[i,t]*LS[i] >= 0;
   
subject to ProductionIndicator {i in PP, t in TT}: 
   d[i,t] - x[i,t]/M >=0;
   
data;

set TT := 1
          2
          3
          4
          5
          6
          7
          8;
          
param M := 10000 ;              # Large number
 
param: PP: LT := AJ8172 2       # Items with Lead Times
                 LQ8811 3
                 RN0098 4
                 NN1100 1
                 WN7342 12;

param R :                      # Number of i to produce one j
        AJ8172 LQ8811 RN0098 NN1100 WN7342  :=
AJ8172  0      0      0      0      0
LQ8811  2      0      0      0      0
RN0098  1      0      0      0      0
NN1100  0      1      0      0      0
WN7342  0      1      0      0      0  ;

param D:                       # external demand for i in t
        1 2 3 4 5 6 7 8 :=
AJ8172  20     30     10     20     30     20     30     40
LQ8811  0      0      0      0      0      0      0      0
RN0098  0      0      0      0      0      0      0      0
NN1100  0      0      0      0      0      0      0      0
WN7342  0      0      0      0      0      0      0      0;

param I := AJ8172 90           # Beginning Inventory of SKU i
           LQ8811 300
           RN0098 100
           NN1100 0
           WN7342 900;

param LS := AJ8172 100         # Lot Size
            LQ8811 400
            RN0098 100
            NN1100 1
            WN7342 1000;


end;