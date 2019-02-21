# mrp.mod
# AMPL: Data for mrp formulation

set PP ordered;              # Set of SKU Numbers
set TT ordered;              # Set of Time Buckets 

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
                     (T-ord(t)+1) * x[i,t];

# -----------------------------------------------------------------

subject to MaterialRequirement {i in PP, t in TT}:
  (sum {s in TT: ord(s) <= ord(t)-LT[i] } x[i,s] )
   + I[i] 
   - sum {s in TT: ord(s)<=ord(t)} 
         (D[i,s] + sum {j in PP} R[i,j]* x[j,s]) 
   >= 0;

subject to LotSize {i in PP, t in TT}:
   x[i,t] - d[i,t]*LS[i] >= 0;

subject to ProductionIndicator {i in PP, t in TT}: 
   d[i,t] - x[i,t]/M >=0;
