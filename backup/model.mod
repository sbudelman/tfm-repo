set PP ordered;              
set TT ordered;              

param P integer := card(PP); 
param T integer := card(TT); 
param M  >= 0;               

param LT {PP} integer;      
param R  {PP,PP} integer;   
param D {PP, TT} integer;   
param I {PP} integer;       
param LS {PP} integer;      


var d {PP, TT} binary;      
var x {PP, TT} >=0;        


minimize objective: sum {i in PP, t in TT} 
                     (T-ord(t)+1) * x[i,t];


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

data;

param M := 10000 ;              
 
param: PP: LT := AJ8172 2       
                 LQ8811 3
                 RN0098 4
                 NN1100 1
                 WN7342 12;

set TT := 1jan04                
          2jan04
          3jan04
          4jan04
          5jan04
          6jan04
          7jan04
          8jan04 ;

param R :                      
        AJ8172 LQ8811 RN0098 NN1100 WN7342  :=
AJ8172  0      0      0      0      0
LQ8811  2      0      0      0      0
RN0098  1      0      0      0      0
NN1100  0      1      0      0      0
WN7342  0      1      0      0      0  ;

param D:                       
        1jan04 2jan04 3jan04 4jan04 5jan04 6jan04 7jan04 8jan04 :=
AJ8172  20     30     10     20     30     20     30     40
LQ8811  0      0      0      0      0      0      0      0
RN0098  0      0      0      0      0      0      0      0
NN1100  0      0      0      0      0      0      0      0
WN7342  0      0      0      0      0      0      0      0;

param I := AJ8172 90           
           LQ8811 300
           RN0098 100
           NN1100 0
           WN7342 900;

param LS := AJ8172 100       
            LQ8811 400
            RN0098 100
            NN1100 1
            WN7342 1000;


end;