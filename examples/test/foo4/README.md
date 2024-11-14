# TO FIX
It would be nice to show that some operations are certain to happen before others (happen before relation).
For example,

if (line 5) `Reg ! r0` is removed, we have

the send of w1 -> (happens before) the send of r1 
the send of w2 -> (happens before) the send of r2 

for all the possible execution