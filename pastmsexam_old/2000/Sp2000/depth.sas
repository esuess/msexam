data depth; input depth flow;
logdepth=log(depth);
logflow=log(flow);
depth2=depth*depth;
datalines;
0.34 0.636
0.29 0.319
0.28 0.734
0.42 1.327
0.29 0.487
0.41 0.924
0.76 7.350
0.73 5.890
0.46 1.979
0.40 1.124
;
*a);
proc gplot; plot depth*flow;
run;
*b);
proc reg; model flow=depth/p;
output out=model1 r=resid p=fits;
*c);
plot residual.*depth;
run;
*d);
proc gplot; plot logdepth*logflow; run;
*e);
proc reg; model logflow=logdepth/p;
output out=model2 r=resid p=fits;
*f);
plot residual.*logdepth; run;
proc reg; model flow=depth depth2/p;
output out=model3 r=resid p=fits;
plot residual.*depth residual.*depth2; run;


