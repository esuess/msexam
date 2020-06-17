*proc print; run;
proc sort data=mpgwt5; by name;
proc sort data=mpghp5; by name;
data combined;
merge mpgwt5 mpghp5; by name;
		weightsq=weight*weight;
		hpsq=hp*hp;
label 	hwympg='Highway miles per gallon'
		hp='Horsepower'
		hpsq='Horsepower squared'
		citympg='City miles per gallon'
		name='Model of automobile'
		weight='Weight of automobile'
		weightsq='Weight squared';
proc print; run;
proc reg data=combined;
model citympg=weight;
id name;
output out=modelout1 r=residuals1 p=fits1 ucl=ucl1 lcl=lcl1;
proc sort data=modelout1; by fits1;
proc gplot data=modelout1;
symbol1 v=star cv=black i=none;
symbol2 v=none cv=none i=join l=1 ci=blue;
symbol3 v=none cv=none i=join l=2 ci=green;
plot citympg*fits1=1 fits1*fits1=2 ucl1*fits1=3 lcl1*fits1=3/overlay;
run;

data combined;
set combined;
hybrid=0;
if citympg>45 then hybrid=1; refzero=0;
label hybrid='Indicates a hybrid model';
proc print; run;

proc reg data=combined;
model citympg=weight weightsq hybrid;
id name;
output out=modelout2 r=residuals2 p=fits2 ucl=ucl2 lcl=lcl2;
title 'City mpg predicted by weight, weight2 and hybrid';
proc sort data=modelout2; by fits2;
proc gplot data=modelout2;
symbol1 v=star cv=black i=none;
symbol2 v=none cv=none i=join l=1 ci=blue;
symbol3 v=none cv=none i=join l=2 ci=green;
plot citympg*fits2=1 fits2*fits2=2 ucl2*fits2=3 lcl2*fits2=3/overlay;
run;

proc reg data=combined;
model citympg=weight weightsq hybrid hp;
id name;
output out=modelout3 r=residuals3 p=fits3 ucl=ucl3 lcl=lcl3;
title 'City mpg predicted by weight, horsepower, weight2 and hybrid';
proc sort data=modelout3; by fits3;
proc gplot data=modelout3;
symbol1 v=star cv=black i=none;
symbol2 v=none cv=none i=join l=1 ci=blue;
symbol3 v=none cv=none i=join l=2 ci=green;
plot citympg*fits3=1 fits3*fits3=2 ucl3*fits3=3 lcl3*fits3=3/overlay;
run;

proc univariate data=modelout3 normal plots;
var residuals3;
run;

data modelout3; set modelout3; refzero=0;

proc gplot data=modelout3;
title 'Residual mpg predicted by weight, horsepower, weight2 and hybrid';
plot residuals3*hpsq=1 refzero*hpsq=2/overlay; 
plot residuals3*hp=1 refzero*hp=2/overlay;run;

proc reg data=combined;
model citympg=weight weightsq hybrid hp hpsq/r p;
id name;
output out=modelout4 r=residuals4 p=fits4 ucl=ucl4 lcl=lcl4;
title 'City mpg predicted by weight, horsepower, hp2, weight2 and hybrid';
proc sort data=modelout4; by fits4;
proc gplot data=modelout4;
symbol1 v=star cv=black i=none;
symbol2 v=none cv=none i=join l=1 ci=blue;
symbol3 v=none cv=none i=join l=2 ci=green;
plot citympg*fits4=1 fits4*fits4=2 ucl4*fits4=3 lcl4*fits4=3/overlay;
proc univariate data=modelout4 normal plots;
var residuals4;
run;
