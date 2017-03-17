proc format; 
   value monthfmt 1='Jan' 2='Feb' 3='Mar'  4='Apr'  5='May'  6='Jun'
                  7='Jul' 8='Aug' 9='Sep' 10='Oct' 11='Nov' 12='Dec';
data month; do i=1 to 12; nmonth=i; format nmonth monthfmt.; output; end; drop i;
data circle; set month;  label x1="cosine" x2="sine" nmonth='month';
pi=constant('pi'); thirtydegrees=pi/6;
if _n_ = 1 then wt = -thirtydegrees; 
wt+thirtydegrees; drop pi thirtydegrees;
x1 = cos(wt); x2 = sin(wt);
proc print; run;
