filename q1 url "http://statistics.csueastbay.edu/~mwatnik/msexam/question1.csv";
data q1;
	infile q1 dlm="," firstobs=2;
	input Bottle_Type $	Shelf_Type $ Worker Time;
run;
proc print;
run;
