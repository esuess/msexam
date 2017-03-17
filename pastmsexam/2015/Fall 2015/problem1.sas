data problem1;
	do i=1 to 2;
		if i=2 then acid='yes';
		if i=1 then acid='no';
		do rep=1 to 3;
			do j=1 to 4;
				if j=4 then shape="rectangular";
				else if j=2 then shape="diagonal";
				else if j=3 then shape="check";
				else if j=1 then shape="circular";
				input resin @@;
				output;
			end;
		end;
	end;
	cards;
	9 43 60 77
	13 48 65 70
	12 57 70 91
	15 66 75 97
	13 58 78 108
	20 73 90 99
run;
proc means data=problem1;
	class acid;
	var resin;
run;
proc glm data=problem1;;
	class shape acid;
	model resin=acid;
	output out=myout r=res;
run;
proc univariate normal plot;
	var res;
run;
proc means data=problem1;
	class shape;
	var resin;
run;
proc glm data=problem1;
	class shape acid;
	model resin=shape;
	means shape / hovtest=bf;
	lsmeans shape / pdiff;
	lsmeans shape / pdiff adjust=tukey;
run;
proc glm data=problem1;;
	class shape acid;
	model resin=acid shape acid*shape;
	output out=myout r=res;
run;
proc univariate normal plot;
	var res;
run;
quit;
