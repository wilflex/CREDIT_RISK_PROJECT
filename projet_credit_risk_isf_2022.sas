


/* Recuperation de la base de donnees */

proc import datafile = "C:\Users\Profs\Desktop\mortgage.csv"
	out= echantillon
	dbms=csv replace;
	delimiter= ",";
run ; 
/* Tri de la donnee et elimination des doublons **/
proc sort data = echantillon   
	out= echantillon2 ;
	by id descending time ; 
run ;

proc sort data = echantillon2   
	 nodupkey ;
	by id  ; 
run ;


/* Conteneu de la data */
proc contents data = echantillon2 ; run ;

/* Statistiques descriptives */
proc freq data = echantillon2 ; 
	tables default_time /*status_time*/ ; 
run ;
proc freq data = echantillon2 ; 
	tables status_time ; 
run ;



%macro stat (var);
	proc means data = echantillon2 ;
		var &var. ; 
	run ;
%mend;
%stat (mat_time);
%stat (balance_time);
%stat (LTV_time);
%stat (interest_rate_time);
%stat (hpi_time);
%stat (gdp_time);
%stat (uer_time);
%stat (balance_orig_time);
%stat (FICO_orig_time);
%stat (LTV_orig_time);

/* Creation de classes de variables */
proc rank data =  echantillon2 out= echantillon_class group=10 ;
	var LTV_time ; 
	ranks qLTV_time;
run ;
proc freq data = echantillon_class ;
	tables 	qLTV_time*default_time /chisq out= chi_LTV_time;
run ;
proc transpose data = chi_LTV_time out = chi_LTV_time_1;
	var count ;
	id default_time ;
	by qLTV_time ;
run ;
data chi_LTV_time_1 ;
	set chi_LTV_time_1 ;
	p1=_1/(_0+_1) ; /* P1 est le nombre de default sur la somme des contreparties*/
run ;
proc gplot data = chi_LTV_time_1 ;
	plot p1*qLTV_time ;
run ;
quit ;
/*******************************************************************/
/* Creation de classes de variables en utilisant la macro variable */
/*******************************************************************/
%macro segment (var);
proc rank data =  echantillon2 out= echantillon_class group=10 ;
	var &var. ; 
	ranks q&var.;
run ;
proc freq data = echantillon_class ;
	tables 	q&var.*default_time /chisq out= chi_&var.;
run ;
proc transpose data = chi_&var. out = chi_&var._1;
	var count ;
	id default_time ;
	by q&var. ;
run ;
data chi_&var._1 ;
	set chi_&var._1 ;
	p1=_1/(_0+_1) ; /* P1 est le nombre de default sur la somme des contreparties*/
run ;
proc gplot data = chi_&var._1 ;
	plot p1*q&var. ;
run ;
quit ;

proc means data = echantillon_class;
	var &var. ;
	class q&var. ;
run ;
%mend;
%segment (LTV_time);
%segment (mat_time);
%segment (hpi_time);
%segment (gdp_time);
%segment (uer_time);
%segment (FICO_orig_time);
%segment (interest_rate_time);



/*
%macro classe (var) ;
proc means data = echantillon_class;
	var &var. ;
	class q&var. ;
run ;
%mend ;
%classe (LTV_time) ;
%classe (mat_time) ;
%classe (hpi_time) ;
%classe (gdp_time) ;
%classe (uer_time) ;
%classe (FICO_orig_time) ;
%classe (interest_rate_time) ;
*/

/*******************************************************************/
/** Contruction de la base finale 				*/
/*******************************************************************/
data echantillon_final ;
	set echantillon2 ;

	/* Variable LTV */
	if 0 <= LTV_time <= 82 then LTV_class = "0 - 82   " ;
	else if 82 < LTV_time <= 96 then LTV_class = "83 - 96" ;
	else if 96 < LTV_time <= 100 then LTV_class = "97 - 100" ;
	else if  LTV_time > 100 then LTV_class = "plus 100" ;

	/* Variable Maturite */
	if mat_time <= 139 then Maturity_class = "inf 139   " ;
	else if 139 < mat_time <= 145 then Maturity_class = "140 - 145" ;
	else if  mat_time > 145 then Maturity_class = "plus 145" ;

	/* Variable Indice des prix immo */

	/* Variable PIB */

	/* Variable Chomage */

	/* Variable Score contreparty */

	/* Variable taux dinteret */

	/* Analyse des varaibles binaires */
	if REtype_CO_orig_time = 1 then type_CO = "appartement" ;
	else type_CO = "other";

	if REtype_PU_orig_time = 1 then type_PU = "urban" ;
	else type_PU = "other";

	if REtype_SF_orig_time = 1 then type_SF = "Single-family home" ;
	else type_SF = "other";

	if investor_orig_time = 1 then type_investor = "Investor" ;
	else type_investor = "other";

	/* Suppression dobservations abberrantes */
	if orig_time < 0 then delete ;
	if payoff_time = 2 then delete ;
	if LTV_time = 150 then delete ; 
run ;


/*******************************************************************/
/** REGRESSION  				*/
/*******************************************************************/


proc logistic data = echantillon_final plots (only)=roc descending; 
	class default_time LTV_class Maturity_class type_CO type_PU ;
	model default_time = LTV_class Maturity_class type_CO type_PU 
	/ selection =stepwise ;
	output out = sortie p=prediction ;
run ;






/*******************************************************************/
/** UTILISATION DES PROBA DE DEFAUT - Classes homogenes de risques */
/*******************************************************************/
proc rank data =  sortie out= sortie2 group=10 ;
	var prediction ; 
	ranks CHR;
run ;
proc freq data = sortie2 ;
	tables 	CHR*default_time /chisq out= chi_prediction;
run ;
proc transpose data = chi_prediction out = chi_prediction_1;
	var count ;
	id default_time ;
	by CHR ;
run ;
data chi_prediction_1 ;
	set chi_prediction_1 ;
	taux_de_defaut=_1/(_0+_1) ; /* P1 est le nombre de default sur la somme des contreparties*/
run ;

proc print data = chi_prediction_1 ; run ; 

data sortie2 ;
	set sortie2 ;
	if CHR =0 then proba_estim= 0.07446;
	else if CHR =1 then proba_estim= 0.12044;
	else if CHR =3 then proba_estim= 0.12203;
	else if CHR =4 then proba_estim= 0.14848;
	else if CHR =5 then proba_estim= 0.20968;
	else if CHR =6 then proba_estim= 0.33494;
	else if CHR =7 then proba_estim= 0.46648;
	else if CHR =8 then proba_estim= 0.69328;
	else if CHR =9 then proba_estim= 0.73959;
	else if CHR =. then proba_estim= 0.29412;
run ;

