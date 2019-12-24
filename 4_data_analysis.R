library(haven)

fix_names <- function(df) {
  names(df) <- tolower(names(df))
  df
}

assign1 <-
  read_sas("data/assign1.sas7bdat") %>%
  fix_names()

# /*4_Data Analysis*/
#
#   libname MYLIB 'D:\Dropbox\Admin\SAS Training 2014';
#
#
# /***********************************Looking at our Data and Descriptives**********************************/
#
#   /*Our objective here is to look at our data to understand what we've pulled, where there may be errors, etc.
# 	Then, we ultimately want to create some descriptives on the data we gathered for assignment #1*/
#
# *First, we may want to check the number of observations that we have per year - PROC FREQ is a very useful tool here;
# PROC FREQ data=MyLib.Assign1;
# 	TABLES YEAR; RUN;
assign1 %>%
  count(year)


#
# *Cross Tabs are also very useful and can be accomplished within PROC FREQ
# 	For example, the below code looks at a cross-tab of year * fiscal-year-end month
# 	Note, the options eliminate the percentages that are normally included in PROC FREQ;
# PROC FREQ data=MyLib.Assign1;
# 	TABLES YEAR*FYR / norow nocol nopercent; RUN;
#
#
# *I'm excited that I have my data... I can't wait to look at some preliminary correlations;
#
# /*Pearson Correlations*/
# PROC CORR data=MyLib.Assign1 noprob;
# 	VAR FUT_ROA ROA deltaROA deltaTURN deltaMARGIN;
# RUN;
#
# /*Spearman Correlations*/
# PROC CORR data=MyLib.Assign1 SPEARMAN;
# 	VAR FUT_ROA ROA deltaROA deltaTURN deltaMARGIN;
# RUN;
#
# */Yikes, I'm really confused now. Look at the differences between the Pearson and Spearman correlations
# (e.g., bet FUT_ROA and ROA).*/
#
#   /*Perhaps I jumped the gun, we probably need to look at our data a bit before we move on*/
#
#   *First, let's view the distributions of a few of our key variables;
# PROC UNIVARIATE data=MyLib.Assign1;
# 	VAR ROA FUT_ROA deltaROA; RUN;
#
# /*Notice the extreme observations.  These likely explain the differences bet the Pearson and Spearman Correlations*/
#
# /*Below are some useful graphics to view your data - NOT IN THE CODE FILE I SENT LAST NIGHT*/
# PROC UNIVARIATE data=MyLib.Assign1;
# VAR ROA;
# histogram ROA; RUN;
#
# PROC SORT data=MyLib.Assign1; BY YEAR; RUN;
# PROC BOXPLOT data=MyLib.Assign1;
# 	PLOT ROA*YEAR;
# RUN;
#
# /*So it's at this point that we need to consider doing something about the outliers in our sample*/
#   /*Common practice in the literature is through winsorization*/
#
#
#   *I'm going to show you how you might winsorize using PROC UNIVARIATE, however we will later cover a macro
# 	that will help improve efficiency.  It's important that you understand what is going on though*/
#
#   *Let's winsorize each of our variables at the 1 and 99 percent levels (overall).;
# 	*Step 1: Determine what the 1 and 99 percentiles are;
# PROC UNIVARIATE data=MyLib.Assign1 noprint;
# 	VAR FUT_ROA ROA deltaROA deltaTURN deltaMARGIN;
# 	OUTPUT OUT= Extreme
# 	p1=L_FUT_ROA L_ROA L_deltaROA L_deltaTURN L_deltaMARGIN
# 	p99=H_FUT_ROA H_ROA H_deltaROA H_deltaTURN H_deltaMARGIN;
# RUN;
#
# 	*Step 2: Add these 'extreme' values to each line in our primary data set (notice how we use MergeDum)
# 				Then replace values that exceed these extremes;
# DATA Extreme; SET Extreme; MergeDum=1; RUN;
#
# Data Winsorize;
# 	SET MyLib.Assign1;
# 	MergeDum=1;
# RUN;
#
# Data Assign1_WIN;
# 	MERGE Winsorize Extreme; /*this also could be done using SQL join, as discussed earlier*/
# 	BY MergeDum;
# 		IF FUT_ROA < L_FUT_ROA THEN FUT_ROA=L_FUT_ROA;
# 		IF FUT_ROA > H_FUT_ROA THEN FUT_ROA=H_FUT_ROA;
# 		IF ROA < L_ROA THEN ROA=L_ROA;
# 		IF ROA > H_ROA THEN ROA=H_ROA;
# 		IF deltaROA < L_deltaROA THEN deltaROA=L_deltaROA;
# 		IF deltaROA > H_deltaROA THEN deltaROA=H_deltaROA;
# 		IF deltaTURN < L_deltaTURN THEN deltaTURN=L_deltaTURN;
# 		IF deltaTURN > H_deltaTURN THEN deltaTURN=H_deltaTURN;
# 		IF deltaMARGIN < L_deltaMARGIN THEN deltaMARGIN=L_deltaMARGIN;
# 		IF deltaMARGIN > H_deltaMARGIN THEN deltaMARGIN=H_deltaMARGIN;
# 	DROP L_FUT_ROA L_ROA L_deltaROA L_deltaTURN L_deltaMARGIN
# 			H_FUT_ROA H_ROA H_deltaROA H_deltaTURN H_deltaMARGIN MergeDum;
# RUN;
#
# *Now let's re-run our correlation tables from above with the new data;
# /*Note how much closer the correlations are*/
#   PROC CORR data=Assign1_WIN;
# VAR FUT_ROA ROA deltaROA deltaTURN deltaMARGIN;
# RUN;
#
# PROC CORR data=Assign1_WIN SPEARMAN;
# VAR FUT_ROA ROA deltaROA deltaTURN deltaMARGIN;
# RUN;
#
#
# *Let's also look at the distributions of our winsorized data;
# PROC UNIVARIATE data=Assign1_Win;
# 	VAR ROA FUT_ROA deltaROA; RUN;
#
#
# /*Let's also review the revised graphics - NOT IN THE CODE FILE I SENT LAST EVENING*/
#   PROC UNIVARIATE data=Assign1_WIN;
# VAR ROA;
# histogram ROA; RUN;
#
# PROC SORT data=Assign1_WIN; BY YEAR; RUN;
# PROC BOXPLOT data=Assign1_WIN;
# PLOT ROA*YEAR;
# RUN;
#
#
#
#
# /*So now we're much happier with our data.  We'd like to tabulate some descriptives.  How might we do that?*/
#   /*This is best achieved in SAS using PROC UNIVARIATE.  This is a very powerful procedure.  We do not
# have time to cover all of its uses, but I encourage you to explore it further.*/
#
#   /*Note, that instead of printing the results (as before), we are specifying the statistics that we'd like to use and
# 	saving them to a new dataset. Note also that I'm using the winsorized dataset*/
#
#   PROC UNIVARIATE data=Assign1_WIN noprint;
# VAR FUT_ROA ROA deltaROA deltaTURN deltaMARGIN;
# OUTPUT OUT=stats
# MEAN=avgFUT_ROA avgROA avgdeltaROA avgdeltaTURN avgdeltaMARGIN
# STD=stdFUT_ROA stdROA stddeltaROA stddeltaTURN stddeltaMARGIN
# MEDIAN=medFUT_ROA medROA meddeltaROA meddeltaTURN meddeltaMARGIN
# Q1=q1FUT_ROA q1ROA q1deltaROA q1deltaTURN q1deltaMARGIN
# Q3=q3FUT_ROA q3ROA q3deltaROA q3deltaTURN q3deltaMARGIN;
# RUN;
#
#
# /*Sometimes we want to do pre- post- analyses. How about comparing 2000-2002 to 2003-2005?*/
#   Data PrePost;
# SET Assign1_WIN;
# IF YEAR<=2005;
# PRE=(YEAR<2003);
# RUN;
#
# PROC FREQ; TABLES PRE; RUN;
#
# *T-test;
# PROC TTEST data=PREPOST;
# VAR ROA;
# CLASS PRE;
# RUN;
#
#
# *Non-parametric Wilcoxon-Mann-Whitney test;
# PROC NPAR1WAY data=PREPOST wilcoxon;
# VAR ROA;
# CLASS PRE;
# RUN;
#
#
# /******************************************Regressions***********************************************/
#
#   /*Alright, now I'm ready for regressions. How do I do a standard OLS in SAS*/
# PROC REG data=MyLib.Assign1;
# MODEL FUT_ROA = ROA deltaROA; quit;
#
# /*Ooops, I did it on the non-winsorized data. Let's re-run on the winsorized data and compare the output*/
#   PROC REG data=Assign1_WIN;
# MODEL FUT_ROA = ROA deltaROA; quit;
#
# PROC REG data=Assign1_WIN;
# MODEL FUT_ROA = ROA deltaTURN deltaMARGIN; quit;
#
#
# /*There are plenty of econometric issues here (primarily w/the standard errors).
# As this isn't an econometic class, I'll leave it at that for now....
# What I will show you, though, is some alternative regression specifications.*/
#
#   /*One problem is violation of the heteroscedasticity assumption*/
#   /*How do I correct for the problem (i.e., how do I use White standard errors, aka Robust standard errors)?*/
#   PROC REG data=Assign1_WIN;
# MODEL FUT_ROA = ROA deltaROA / HCC; quit;
#
#
# /*What if I want to see how the relationship varies over time? Or, alternatively, I want a preliminary look at the data without
# multiple observations per firm? How about we run annual regressions?*/
#
#   PROC SORT data=Assign1_WIN;
# BY YEAR GVKEY; RUN;
#
# PROC REG data=Assign1_WIN;
# MODEL FUT_ROA = ROA deltaROA;
# BY YEAR; quit;
#
# /*Note, you can do most data analysis using a by statement, but the data needs to be sorted according
# to the 'BY' variable*/
#
#   /*The Fama-MacBeth Regression methodology just builds on this idea*/
#   /*Just as a FYI: Fama-MacBeth regressions correct only for cross-sectional correlation. The standard errors from this method do
# not correct for time-series autocorrelation. Thus, this method is quite effective for stock trading, since stocks have
# weak time-series autocorrelation in daily and weekly series*/
#
#   PROC SORT data=Assign1_WIN;
# BY YEAR GVKEY;
# RUN;
#
# /*This section of code runs annual regressions and saves the parameter estimates to a dataset named 'pe'.
# Then it looks at the means of the time-series of parameter estimates*/
#   ods listing close;
# ods output parameterestimates=pe;
# proc reg data=Assign1_WIN;
# by year;
# model FUT_ROA = ROA deltaROA; run;
# quit;
# ods listing;
#
# proc means data=pe mean std t probt;
# var estimate; class variable;
# run;
#
# /*Newey West Adjustment for Standard Errors in Fama MacBeth (i.e., use after the above code is run)*/
#   *See Noah's webpage for details on this methodology;
# proc sort data=pe;
# 	by variable; run;
#
# %let lags=3;
# ods output parameterestimates=nw;
# ods listing close;
# proc model data=pe;
#  by variable;
#  instruments / intonly;
#  estimate=a;
#  fit estimate / gmm kernel=(bart,%eval(&lags+1),0) vardef=n; run;
# quit;
# ods listing;
#
# proc print data=nw; id variable;
#  var estimate--df; format estimate stderr 7.4;
# run;
#
# /*Note that the coefficient estimates from our base regression are similar, but the t-statistics have changed dramatically*/
#
# /*Another option is to cluster our standard errors by firm*/
# PROC SURVEYREG data=Assign1_WIN;
# 	CLUSTER GVKEY;
# 	MODEL FUT_ROA = ROA deltaROA; RUN;
# quit;
#
# /*We often two-way cluster (i.e., by firm and year) if we have a long enough time series.*/
# /*See 'https://webspace.utexas.edu/johnmac/www/cluster%20code.htm' for example code on two-way clustering in sas*/
#
#
# /*Now that we've briefly discussed some of the common standard error problems, we should discuss fixed effects regressions.
# Essentially, a fixed effect regression can be viewed as de-meaning, or inserting a series of dummy variables*/
#
#   /*For example, based on our earlier results, we may want to include yearly fixed effects within our analyses*/
#   /*We can do this by inserting a number of dummy variables (i.e., the long way)*/
#   DATA Assign1_WIN2;
# SET Assign1_WIN;
# yr2000=(year=2000);
# yr2001=(year=2001);
# yr2002=(year=2002);
# yr2003=(year=2003);
# yr2004=(year=2004);
# yr2005=(year=2005);
# yr2006=(year=2006);
# yr2007=(year=2007);
# yr2008=(year=2008);
# yr2009=(year=2009);
# yr2010=(year=2010);
# RUN;
#
# PROC REG data=Assign1_WIN2;
# MODEL FUT_ROA = ROA deltaROA yr2000 yr2001 yr2002 yr2003 yr2004 yr2005 yr2006 yr2007 yr2008 yr2009 / HCC; run;
# quit; /*Notice that I left yr2010 out of the regression*/
#
#   /*Shorter Option*/
#   PROC GLM data=Assign1_WIN;
# CLASS YEAR;
# MODEL FUT_ROA = ROA deltaROA YEAR/ solution; run;
# quit;
#
# /*Alternatively, you can suppress the fixed effects*/
#   PROC GLM data=Assign1_WIN;
# absorb year;
# MODEL FUT_ROA = ROA deltaROA / solution noint; RUN;
# quit;
#
# /*Suppressing the fixed effects is extremely useful when looking at something like a firm-fixed-effect regression*/
#   PROC GLM data=Assign1_WIN;
# absorb gvkey;
# MODEL FUT_ROA = ROA deltaROA / solution noint; run; /*the option suppresses the intercept to avoid dummy var trap*/
#   quit;
#
#
# /**************************************Logistic Regression********************************************************/
#   *Another Common technique is a logistic regression. What if instead of forecasting the value of ROA,
# we actually want to predict whether ROA will increase or decrease.  We can use logit for this purpose.;
#
# DATA MeetROA; SET Assign1_WIN;
# IF FUT_ROA>=ROA THEN MEET=1;
# ELSE MEET=0;
# RUN;
#
# PROC LOGISTIC data=MeetROA;
# MODEL MEET (DESCENDING) = ROA deltaROA; quit; /*DESCENDING is necessary to model prob that MEET = 1*/
#
#   PROC LOGISTIC data=MeetROA;
# MODEL MEET (DESCENDING) = ROA deltaTURN deltaMARGIN; quit;
#
#
# /*********************************Looking at the data in Stata****************************************/
#   PROC EXPORT DATA= WORK.Assign1_WIN
# OUTFILE= "H:\SAS Training 2014\assign1.dta"
# DBMS=STATA REPLACE;
# RUN;
