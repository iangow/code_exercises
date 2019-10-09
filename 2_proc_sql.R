library(dplyr, warn.conflicts = FALSE)
library(lubridate)
# /*2_INTRODUCTION TO PROC SQL*/

#  libname MYLIB 'D:\Dropbox\Admin\SAS Training 2014';

# SQL is a very powerful and useful language. This procedure is extremely useful when interfacing with WRDS.;
# This class only allows us to cover a small portion of what SQL can do -- there's lots more out there!;

#*EXAMINING DATA USING SQL*/
# We can do all of the basic functions (i.e., subsets, calculations, etc.) in the basic SQL Formation;
# Let's create a gross margin table, similar to our earlier exercise;

train1 <- readRDS("data/train1.rds")
train2 <- readRDS("data/train2.rds")

gm <-
  train1 %>%
  mutate(margin = 100 * (sale - cogs)/sale) %>%
  select(gvkey, permno, datadate, fyear, margin) %>%
  arrange(datadate) %>%
  mutate(id = row_number())


# DATA GM; SET GM; ID=_N_; RUN;

# IDG: The following is not valid *proper* SQL. The SQL standard
#      requires that variables either be the result of an "aggregate"
#      function. In proper SQL you would achieve the resukt of the SAS code
#      as I have done it below:
#      - 1. Create summary statistics.
#      - 2. Merge summary statistics with original table.

# *Another Powerful Feature is to Create Summary Statistics (e.g., average GM per company);
# PROC SQL;
# create table meanGM
# as select a.*, mean(MARGIN) as avgMARGIN FORMAT 10.4
# from GM as a
# group by GVKEY
# order by gvkey, datadate; quit;
summary_gm <-
  gm %>%
  group_by(gvkey) %>%
  summarize(avg_margin = mean(margin, na.rm = TRUE))

mean_gm <-
  gm %>%
  inner_join(summary_gm, by = "gvkey")

# It's Also Useful to identify Distinct Records;
# PROC SQL;
#	create table Summary
#	as select distinct gvkey, avgMARGIN
# from meanGM; quit;

# *Subsets can also be created using SQL (e.g., only Apple);
# PROC SQL;
#	create table Apple
#	as select a.*
#	from meanGM as a
#	where GVKEY=1690; quit;
apple <-
  mean_gm %>%
  filter(gvkey == 1690L)


#/*JOINING DATA WITH SQL*/
#	*This is where the real power of SQL comes in for accounting researchers;

#*Let's say we need to compare this year's GM to last year's GM for Apple only
#(we will use the Apple and GM dataset to do this);

# IDG: I don't think this is a very good example. You wouldn't do lags this
#      way
lagged_margin <-
  gm %>%
  mutate(fyear = fyear + 1) %>%
  select(gvkey, fyear, lag_margin = margin)


apple_inner <-
  apple %>%
  inner_join(lagged_margin, by = c("gvkey", "fyear"))

#/*INNER JOINS keep only records where data comes from both datasets.*/
#  PROC SQL;
#create table AAPL_InnerJoin
# as select a.*, b.MARGIN as MARGIN1 'Lagged Margin', a.MARGIN-b.MARGIN as chgMARGIN FORMAT 10.4
#from Apple as a, GM as b
# where a.GVKEY=b.GVKEY and (a.FYEAR-1)=b.FYEAR; quit;


#/*OUTER JOINS take data from which ever data is specified as Master - in this case LEFT JOIN*/
#  PROC SQL;
#create table AAPL_OuterJoin
#as select a.*, b.MARGIN as MARGIN1 'Lagged Margin', a.MARGIN-b.MARGIN as chgMARGIN FORMAT 10.4
#from Apple as a left join GM as b
# on a.GVKEY=b.GVKEY and (a.FYEAR-1)=b.FYEAR; quit;

#*Note that the Inner Join has 32 observations, but the Outer Join has all 33 observations
# (chgMARGIN will be missing for one observation);
apple_outer <-
  apple %>%
  left_join(lagged_margin, by = c("gvkey", "fyear"))

apple_inner %>% count()
apple_outer %>% count()
# */LET'S PUT IT ALL TOGETHER*/

# Our task is to get the annual returns that correspond to the fiscal year dates in Train1;

# *One item to note: due to the way SAS operates, it is easy to sum,
# but difficult to multiply. Because of this, we often accumulate returns
# using logs.  You will see that in the following example.;

# Quick Aside -- Up to this point we haven't used the datadate variable. Now we plan to --
#  the problem, is that SAS imported it as a number.  How do we make it a date?;
# Best solution, I've found is convert to character and then convert back
# to number as a date;

# Data Train1a;
#	SET MyLib.Train1;
#	datadate2=input(put(datadate, 8.), yymmdd8.);
# FORMAT DATADATE2 DATE9.;
# RUN;

# Data Train2a;
#	SET MyLib.Train2;
#	date2=input(put(date, 8.), yymmdd8.);
#	FORMAT DATE2 DATE9.;
# RUN;


#	*Step 1: Add Begin/End Dates and ID# to Train1;
# PROC SORT data=Train1a;
# BY DATADATE; RUN;

# DATA Train1b; SET Train1a;
#	ID=_N_;
#	Retenddt=intnx('month',datadate2,0,'end');
#	Retbegdt=intnx('month',datadate2,-11,'beginning');
#	FORMAT retbegdt retenddt date9.;
# RUN;

eomonth <- function(date) {
    ceiling_date(date, "month") - days(1)
}

bomonth <- function(date) {
  floor_date(date, "month")
}

ret_dates <-
	  train1 %>%
    select(permno, datadate) %>%
	  arrange(datadate) %>%
	  mutate(id = row_number(),
	         ret_end_dt = eomonth(datadate),
	         ret_beg_dt = bomonth(datadate) - months(11))


returns <-
  ret_dates %>%
  inner_join(train2, by = "permno") %>%
  filter(date >= ret_beg_dt, date <= ret_end_dt) %>%
  group_by(permno, datadate) %>%
  summarize(n_mos = sum(!is.na(ret)),
            n_miss = sum(is.na(ret)),
            ret_cum = prod(1 + ret) - 1,
            ret_alt = exp(sum(log1p(ret)))-1)
#	*Step 2: Cumulate Returns;
#PROC SQL;
#	create table RETURNS
#	as select distinct a.*, sum(log(1+b.RET)) as LRET 'Compound Log Return', n(b.RET) as n_mos, nmiss(b.RET) as n_miss
#	from Train1b as a left join Train2a as b
#	on a.PERMNO=b.PERMNO and b.DATE2>=a.RETBEGDT and b.DATE2<=a.RETENDDT
#	group by a.id order by a.id; quit;

# Step 3: Let's say you only want complete years and don't like compound log returns
#			Plus, you only want non-missing Net Income - you can then use this below;
gm_subset <-
  train1 %>%
  inner_join(returns, by = c("datadate", "permno")) %>%
  filter(n_mos == 12, n_miss == 0, !is.na(oiadp)) %>%
  select(gvkey, permno, datadate, conm, oiadp, ret = ret_alt)

#PROC SQL;
#	create table SUBSET
#	as select GVKEY, PERMNO, datadate2 as datedate, CONM, OIADP, exp(LRET)-1 as RET 'Fyear Return' FORMAT 10.4
#	from RETURNS
#	where n_mos=12 and n_miss=0 and OIADP is not missing; quit;
