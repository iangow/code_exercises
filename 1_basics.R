# This module is going to be based around some data that I pulled for
# IBM and Apple
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(stringr)
library(lubridate)

fix_names <- function(df) {
  names(df) <- tolower(names(df))
  df
}

# Now let's import the files (which are saved as .csv files)
# TRAIN1 is some data from Compustat (plus a PERMNO link that I manually added).
# TRAIN2 is some data from CRSP
#
# SAS has just two data types: floating-point numberic and fixed-width characters
# R has a much richer set of data types. A cost is that one needs to be a bit more
# precise when importing data from (say) CSV files. When using cleaned data, the
# data-types will be pre-specified.
#
train1 <-
  read_csv("data/train1.csv",
           col_types = "iDiciddddddi",
           locale = locale(date_format = "%Y%M%d")) %>%
  fix_names()

train2 <-
  read_csv("data/train2.csv", col_types = "iiDddddd",
           na="C",
           locale = locale(date_format = "%Y%M%d")) %>%
  fix_names()

train1
train2

# For most things in SAS, there are multiple ways of doing things.
# We are going to start by introducing a few basic
#	things in the common SAS language -- we are going to move through this quickly,
# however, since I tend to focus on doing things using PROC SQL.
# This will be our next topic.;

# IDG: There's no real need to learn SQL separately with dplyr.
#      Instead dplyr uses a slightly modified version of SQL (in effect).
#      The ideas are the same. If the data source is an SQL source, then
#      dplyr will translate the dplyr code into SQL behind the scenes.

# Let's assume for my study that I'm interested in Gross Margin by year by company.
#	Second, I want an ID number based on calendar time.
# Third, I want a founder field that says 'Jobs' for Apple and 'Flint' for IBM.
#	Finally, I want separate data sets for each company.
# How might I accomplish this?*/

# Step 1 - Create initial dataset;
gm <-
  train1 %>%
  select(gvkey, datadate, fyear, fyr, conm, sale, cogs) %>%
  mutate(margin = 100 * (sale - cogs)/sale) # calculate Gross Margin

# Step 2 - Sort data and add ID;
gm2 <-
  gm %>%
  arrange(datadate) %>%
  mutate(id = row_number())

# Step 3 - Add Founder Field (Note: this could've been incorporated above, but I separated to show IF/THEN statements);
gm3 <-
  gm2 %>%
  mutate(founder = case_when(gvkey == 6066 ~ 'FLINT',
                             gvkey == 1690 ~ 'JOBS'))

# Step 4 - Create an Apple dataset and an IBM dataset;
apple <- gm3 %>% filter(gvkey != 6066)
ibm <- gm3 %>% filter(gvkey == 6066)

# Compustat normally reports GVKEYs as characters with 6 characters, but ours are numeric.  How can we adjust?;
gm4 <-
  gm3 %>%
  mutate(gvkey2 = as.character(gvkey), # This makes it a character
         # This adds leading zeroes
         gvkey3 = str_pad(gvkey2, width = 6, pad = "0"))

# Next in our assignment, we want to compare the GM from last year to this year. How should we do this?*/
# A useful function is the lag() or lagX function.
# HOWEVER, IN SAS this will not distinguish between companies.  For example-;
gm5 <-
  gm4 %>%
  arrange(gvkey, datadate) %>% # to sort by company then date
  mutate(lagmargin = lag(margin),
         lag5margin = lag(margin, 5))

#  SEE LINE 34 OF THE TABLE - NOTICE HOW THE FIRST LAGGED VALUE FOR
# IBM IS THE LAST OBSERVATION FOR APPLE.
# Thererfore, this can't be the best solution.
# There is a way to make this work.  BUT, I'm going to show you
# a more efficient alternative;

gm5 %>%
  slice(27:36) %>%
  select(gvkey, datadate, margin, lagmargin, lag5margin)

# First a small aside - dates can be a bit quirky. SO, to align dates and ensure comparability, we often take extra steps.
#  INTNX is a function that increments dates by intervals.  MDY() allows you to specify dates.
#  Below I demonstrate how these are helpful.;

#  *Further, PROC EXPAND only works if there is one observation per time point per subject. This sets us up for that;
# /*Note this overwrites our GM 5 from above, which we learned was incorrect*/
gm5 <-
  gm4 %>%
  mutate(fdate = ymd(paste(fyear, "12-31", sep="-")))

# Ensure that I don't have any duplicate entries based on following keys*/
gm5 %>%
  group_by(gvkey, fdate) %>%
  filter(n() > 1) %>%
  select(gvkey, fdate, everything()) %>%
  ungroup() %>%
  count() %>%
  pull() == 0L

# Now we can use PROC EXPAND to gather our lagged variable*/
gm7 <-
  gm5 %>%
  group_by(gvkey) %>%
  arrange(fdate) %>%
  mutate(chgmargin = margin - lag(margin)) %>%
  ungroup() %>%
  arrange(gvkey, datadate)

gm7 %>%
  slice(27:42) %>%
  select(gvkey, datadate, margin, chgmargin)

gm8 <-
  gm7 %>%
  filter(!is.na(chgmargin)) # Remove entries with missing data

# Great we've completed our analysis.
# This is our end file.  We should save it, so we can look at it later.
margins <- gm8

# Here's the above in one go, but slightly streamlined:
# (I simply converted GVKEY to the right format rather than keeping
#  alternative versions of it.)
margins <-
  train1 %>%
  select(gvkey, datadate, fyear, fyr, conm, sale, cogs) %>%
  mutate(margin = 100 * (sale - cogs)/sale) %>%
  arrange(datadate) %>%
  mutate(id = row_number(),
         gvkey = str_pad(gvkey, width = 6, pad = "0"),
         founder = case_when(gvkey == "006066" ~ 'FLINT',
                             gvkey == "001690" ~ 'JOBS'),
         fdate = ymd(paste(fyear, "12-31", sep="-"))) %>%
  group_by(gvkey) %>%
  arrange(fdate) %>%
  mutate(chgmargin = margin - lag(margin)) %>%
  ungroup() %>%
  arrange(gvkey, datadate)

margins
save(margins, file = "margins.Rdata")
