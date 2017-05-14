##############################################################################
# set options, working directory, load libraries

setwd("~/Documents/ATUS")

options(stringsAsFactors=FALSE)

library(ggplot2)

##############################################################################
# load data
# downloaded 2017-05-14 from https://www.bls.gov/tus/datafiles_2015.htm

# respondent file
resp <- read.table("atusresp_2015/atusresp_2015.dat", sep=",", header=TRUE)

# roster file
rost <- read.table("atusrost_2015/atusrost_2015.dat", sep=",", header=TRUE)

# activity summary file
act <- read.table("atussum_2015/atussum_2015.dat", sep=",", header=TRUE)

##############################################################################
# wrangle and merge

# TRYHHCHILD = "Age of youngest household child < 18";
table(act$TRYHHCHILD)
sum(act$TRYHHCHILD %in% 0:5)

dat <- merge(resp, act, by="TUCASEID")
dat <- subset(dat, TRYHHCHILD.x %in% 0:5)

# clean up
rm(list = setdiff(ls(), c("dat")))

##############################################################################


