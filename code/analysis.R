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

# roster has duplicate rows for each case in act/resp
# want number of kids and their ages
table(rost$TERRP)
# TERRP: 22 = "Own household child"
rost <- subset(rost, TERRP==22)
summary(rost$TEAGE)
# let's subset to those with at least 1 little kid
sum(rost$TEAGE<=5)
ids <- unique(rost$TUCASEID[rost$TEAGE<=5])

dat <- merge(resp, act, by="TUCASEID")
dat <- subset(dat, TUCASEID %in% ids)

##############################################################################

