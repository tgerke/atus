##############################################################################
# set options, working directory, load libraries

setwd("~/Documents/ATUS")

options(stringsAsFactors=FALSE)

library(ggplot2)
library(gridExtra)
library(reshape2)
library(scales)
# to show ggplot colors: show_col(hue_pal()(4))

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

# subset to mothers
table(dat$TESEX) #1 = "Male", 2 = "Female"
dat <- subset(dat, TESEX == 2)

# working with 1108 moms
nrow(dat)

# clean up
rm(list = setdiff(ls(), c("dat")))

##############################################################################
# grab some interesting time-use variables

# TELFS = "labor force status";
# 1 = "Employed - at work"
# 2 = "Employed - absent"
# 3 = "Unemployed - on layoff"
# 4 = "Unemployed - looking"
# 5 = "Not in labor force"
table(dat$TELFS.x) 

# TEHRUSLT = "total hours usually worked per week";
summary(dat$TEHRUSLT.x)
sum(dat$TEHRUSLT.x<10)

# t010101 = "Sleeping";
summary(dat$t010101)
summary(dat$t010101)/60

# t010201 = "Washing, dressing and grooming oneself";
summary(dat$t010201)

# t010401 = "Personal/Private activities";
summary(dat$t010401)


# t020101 = "Interior cleaning";
summary(dat$t020101)

# t020102 = "Laundry";
summary(dat$t020102)

# t020201 = "Food and drink preparation";
# t020202 = "Food presentation";
# t020203 = "Kitchen and food clean-up";
dat$MealPrep <- dat$t020201 + dat$t020202 + dat$t020203
summary(dat$MealPrep)
summary(dat$t150201)

# t020901 = "Financial management";
summary(dat$t020901)

# t030101 = "Physical care for hh children";
# t030102 = "Reading to/with hh children";
# t030103 = "Playing with hh children, not sports";
# t030104 = "Arts and crafts with hh children";
# t030105 = "Playing sports with hh children";
# t030106 = "Talking with/listening to hh children";
# t030108 = "Organization and planning for hh children";
# t030109 = "Looking after hh children (as a primary activity)";
# t030110 = "Attending hh children's events";
# t030111 = "Waiting for/with hh children";
# t030112 = "Picking up/dropping off hh children";
# t030201 = "Homework (hh children)";
# t030202 = "Meetings and school conferences (hh children)";
# t030203 = "Home schooling of hh children";
# t030204 = "Waiting associated with hh children's education";
# t030301 = "Providing medical care to hh children";
# t030302 = "Obtaining medical care for hh children";
# t030303 = "Waiting associated with hh children's health";

dat$PhysicalCare <- dat$t030101
dat$Reading <- dat$t030102
dat$Playing <- dat$t030103 + dat$t030105
dat$ArtsCrafts <- dat$t030104
dat$TalkingListening <- dat$t030106
dat$MonitoringPlay <- dat$t030109
dat$AllCare <- dat$t030101 + dat$t030102 + dat$t030103 + dat$t030105 + 
  dat$t030104 + dat$t030106 + dat$t030109

# t070101 = "Grocery shopping";
# t070102 = "Purchasing gas";
# t070103 = "Purchasing food (not groceries)";
# t070104 = "Shopping, except groceries, food and gas";
# t070105 = "Waiting associated with shopping";
# t070201 = "Comparison shopping";
dat$GroceryShop <- dat$t070101
# 
# t080501 = "Using personal care services";
# 
# t110101 = "Eating and drinking";
# t110201 = "Waiting associated w/eating and drinking";
# 
# t120101 = "Socializing and communicating with others";
# t120501 = "Waiting assoc. w/socializing and communicating";
# t120201 = "Attending or hosting parties/receptions/ceremonies";
# t120502 = "Waiting assoc. w/attending/hosting social events";
# t120202 = "Attending meetings for personal interest (not volunteering)";
dat$SocialActivities <- dat$t120101 + dat$t120501 + dat$t120201 + dat$t120502 + dat$t120202

# t120301 = "Relaxing, thinking";
dat$Relaxing <- dat$t120301
# t120302 = "Tobacco and drug use";
# t120303 = "Television and movies (not religious)";
# t120304 = "Television (religious)";
# t120305 = "Listening to the radio";
# t120306 = "Listening to/playing music (not radio)";
# t120307 = "Playing games";
# t120308 = "Computer use for leisure (exc. Games)";
# t120309 = "Arts and crafts as a hobby";
# t120310 = "Collecting as a hobby";
# t120311 = "Hobbies, except arts and crafts and collecting";
# t120312 = "Reading for personal interest";
# t120313 = "Writing for personal interest";
# t120401 = "Attending performing arts";
# t120402 = "Attending museums";
# t120403 = "Attending movies/film";
# t120404 = "Attending gambling establishments";
# t120503 = "Waiting associated with relaxing/leisure";
# t120504 = "Waiting associated with arts and entertainment";
# t130101 = "Doing aerobics";
# t130102 = "Playing baseball";
# t130103 = "Playing basketball";
# t130104 = "Biking";
# t130105 = "Playing billiards";
# t130106 = "Boating";
# t130107 = "Bowling";
# t130108 = "Climbing, spelunking, caving";
# t130109 = "Dancing";
# t130110 = "Participating in equestrian sports";
# t130112 = "Fishing";
# t130113 = "Playing football";
# t130114 = "Golfing";
# t130115 = "Doing gymnastics";
# t130116 = "Hiking";
# t130117 = "Playing hockey";
# t130118 = "Hunting";
# t130119 = "Participating in martial arts";
# t130120 = "Playing racquet sports";
# t130122 = "Rollerblading";
# t130124 = "Running";
# t130125 = "Skiing, ice skating, snowboarding";
# t130126 = "Playing soccer";
# t130127 = "Softball";
# t130128 = "Using cardiovascular equipment";
# t130129 = "Vehicle touring/racing";
# t130130 = "Playing volleyball";
# t130131 = "Walking";
# t130132 = "Participating in water sports";
# t130133 = "Weightlifting/strength training";
# t130134 = "Working out, unspecified";
# t130135 = "Wrestling";
# t130136 = "Doing yoga";
# t130202 = "Watching baseball";
# t130203 = "Watching basketball";
# t130207 = "Watching bowling";
# t130210 = "Watching equestrian sports";
# t130212 = "Watching fishing";
# t130213 = "Watching football";
# t130216 = "Watching hockey";
# t130218 = "Watching racquet sports";
# t130219 = "Watching rodeo competitions";
# t130220 = "Watching rollerblading";
# t130222 = "Watching running";
# t130223 = "Watching skiing, ice skating, snowboarding";
# t130224 = "Watching soccer";
# t130225 = "Watching softball";
# t130226 = "Watching vehicle touring/racing";
# t130227 = "Watching volleyball";
# t130229 = "Watching water sports";
# t130232 = "Watching wrestling";
# t130301 = "Waiting related to playing sports or exercising";
# t130302 = "Waiting related to attending sporting events";
# t140101 = "Attending religious services";
# t140102 = "Participation in religious practices";
# t140103 = "Waiting associated w/religious and spiritual activities";
# t140105 = "Religious education activities";
recVars <- c(paste("t12030", 2:9, sep=""), paste("t12031", 0:3, sep=""),
             paste("t12040", 1:4, sep=""), "t120503", "t120504",
             paste("t13010", 1:9, sep=""), paste("t13011", c(0,2:9), sep=""),
             paste("t13012", c(0,2,4:9), sep=""), paste("t13013", 0:6, sep=""),
             paste("t13020", c(2,3,7), sep=""), paste("t13021", c(0,2,3,6,8,9), sep=""),
             paste("t13022", c(0,2:7,9), sep=""), "t130232", "t130301", "t130302",
             paste("t14010", c(1,2,3,5), sep=""))
dat$OtherRecreation <- apply(dat[,recVars], 1, sum)

# t150101 = "Computer use";
# t150102 = "Organizing and preparing";
# t150103 = "Reading";
# t150104 = "Telephone calls";
# t150105 = "Writing";


##############################################################################
# summarize and visualize

# define some colors
pointcol <- hue_pal()(4)[3]
Kcol <- hue_pal()(1)

### Needs

# Sleep (analyze separately because of scale)
dat$Sleep <- dat$t010101
sleep <- ggplot(dat, aes(x=0, y=Sleep/60)) + 
  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0), col=pointcol, shape=1) + 
  geom_point(aes(y=7) , shape=17, size=4, col=Kcol) +
  labs(x=" ", y = "Hours sleep per day") + 
  #scale_x_discrete(name="", labels=c("Label 1", "Label 2")) +
  guides(colour=FALSE) + 
  theme_classic()

# Other needs
dat$WashDressGroom <- dat$t010201
dat$PrivateActivities <- dat$t010401
dat$EatingDrinking <- dat$t110101 + dat$t110201
dat$Needs <- dat$WashDressGroom + dat$PrivateActivities + dat$EatingDrinking

dat.m <- melt(dat, id.vars = "TUCASEID", 
              measure.vars=c("WashDressGroom", "PrivateActivities",
                             "EatingDrinking", "Needs"))
dat.K <- data.frame(variable=c("WashDressGroom", "PrivateActivities",
                               "EatingDrinking", "Needs"),
                    value=c(7, 0, 20, 27))
otherNeeds <- ggplot(dat.m, aes(x=variable, y=value, col=pointcol)) + 
  geom_boxplot(outlier.shape=NA, col="black") + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0), shape=1, col=pointcol) +
  geom_point(data=dat.K, shape=17, size=4, col=Kcol) +
  labs(x=" ", y = "Minutes per day") + 
  #scale_x_discrete(name="", labels=c("Label 1", "Label 2")) +
  guides(colour=FALSE) + 
  theme_classic()

pdf("atus/figures/Needs.pdf", width=16, height=8)
grid.arrange(sleep, otherNeeds, ncol=2)
dev.off()

### Cleaning, shopping, and cooking (dat$MealPrep)
dat$Cleaning <- dat$t020101
dat$Laundry <- dat$t020102
dat$CleaningCooking <- dat$MealPrep + dat$GroceryShop + dat$Cleaning + dat$Laundry
dat.m <- melt(dat, id.vars = "TUCASEID", 
              measure.vars=c("MealPrep", "GroceryShop", "Cleaning",
                             "Laundry", "CleaningCooking"))
dat.K <- data.frame(variable=c("MealPrep", "GroceryShop", "Cleaning",
                               "Laundry", "CleaningCooking"),
                    value=c(60, 30, 45, 15, 60+45+15))
cleancook <- ggplot(dat.m, aes(x=variable, y=value, col=pointcol)) + 
  geom_boxplot(outlier.shape=NA, col="black") + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0), shape=1, col=pointcol) +
  geom_point(data=dat.K, shape=17, size=4, col=Kcol) +
  labs(x=" ", y = "Minutes per day") + 
  #scale_x_discrete(name="", labels=c("Label 1", "Label 2")) +
  guides(colour=FALSE) + 
  theme_classic()
pdf("atus/figures/CleanCook.pdf", width=8, height=8)
cleancook
dev.off()

# Activities with children
dat.m <- melt(dat, id.vars = "TUCASEID", 
              measure.vars=c("PhysicalCare", "Reading",
                             "Playing", "ArtsCrafts", 
                             "TalkingListening", "MonitoringPlay",
                             "AllCare"))
dat.K <- data.frame(variable=c("PhysicalCare", "Reading",
                               "Playing", "ArtsCrafts", 
                               "TalkingListening", "MonitoringPlay",
                               "AllCare"),
                    value=c(60, 30, 120, 45, 60, 60*3, 
                            60+ 30+ 120+ 45+ 60+ 60*3))
activities <- ggplot(dat.m, aes(x=variable, y=value, col=pointcol)) + 
  geom_boxplot(outlier.shape=NA, col="black") + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0), shape=1, col=pointcol) +
  geom_point(data=dat.K, shape=17, size=4, col=Kcol) +
  labs(x=" ", y = "Minutes per day") + 
  #scale_x_discrete(name="", labels=c("Label 1", "Label 2")) +
  guides(colour=FALSE) + 
  theme_classic()
pdf("atus/figures/ActivitiesWithChildren.pdf", width=8, height=8)
activities
dev.off()

# Personal Recreation
dat$AllRecreation <- dat$SocialActivities + dat$Relaxing + dat$OtherRecreation
dat.m <- melt(dat, id.vars = "TUCASEID", 
              measure.vars=c("SocialActivities", "Relaxing",
                             "OtherRecreation", "AllRecreation"))
dat.K <- data.frame(variable=c(c("SocialActivities", "Relaxing",
                                 "OtherRecreation", "AllRecreation")),
                    value=c(5, 15, 45,
                            5+ 15+ 45))
recreation <- ggplot(dat.m, aes(x=variable, y=value, col=pointcol)) + 
  geom_boxplot(outlier.shape=NA, col="black") + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0), shape=1, col=pointcol) +
  geom_point(data=dat.K, shape=17, size=4, col=Kcol) +
  labs(x=" ", y = "Minutes per day") + 
  #scale_x_discrete(name="", labels=c("Label 1", "Label 2")) +
  guides(colour=FALSE) + 
  theme_classic()
pdf("atus/figures/PersonalRecreation.pdf", width=8, height=8)
recreation
dev.off()
