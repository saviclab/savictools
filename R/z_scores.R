
#TODO re-write clunky WHO z-score code
#TODO include data by using usethis::use_data(data, internal = TRUE)


library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
#CALCULATE WHO Z-scores

#First create a data set with only subset of data


data <- read_csv("~/Desktop/rada/HIV_project/DTG/kids/dtg-kids-vomit.csv")
data$rownum <- 1:nrow(data)

# 5 and below for igrowup
data.5below <- data %>% filter(AGE <= 60) %>% select(ID, rownum, AGE, SEX, WT, HT)
data.5below <- as.data.frame(data.5below)


# above 5 for WHO2007
data.5above <- data %>% filter(AGE > 60) %>% select(ID, rownum, AGE, SEX, WT, HT)
data.5above<- as.data.frame(data.5above)





##################---------- UNDER 5 YEARS -----------##################

#LOAD WHO DATA SETS
#FOR 5 below: IGROWUP


dir <- "~/Desktop/rada/WHO_FILES" ##NOTE, must define "dir" = directory

source(paste(dir,"/igrowup_R/igrowup_standard.r", sep=""))
source(paste(dir,"/igrowup_R/igrowup_restricted.r",sep=""))

weianthro<-read.table(paste(dir,"/igrowup_R/weianthro.txt",sep=""),header=T,sep="",skip=0)
lenanthro<-read.table(paste(dir,"/igrowup_R/lenanthro.txt",sep=""),header=T,sep="",skip=0)
bmianthro<-read.table(paste(dir,"/igrowup_R/bmianthro.txt",sep=""),header=T,sep="",skip=0)
hcanthro<-read.table(paste(dir,"/igrowup_R/hcanthro.txt",sep=""), header=T,sep="",skip=0)
acanthro<-read.table(paste(dir,"/igrowup_R/acanthro.txt",sep=""),header=T,sep="",skip=0)
ssanthro<-read.table(paste(dir,"/igrowup_R/ssanthro.txt",sep=""),header=T,sep="",skip=0)
tsanthro<-read.table(paste(dir,"/igrowup_R/tsanthro.txt",sep=""),header=T,sep="",skip=0)
wflanthro<-read.table(paste(dir,"/igrowup_R/wflanthro.txt",sep=""),header=T,sep="",skip=0)
wfhanthro<-read.table(paste(dir,"/igrowup_R/wfhanthro.txt",sep=""),header=T,sep="",skip=0)

#calculate Z-scores
igrowup.standard(FilePath=paste(dir, "/igrowup_R/", sep=''),
                 FileLab="MyZscore",
                 mydf= data.5below,
                 sex=SEX,
                 age=AGE,
                 age.month=T,
                 weight=WT,
                 lenhei=HT)

#Select variables wanted
#cbmi = BMI
#zwei = WAZ
#zlen = HAZ
#zwfl = WHZ
#zbmi = BAZ
#fwei = WAZ out of range flag
#flen = HAZ out of range flag
#fwfl = WHZ out of range flag
#fbmi = BAZ out of range flag

#select and rename columns
zvars <- matz[,c('ID', "rownum", 'cbmi','zwei','zlen', 'zbmi', 'zwfl', 'fwei', 'flen', 'fwfl', 'fbmi')]
zvars <- rename(zvars, BMI = cbmi,
                WAZ = zwei,
                HAZ = zlen,
                WHZ = zwfl,
                BAZ = zbmi,
                WAZ_F = fwei, #WAZ out of range flag
                HAZ_F = flen, #HAZ out of range flag
                WHZ_F = fwfl, #WHZ out of range flag
                BAZ_F = fbmi) #BAZ out of range flag

summary(zvars)

#remove any objects that may interfere with WHO2007 run
rm(list=c('acanthro', 'bmianthro', 'calc.zac', 'calc.zbmi', 'calc.zhc',
          'calc.zlen', 'calc.zss', 'calc.zts', 'calc.zwei', 'calc.zwfl',
          'hcanthro', 'igrowup.standard', 'lenanthro', 'matprev', 'matz',
          'prevnh', 'prevnh.L', 'prevph', 'prevph.L', 'rounde', 'ssanthro',
          'tsanthro', 'weianthro', 'wfhanthro', 'wflanthro', 'wmean', 'wsd'))






##################---------- OVER 5 YEARS -----------##################
#LOAD WHO DATA SETS
#FOR 5 above: WHO2007
#only contains BAZ, WAZ, HAZ

source(paste(dir, '/who2007_R/who2007.r', sep='')) # same directory as before

wfawho2007 <- read.table(paste(dir,"/who2007_R/wfawho2007.txt",sep=''), header = TRUE, sep="", skip = 0)
hfawho2007 <- read.table(paste(dir,"/who2007_R/hfawho2007.txt",sep=''), header = TRUE, sep="", skip = 0)
bfawho2007 <- read.table(paste(dir,"/who2007_R/bfawho2007.txt",sep=''), header = TRUE, sep="", skip = 0)

#Calculate Z-scores
# age must be months for this function
who2007(FileLab = 'MyZscore',
        FilePath = paste(dir,'/who2007_R/',sep=''),
        mydf = data.5above,
        sex = SEX,
        age = AGE,
        weight = WT,
        height = HT)

#Select variables wanted
#cbmi calculated BMI=weight / squared (height)
#zwfa = Weight-for-age z-score
#zhfa = Height-for-age z-score
#zbfa = BMI-for-age z-score
#fwfa = Flag for zwfa<-6 or zwfa>5
#fhfa = Flag for zhfa<-6 or zhfa>6
#fbfa = Flag for zbfa<-5 or zbfa>5

zvars1 <- matz[,c('ID', "rownum", 'cbmi','zwfa', 'zhfa', 'zbfa', 'fwfa', 'fhfa', 'fbfa')]
zvars1 <- rename(zvars1, BMI = cbmi,
WAZ = zwfa,
HAZ = zhfa,
BAZ = zbfa,
WAZ_F = fwfa, #WAZ out of range flag
HAZ_F = fhfa, #HAZ out of range flag
BAZ_F = fbfa) #BAZ out of range flag

summary(zvars1) #No NAs

#remove any objects that may interfere with future runs
rm(list=c('bfawho2007', 'calc.zbmi', 'calc.zhfa', 'calc.zwei',
'hfawho2007', 'matprev', 'matz', 'prevnh', 'prevnh.L',
'prevph', 'prevph.L', 'rounde', 'wfawho2007', 'who2007',
'wmean', 'wsd'))


#To merge data sets, first merge z-scores together
zvars_full <- bind_rows(zvars, zvars1)
summary(zvars_full)


result <- left_join(data, zvars_full, by="rownum") %>%
        rename(ID = ID.x) %>% select(-ID.y, -rownum)


# deal with missing and unlikely values



result99 <- result %>% replace_na(list(WAZ=-99, HAZ=-99, BAZ=-99, WHZ=-99,
                                       WAZ_F=0, HAZ_F=0, BAZ_F=0, WHZ_F=0))
result98 <- result99 %>% mutate(WAZ = ifelse(WAZ_F == 1, -98, WAZ),
                                HAZ = ifelse(HAZ_F == 1, -98, HAZ),
                                BAZ = ifelse(BAZ_F == 1, -98, BAZ),
                                WHZ = ifelse(WHZ_F == 1, -98, WHZ)) %>%
        select(-WAZ_F, -HAZ_F, -BAZ_F, -WHZ_F)


write_csv(result98, "~/Desktop/rada/HIV_project/DTG/kids/dtg-kids-zscores.csv")

ggplot(mutate(result98, grp=cut(WAZ, c(-6, -3, -2, -1, 0, 6))),
       aes(x = AGE, y = WT, color = grp, group=ID)) +
        geom_point() +
        geom_line() +
        labs(color = "WAZ score")
+
        theme(legend.position="none")
