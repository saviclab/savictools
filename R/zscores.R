#' @title zscores
#' @author Alexander Floren, Dhruv Vaish
#'
#' @description Function wrapper for a script provided by the WHO to produce z-scores
#'
#' @param data A tibble of data
#' @param missing_flag OPTIONAL: value used to replace missing values
#' @param unlikely_flag OPTIONAL: value used to replace flagged z-scores
#'
#' @export

zscores <- function(data, missing_flag=-99, unlikely_flag=-98) {
  data <- read_csv("~/Documents/Berkeley/Savic/WHO_FILES/dtg-kids-vomit.csv")
  data$rownum <- 1:nrow(data)
  below_five <- data %>% dplyr::filter(AGE <= 60) %>% dplyr::select(ID, rownum, AGE, SEX, WT, HT)
  above_five <- data %>% dplyr::filter(AGE > 60) %>% dplyr::select(ID, rownum, AGE, SEX, WT, HT)

  # Data for children below 5 years
  # data.5below <- data %>% dplyr::filter(AGE <= 60) %>% dplyr::select(ID, rownum, AGE, SEX, WT, HT)
  # data.5below <- as.data.frame(data.5below)
  below_five_frame <- as.data.frame(below_five)
  above_five_frame <- as.data.frame(above_five)

  # Data for children above 5 years
  # data.5above <- data %>% dplyr::filter(AGE > 60) %>% dplyr::select(ID, rownum, AGE, SEX, WT, HT)
  # data.5above<- as.data.frame(data.5above)

  # Load WHO datasets

  # SCRIPT: Under 5 years
  source("R/helpers/igrowup_standard.r", local = TRUE)
  source("R/helpers/igrowup_restricted.r", local = TRUE)
  load("R/sysdata.rda")

  #calculate Z-scores
  igrowup.standard(FilePath=paste("~/Documents/Berkeley/Savic/test/outputs/", sep=''),
                   FileLab="MyZscore",
                   mydf=below_five_frame,
                   sex=SEX,
                   age=AGE,
                   age.month=T,
                   weight=WT,
                   lenhei=HT)

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

  # SCRIPT: Over 5 years old
  source("R/helpers/who2007.r", local = TRUE)

  #Calculate Z-scores
  # age must be months for this function
  who2007(FileLab = 'MyZscore',
          FilePath = paste('~/Documents/Berkeley/Savic/test/outputs/',sep=''),
          mydf = above_five_frame,
          sex = SEX,
          age = AGE,
          weight = WT,
          height = HT)

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

  na_parsed <- result %>% replace_na(list(
                                      WAZ = missing_flag,
                                      HAZ = missing_flag,
                                      BAZ = missing_flag,
                                      WHZ = missing_flag,
                                      WAZ_F=0, HAZ_F=0, BAZ_F=0, WHZ_F=0)
                                    )

  fully_parsed <- na_parsed %>%  mutate(
                                WAZ = ifelse(WAZ_F == 1, -98, WAZ),
                                HAZ = ifelse(HAZ_F == 1, -98, HAZ),
                                BAZ = ifelse(BAZ_F == 1, -98, BAZ),
                                WHZ = ifelse(WHZ_F == 1, -98, WHZ)
                            ) %>%
                            select(-WAZ_F, -HAZ_F, -BAZ_F, -WHZ_F)

  # return
  fully_parsed
}

