#' Calculate z-scores for anthrompometric indicators
#'
#' `zscores()` computes z-scores indicating nutritional status, adding new
#'  columns to the end of a dataframe.
#'
#' `zscores()` is a more streamlined implementation of the WHO scripts
#' `igrowup_standard.R`, `igrowup_restricted.R`, and `who2007.R`.
#'
#' @param data A dataframe with ID, AGE, SEX, WT, and HT columns.
#' @param missing_flag Value to replace NAs. Default is -99.
#' @param extreme_flag Value used to replace extreme/implausible z-scores.
#' Default is -98.
#'
#' @importFrom magrittr %>%
#' @export

zscores <- function(data, missing_flag=-99, extreme_flag=-98) {
  start <- Sys.time()
  data$rownum <- 1:nrow(data)

  below_five <- data %>% dplyr::filter(AGE <= 60) %>% dplyr::select(ID, rownum, AGE, SEX, WT, HT)
  below_five_frame <- as.data.frame(below_five)

  above_five <- data %>% dplyr::filter(AGE > 60) %>% dplyr::select(ID, rownum, AGE, SEX, WT, HT)
  above_five_frame <- as.data.frame(above_five)

  # Load WHO datasets

  # SCRIPT: Under 5 years
  source("R/igrowup_standard.r", local = TRUE)
  load("R/sysdata.rda")

  #calculate Z-scores
  matz <- igrowup.standard(mydf=below_five_frame,
                   sex=SEX,
                   age=AGE,
                   age.month=T,
                   weight=WT,
                   lenhei=HT)

  #select and rename columns
  zvars <- matz[,c('ID', "rownum", 'cbmi','zwei','zlen', 'zbmi', 'zwfl', 'fwei', 'flen', 'fwfl', 'fbmi')]
  zvars <- dplyr::rename(zvars, BMI = cbmi,
                  WAZ = zwei,
                  HAZ = zlen,
                  WHZ = zwfl,
                  BAZ = zbmi,
                  WAZ_F = fwei, #WAZ out of range flag
                  HAZ_F = flen, #HAZ out of range flag
                  WHZ_F = fwfl, #WHZ out of range flag
                  BAZ_F = fbmi) #BAZ out of range flag

  #Calculate Z-scores
  # age must be months for this function
  source("R/who2007.r", local = TRUE)

  matz2 <- who2007(mydf = above_five_frame,
          sex = SEX,
          age = AGE,
          weight = WT,
          height = HT)

  zvars1 <- matz2[,c('ID', "rownum", 'cbmi','zwfa', 'zhfa', 'zbfa', 'fwfa', 'fhfa', 'fbfa')]
  zvars1 <- dplyr::rename(zvars1, BMI = cbmi,
                   WAZ = zwfa,
                   HAZ = zhfa,
                   BAZ = zbfa,
                   WAZ_F = fwfa, #WAZ out of range flag
                   HAZ_F = fhfa, #HAZ out of range flag
                   BAZ_F = fbfa) #BAZ out of range flag


  # To merge data sets, first merge z-scores together
  zvars_full <- dplyr::bind_rows(zvars, zvars1)

  result <- dplyr::left_join(data, zvars_full, by="rownum") %>%
            dplyr::rename(ID = ID.x) %>% dplyr::select(-ID.y, -rownum)


  # deal with missing and  values

  na_parsed <- result %>% tidyr::replace_na(list(
                                      WAZ = missing_flag,
                                      HAZ = missing_flag,
                                      BAZ = missing_flag,
                                      WHZ = missing_flag,
                                      WAZ_F=0, HAZ_F=0, BAZ_F=0, WHZ_F=0)
                                    )

  fully_parsed <- na_parsed %>%  dplyr::mutate(
                                WAZ = ifelse(WAZ_F == 1, extreme_flag, WAZ),
                                HAZ = ifelse(HAZ_F == 1, extreme_flag, HAZ),
                                BAZ = ifelse(BAZ_F == 1, extreme_flag, BAZ),
                                WHZ = ifelse(WHZ_F == 1, extreme_flag, WHZ)
                            ) %>%
                            dplyr::select(-WAZ_F, -HAZ_F, -BAZ_F, -WHZ_F)

  # return
  end <- Sys.time()
  print(end-start)
  return(fully_parsed)
}

