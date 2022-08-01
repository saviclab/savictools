#' Calculate z-scores for anthrompometric indicators
#'
#' `zscores()` computes z-scores indicating nutritional status, adding new
#'  columns to the end of a dataframe.
#'
#' `zscores()` is a vectorized, more streamlined implementation of the WHO scripts
#' `igrowup_standard.R`, `igrowup_restricted.R`, and `who2007.R`.
#'
#' @param data A dataframe with ID, AGE, SEX, WT, and HT columns.
#' @param units Units for AGE. Default is "months".
#' @param missing_flag Value used to replace missing z-scores Default is NA.
#' @param extreme_flag Value used to replace extreme/implausible z-scores.
#' Default is NA.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' zscores(pop_example, units = "months")
#'
#' @export

# TODO: Include some authorship info for WHO functions
# TODO: Clean up WHO function docs
# TODO: Make lines wrap at 80 characters
# TODO: Preserve column order
zscores <-
  function(data,
           units = c("months", "years", "weeks"),
           missing_flag = NA,
           extreme_flag = NA) {
    units <- match.arg(units)

    # convert units to months, if necessary
    if (units == "years") {
      data$AGE <- data$AGE * 12
    }
    else if (units == "weeks") {
      data$AGE <- data$AGE / 4.345
    }
    data$rownum <- 1:nrow(data)

    # Null out existing z-score columns
    data$WHZ <- NA
    data$WAZ <- NA
    data$BAZ <- NA
    data$HAZ <- NA
    data$BMI <- NA
    data$WHZ_F <- NA
    data$WAZ_F <- NA
    data$BAZ_F <- NA
    data$HAZ_F <- NA


    # Check for missing
    below_five <- data %>%
      dplyr::filter(AGE <= 60 | is.na(AGE)) %>%
      dplyr::select(ID, rownum, AGE, SEX, WT, HT)
    below_five_frame <- as.data.frame(below_five)

    above_five <- data %>%
      dplyr::filter(AGE > 60) %>%
      dplyr::select(ID, rownum, AGE, SEX, WT, HT)
    above_five_frame <- as.data.frame(above_five)

    # Load WHO datasets

    # SCRIPT: Under 5 years
    # source("R/igrowup_standard.r", local = TRUE)
    # load("R/sysdata.rda")

    if (nrow(below_five_frame) > 0) {
      #calculate Z-scores
      matz_below_5 <- igrowup.standard_vec(
        mydf = below_five_frame,
        sex = SEX,
        age = AGE,
        age.month = T,
        weight = WT,
        lenhei = HT
      )

      #select and rename columns
      zvars_below_5 <- matz_below_5[, c(
        'ID',
        'rownum',
        'cbmi',
        'zwei',
        'zlen',
        'zbmi',
        'zwfl',
        'fwei',
        'flen',
        'fwfl',
        'fbmi'
      )]
      zvars_below_5 <- dplyr::rename(
        zvars_below_5,
        BMI = cbmi,
        WAZ = zwei,
        HAZ = zlen,
        WHZ = zwfl,
        BAZ = zbmi,
        WAZ_F = fwei, # WAZ out of range flag
        HAZ_F = flen, # HAZ out of range flag
        WHZ_F = fwfl, # WHZ out of range flag
        BAZ_F = fbmi  # BAZ out of range flag
      )
    }

    # Calculate Z-scores
    # age must be months for this function

    if (nrow(above_five_frame) > 0) {
      matz_above_5 <- who2007_vec(
        mydf = above_five_frame,
        sex = SEX,
        age = AGE,
        weight = WT,
        height = HT
      )


      zvars_above_5 <- matz_above_5[, c('ID',
                                        'rownum',
                                        'cbmi',
                                        'zwfa',
                                        'zhfa',
                                        'zbfa',
                                        'fwfa',
                                        'fhfa',
                                        'fbfa')]
      zvars_above_5 <- dplyr::rename(
        zvars_above_5,
        BMI = cbmi,
        WAZ = zwfa,
        HAZ = zhfa,
        BAZ = zbfa,
        WAZ_F = fwfa, # WAZ out of range flag
        HAZ_F = fhfa, # HAZ out of range flag
        BAZ_F = fbfa  # BAZ out of range flag
      ) %>%
        dplyr::mutate(
          WHZ_F = 0,
          WHZ = NA
        )
    }

    # To merge data sets, first merge z-scores together
    if (nrow(below_five_frame) > 0 & nrow(above_five_frame) > 0) {
      zvars_full <- dplyr::bind_rows(zvars_below_5, zvars_above_5)
    } else if (nrow(below_five_frame) > 0) {
      zvars_full <- zvars_below_5
    } else if (nrow(above_five_frame) > 0) {
      zvars_full <- zvars_above_5
    }

    zvars_full <- dplyr::arrange(zvars_full, rownum)

    data$WHZ <- zvars_full$WHZ
    data$WAZ <- zvars_full$WAZ
    data$BAZ <- zvars_full$BAZ
    data$HAZ <- zvars_full$HAZ
    data$BMI <- zvars_full$BMI
    data$WHZ_F <- zvars_full$WHZ_F
    data$WAZ_F <- zvars_full$WAZ_F
    data$BAZ_F <- zvars_full$BAZ_F
    data$HAZ_F <- zvars_full$HAZ_F
    # zvars_full <- zvars_below_5
    # result <- dplyr::left_join(data, zvars_full, by = "rownum") %>%
    #  dplyr::rename(ID = ID.x) %>% dplyr::select(-ID.y,-rownum)


    # deal with missing and extreme values

    extreme_parsed <- data %>%  dplyr::mutate(
      WAZ = ifelse(WAZ_F == 1, extreme_flag, WAZ),
      HAZ = ifelse(HAZ_F == 1, extreme_flag, HAZ),
      BAZ = ifelse(BAZ_F == 1, extreme_flag, BAZ),
      WHZ = ifelse(WHZ_F == 1, extreme_flag, WHZ)
    )

    extreme_parsed$WAZ_F <- NULL
    extreme_parsed$HAZ_F <- NULL
    extreme_parsed$BAZ_F <- NULL
    extreme_parsed$WHZ_F <- NULL
    extreme_parsed$rownum <- NULL

    na_parsed <- extreme_parsed %>% tidyr::replace_na(
      list(
        WAZ = missing_flag,
        HAZ = missing_flag,
        BAZ = missing_flag,
        WHZ = missing_flag,
        BMI = missing_flag
      )
    )

    return(na_parsed)
  }



#-------------------------- Vectorized versions start here


calc.zlen_vec <- function(mat, lenanthro) {
  age_sex <- lenanthro$age * 10 + lenanthro$sex
  x <-
    lenanthro[match(mat$age.days * 10 + mat$sex, age_sex),]

  l.val <- x$l
  m.val <- x$m
  s.val <- x$s

  mat$zlen <- (((mat$clenhei / m.val) ^ l.val) - 1) / (s.val * l.val)
  mat$zlen <- ifelse(!is.na(mat$age.days) &
                       mat$age.days >= 0 & mat$age.days <= 1856,
                     mat$zlen,
                     NA_real_)
  mat

}

######################################################################################
### Function for calculating individual Head circumference-for-age z-scores
######################################################################################

calc.zhc_vec <- function(mat, hcanthro) {

  age_sex <- hcanthro$age * 10 + hcanthro$sex
  x <-
    hcanthro[match(mat$age.days * 10 + mat$sex, age_sex),]

  l.val <- x$l
  m.val <- x$m
  s.val <- x$s

  mat$zhc <- (((mat$headc / m.val) ^ l.val) - 1) / (s.val * l.val)
  mat$zhc <- ifelse(!is.na(mat$age.days) &
                       mat$age.days >= 0 & mat$age.days <= 1856,
                     mat$zhc,
                     NA_real_)
  mat

}

######################################################################################
### Function for calculating individual Weight-for-age z-scores
######################################################################################

calc.zwei_vec <- function(mat, weianthro) {

  age_sex <- weianthro$age * 10 + weianthro$sex
  x <-
    weianthro[match(mat$age.days * 10 + mat$sex, age_sex),]

  l.val <- x$l
  m.val <- x$m
  s.val <- x$s

  mat$zwei <- (((mat$weight / m.val) ^ l.val) - 1) / (s.val * l.val)

  sd3pos <- m.val * ((1 + l.val * s.val * 3) ^ (1 / l.val))
  sd23pos <- sd3pos - m.val * ((1 + l.val * s.val * 2) ^ (1 / l.val))
  sd3neg <- m.val * ((1 + l.val * s.val * (-3)) ^ (1 / l.val))
  sd23neg <- m.val * ((1 + l.val * s.val * (-2)) ^ (1 / l.val)) - sd3neg


  mat$zwei <- ifelse(mat$zwei > 3,
                     3 + ((mat$weight - sd3pos) / sd23pos),
                     ifelse(mat$zwei < -3,
                            -3 + ((mat$weight - sd3neg) / sd23neg),
                            mat$zwei))

  mat$zwei <- ifelse(
    !is.na(mat$age.days) &
      mat$age.days >= 0 &
      mat$age.days <= 1856 & mat$oedema != "y",
    mat$zwei,
    NA_real_
  )
  mat

}



######################################################################################
### Function for calculating individual Arm circumference-for-age z-scores
######################################################################################


calc.zac_vec <- function(mat, acanthro) {
  age_sex <- acanthro$age * 10 + acanthro$sex
  x <-
    acanthro[match(mat$age.days * 10 + mat$sex, age_sex), ]

  l.val <- x$l
  m.val <- x$m
  s.val <- x$s

  mat$zac <- (((mat$armc / m.val) ^ l.val) - 1) / (s.val * l.val)

  sd3pos <- m.val * ((1 + l.val * s.val * 3) ^ (1 / l.val))
  sd23pos <- sd3pos - m.val * ((1 + l.val * s.val * 2) ^ (1 / l.val))
  sd3neg <- m.val * ((1 + l.val * s.val * (-3)) ^ (1 / l.val))
  sd23neg <- m.val * ((1 + l.val * s.val * (-2)) ^ (1 / l.val)) - sd3neg

  mat$zac <- ifelse(mat$zac > 3,
                    3 + ((mat$armc - sd3pos) / sd23pos),
                    ifelse(mat$zac < -3,-3 + ((mat$armc - sd3neg) / sd23neg),
                           mat$zac))

  mat$zac <- ifelse(!is.na(mat$age.days) &
                      mat$age.days >= 91 & mat$age.days <= 1856,
                    mat$zac,
                    NA_real_)
  mat

}

######################################################################################
### Function for calculating individual Triceps skinfold-for-age z-scores
######################################################################################

calc.zts_vec <- function(mat, tsanthro) {
  age_sex <- tsanthro$age * 10 + tsanthro$sex
  x <-
    tsanthro[match(mat$age.days * 10 + mat$sex, age_sex),]

  l.val <- x$l
  m.val <- x$m
  s.val <- x$s

  mat$zts <- (((mat$triskin / m.val) ^ l.val) - 1) / (s.val * l.val)

  sd3pos <- m.val * ((1 + l.val * s.val * 3) ^ (1 / l.val))
  sd23pos <- sd3pos - m.val * ((1 + l.val * s.val * 2) ^ (1 / l.val))
  sd3neg <- m.val * ((1 + l.val * s.val * (-3)) ^ (1 / l.val))
  sd23neg <- m.val * ((1 + l.val * s.val * (-2)) ^ (1 / l.val)) - sd3neg

  mat$zts <- ifelse(mat$zts > 3,
                    3 + ((mat$triskin - sd3pos) / sd23pos),
                    ifelse(mat$zts < -3,-3 + ((
                      mat$triskin - sd3neg
                    ) / sd23neg),
                    mat$zts))

  mat$zts <- ifelse(!is.na(mat$age.days) &
                      mat$age.days >= 91 & mat$age.days <= 1856,
                    mat$zts,
                    NA_real_)
  mat

}


######################################################################################
### Function for calculating individual Subscapular skinfold-for-age z-scores
######################################################################################

calc.zss_vec <- function(mat, ssanthro) {
  age_sex <- ssanthro$age * 10 + ssanthro$sex
  x <-
    ssanthro[match(mat$age.days * 10 + mat$sex, age_sex), ]

  l.val <- x$l
  m.val <- x$m
  s.val <- x$s

  mat$zss <- (((mat$subskin / m.val) ^ l.val) - 1) / (s.val * l.val)

  sd3pos <- m.val * ((1 + l.val * s.val * 3) ^ (1 / l.val))
  sd23pos <- sd3pos - m.val * ((1 + l.val * s.val * 2) ^ (1 / l.val))
  sd3neg <- m.val * ((1 + l.val * s.val * (-3)) ^ (1 / l.val))
  sd23neg <- m.val * ((1 + l.val * s.val * (-2)) ^ (1 / l.val)) - sd3neg

  mat$zss <- ifelse(mat$zss > 3,
                    3 + ((mat$subskin - sd3pos) / sd23pos),
                    ifelse(mat$zss < -3, -3 + ((
                      mat$subskin - sd3neg
                    ) / sd23neg),
                    mat$zss))

  mat$zss <- ifelse(!is.na(mat$age.days) &
                      mat$age.days >= 91 & mat$age.days <= 1856,
                    mat$zss,
                    NA_real_)
  mat

}


######################################################################################
### Function for calculating individual Weight-for-length/height z-scores
######################################################################################

calc.zwfl_vec <- function(mat, wflanthro, wfhanthro) {
  low.len <- trunc(mat$clenhei * 10) / 10
  upp.len <- trunc(mat$clenhei * 10 + 1) / 10
  diff.len <- (mat$clenhei - low.len) / 0.1

  length_sex <- wflanthro$length * 100 + wflanthro$sex
  height_sex <- wfhanthro$height * 100 + wfhanthro$sex

  x_length_low <-
    wflanthro[match(low.len * 100 + mat$sex, length_sex),]
  x_length_upp <-
    wflanthro[match(upp.len * 100 + mat$sex, length_sex),]

  x_height_low <-
    wfhanthro[match(low.len * 100 + mat$sex, height_sex),]
  x_height_upp <-
    wfhanthro[match(upp.len * 100 + mat$sex, height_sex),]

  l.val <-
    ifelse(
      mat$age.days < 731 | (is.na(mat$age.days) &
        (mat$l.h == "l" | mat$l.h == "L" | mat$clenhei < 87)),
      # length
      ifelse(
        mat$clenhei >= 45 & mat$clenhei < 110,
        ifelse(
          diff.len > 0,
          x_length_low$l + diff.len * (x_length_upp$l - x_length_low$l),
          x_length_low$l
        ),
       NA
      ),
      # height
      ifelse(
        mat$clenhei >= 65 & mat$clenhei <= 120,
        ifelse(
          diff.len > 0,
          x_height_low$l + diff.len * (x_height_upp$l - x_height_low$l),
          x_height_low$l
        ),
        NA
      )
    )

  m.val <-
    ifelse(
      mat$age.days < 731 | (is.na(mat$age.days) &
                              (mat$l.h == "l" | mat$l.h == "L" | mat$clenhei < 87)),
      # length
      ifelse(
        mat$clenhei >= 45 & mat$clenhei < 110,
        ifelse(
          diff.len > 0,
          x_length_low$m + diff.len * (x_length_upp$m - x_length_low$m),
          x_length_low$m
        ),
        NA
      ),
      # height
      ifelse(
        mat$clenhei >= 65 & mat$clenhei <= 120,
        ifelse(
          diff.len > 0,
          x_height_low$m + diff.len * (x_height_upp$m - x_height_low$m),
          x_height_low$m
        ),
        NA
      )
    )


  s.val <-
    ifelse(
      mat$age.days < 731 | (is.na(mat$age.days) &
                              (mat$l.h == "l" | mat$l.h == "L" | mat$clenhei < 87)),
      # length
      ifelse(
        mat$clenhei >= 45 & mat$clenhei < 110,
        ifelse(
          diff.len > 0,
          x_length_low$s + diff.len * (x_length_upp$s - x_length_low$s),
          x_length_low$s
        ),
        NA
      ),
      # height
      ifelse(
        mat$clenhei >= 65 & mat$clenhei <= 120,
        ifelse(
          diff.len > 0,
          x_height_low$s + diff.len * (x_height_upp$s - x_height_low$s),
          x_height_low$s
        ),
        NA
      )
    )



  mat$zwfl <- (((mat$weight / m.val) ^ l.val) - 1) / (s.val * l.val)

  sd3pos <- m.val * ((1 + l.val * s.val * 3) ^ (1 / l.val))
  sd23pos <- sd3pos - m.val * ((1 + l.val * s.val * 2) ^ (1 / l.val))
  sd3neg <- m.val * ((1 + l.val * s.val * (-3)) ^ (1 / l.val))
  sd23neg <- m.val * ((1 + l.val * s.val * (-2)) ^ (1 / l.val)) - sd3neg


  mat$zwfl <- ifelse(mat$zwfl > 3,
                     3 + ((mat$weight - sd3pos) / sd23pos),
                     ifelse(mat$zwfl < -3,-3 + ((
                       mat$weight - sd3neg
                     ) / sd23neg),
                     mat$zwfl))

  mat$zwfl <- ifelse(
    !is.na(mat$age.days) &
      mat$age.days >= 0 &
      mat$age.days <= 1856 & mat$oedema != "y",
    #    mat$age.days >= 91 & mat$age.days <= 1856,
    mat$zwfl,
    NA_real_
  )
  mat

}


######################################################################################
### Function for calulating individual BMI-for-age z-scores
######################################################################################


calc.zbmi_vec <- function(mat, bmianthro) {
  age_sex <- bmianthro$age * 10 + bmianthro$sex
  x <-
    bmianthro[match(mat$age.days * 10 + mat$sex, age_sex),]

  l.val <- x$l
  m.val <- x$m
  s.val <- x$s

  mat$zbmi <- (((mat$cbmi / m.val) ^ l.val) - 1) / (s.val * l.val)

  sd3pos <- m.val * ((1 + l.val * s.val * 3) ^ (1 / l.val))
  sd23pos <- sd3pos - m.val * ((1 + l.val * s.val * 2) ^ (1 / l.val))
  sd3neg <- m.val * ((1 + l.val * s.val * (-3)) ^ (1 / l.val))
  sd23neg <- m.val * ((1 + l.val * s.val * (-2)) ^ (1 / l.val)) - sd3neg

  mat$zbmi <- ifelse(mat$zbmi > 3,
                     3 + ((mat$cbmi - sd3pos) / sd23pos),
                     ifelse(mat$zbmi < -3,-3 + ((mat$cbmi - sd3neg) / sd23neg),
                            mat$zbmi))

  mat$zbmi <- ifelse(
    !is.na(mat$age.days) &
      mat$age.days >= 0 &
      mat$age.days <= 1856 & mat$oedema != "y",
    mat$zbmi,
    NA_real_
  )
  mat

}

###################################################################################
#### Main function starts here: igrowup
###################################################################################

###############################################################################################################################################
#### This function can be used to:
#### 1. Calculate the z-scores for the indicators: length/height-for-age, weight-for-age, weight-for-legnth/height and body mass index-for-age
####    The output file with z-scores values is exported the file to an Excel spreadsheet (see readme file);
#### 2. Calculate the prevalence rates of stunting, underweight, wasting and overweight, and z-scores means and standard deviations. Results
####    are exported to an Excel spreadsheet, displayed by age group.
###############################################################################################################################################


#############################################################################
##### Function for calculating the z-scores for all indicators
#############################################################################

igrowup.standard_vec <- function(
  mydf,
  sex,
  age,
  age.month=F,
  weight=NA,
  lenhei=NA,
  measure=NA,
  headc=NA,
  armc=NA,
  triskin=NA,
  subskin=NA,
  oedema="n",
  sw=1) {

  #############################################################################
  ###########   Calculating the z-scores for all indicators
  #############################################################################

  sex.x<-as.character(mydf[,deparse(substitute(sex))])
  age.x<-as.double(mydf[,deparse(substitute(age))])
  if(!missing(weight)) weight.x<-as.double(mydf[,deparse(substitute(weight))]) else weight.x<-as.double(weight)
  if(!missing(lenhei)) lenhei.x<-as.double(mydf[,deparse(substitute(lenhei))]) else lenhei.x<-as.double(lenhei)
  if(!missing(headc)) headc.x<-as.double(mydf[,deparse(substitute(headc))]) else headc.x<-as.double(headc)
  if(!missing(armc)) armc.x<-as.double(mydf[,deparse(substitute(armc))]) else armc.x<-as.double(armc)
  if(!missing(triskin)) triskin.x<-as.double(mydf[,deparse(substitute(triskin))]) else triskin.x<-as.double(triskin)
  if(!missing(subskin)) subskin.x<-as.double(mydf[,deparse(substitute(subskin))]) else subskin.x<-as.double(subskin)
  if(!missing(measure)) lorh.vec<-as.character(mydf[,deparse(substitute(measure))]) else lorh.vec<-as.character(measure)
  if(!missing(oedema)) oedema.vec<-as.character(mydf[,deparse(substitute(oedema))]) else oedema.vec<-oedema
  if(!missing(sw))	sw<-as.double(mydf[,deparse(substitute(sw))])	else sw<-as.double(sw)
  sw <- ifelse(is.na(sw),0,sw)

  sex.vec<-NULL

  if(age.month) age.vec<-rounde(age.x*30.4375) else age.vec<-rounde(age.x)
  lenhei.vec<-ifelse((!is.na(age.vec) & age.vec<731 & !is.na(lorh.vec) & (lorh.vec=="h" | lorh.vec=="H")),lenhei.x+0.7,#
                     ifelse((!is.na(age.vec) & age.vec>=731 & !is.na(lorh.vec) & (lorh.vec=="l" | lorh.vec=="L")),lenhei.x-0.7,lenhei.x))

  sex.vec<-ifelse(!is.na(sex.x) & (sex.x=="m" | sex.x=="M" | sex.x=="1"),1,ifelse(!is.na(sex.x) & (sex.x=="f" | sex.x=="F" | sex.x=="2"),2,NA))

  lorh.vec<-ifelse(is.na(lorh.vec) | lorh.vec=="l" | lorh.vec=="L" | lorh.vec=="h" | lorh.vec=="H",lorh.vec,NA)

  oedema.vec<-ifelse(oedema.vec=="n" | oedema.vec=="N","n",ifelse(oedema.vec=="y" | oedema.vec=="Y","y","n"))

  mat<-cbind.data.frame(age.x,as.integer(age.vec),as.double(sex.vec),weight.x,lenhei.x,lorh.vec,lenhei.vec,headc.x,armc.x,triskin.x,subskin.x,oedema.vec,sw,stringsAsFactors=F)
  names(mat)<-c("age","age.days","sex","weight","len.hei","l.h","clenhei","headc","armc","triskin","subskin","oedema","sw")

  mat$cbmi<-mat$weight/((lenhei.vec/100)^2)
  mat$zlen<-rep(NA,length(mat$age))
  mat$zwei<-rep(NA,length(mat$age))
  mat$zwfl<-rep(NA,length(mat$age))
  mat$zbmi<-rep(NA,length(mat$age))
  mat$zhc<-rep(NA,length(mat$age))
  mat$zac<-rep(NA,length(mat$age))
  mat$zts<-rep(NA,length(mat$age))
  mat$zss<-rep(NA,length(mat$age))

  mat$zlen<-rep(NA,length(mat$age))
  mat$flen<-rep(NA,length(mat$age))
  mat$fwei<-rep(NA,length(mat$age))
  mat$fwfl<-rep(NA,length(mat$age))
  mat$fbmi<-rep(NA,length(mat$age))
  mat$fhc<-rep(NA,length(mat$age))
  mat$fac<-rep(NA,length(mat$age))
  mat$fts<-rep(NA,length(mat$age))
  mat$fss<-rep(NA,length(mat$age))



  #############################################################################
  ###########   Calculating the z-scores for all indicators
  #############################################################################

  ### Length-for-age z-score

  mat<-calc.zlen_vec(mat,lenanthro)
  ### Head circumference-for-age z-score

  mat<-calc.zhc_vec(mat,hcanthro)

  ### Weight-for-age z-score

  mat<-calc.zwei_vec(mat,weianthro)

  ### Arm circumference-for-age z-score

  mat<-calc.zac_vec(mat,acanthro)

  ### Triceps skinfold-for-age z-score

  mat<-calc.zts_vec(mat,tsanthro)

  ### Subscapular skinfold-for-age z-score

  mat<-calc.zss_vec(mat,ssanthro)

  ### Weight-for-length/height z-score

  mat<-calc.zwfl_vec(mat,wflanthro,wfhanthro)

  ### BMI-for-age z-score

  mat<-calc.zbmi_vec(mat,bmianthro)

  #### roundeing the z-scores to two decimals

  mat$zlen<-rounde(mat$zlen,digits=2)
  mat$zwei<-rounde(mat$zwei,digits=2)
  mat$zwfl<-rounde(mat$zwfl,digits=2)
  mat$zbmi<-rounde(mat$zbmi,digits=2)
  mat$zhc<-rounde(mat$zhc,digits=2)
  mat$zac<-rounde(mat$zac,digits=2)
  mat$zts<-rounde(mat$zts,digits=2)
  mat$zss<-rounde(mat$zss,digits=2)

  #### Flagging z-score values for individual indicators

  mat$flen<-ifelse(abs(mat$zlen) > 6,1,0)
  mat$fwei<-ifelse(mat$zwei > 5 | mat$zwei < (-6),1,0)
  mat$fwfl<-ifelse(abs(mat$zwfl) > 5,1,0)
  mat$fbmi<-ifelse(abs(mat$zbmi) > 5,1,0)
  mat$fhc<-ifelse(abs(mat$zhc) > 5,1,0)
  mat$fac<-ifelse(abs(mat$zac) > 5,1,0)
  mat$fts<-ifelse(abs(mat$zts) > 5,1,0)
  mat$fss<-ifelse(abs(mat$zss) > 5,1,0)

  mat <- cbind.data.frame(mydf,mat[,-c(1,3:6,8:9)])


  ###################################################################################################
  ######### Export data frame with z-scores and flag variables
  ###################################################################################################

  return(mat)

}


# Helpers for who2007




calc.zhfa2007_vec <- function(mat, hfawho2007) {
  low.age <- trunc(mat$age.mo)
  upp.age <- trunc(mat$age.mo + 1)
  diff.age <- (mat$age.mo - low.age)
  age_sex <- hfawho2007$age * 100 + hfawho2007$sex

  x_age_low <-
    hfawho2007[match(low.age * 100 + mat$sex, age_sex),]
  x_age_upp <-
    hfawho2007[match(upp.age * 100 + mat$sex, age_sex),]

  l.val <-
    ifelse(diff.age > 0,
           x_age_low$l + diff.age * (x_age_upp$l - x_age_low$l),
           x_age_low$l)

  m.val <-
    ifelse(diff.age > 0,
           x_age_low$m + diff.age * (x_age_upp$m - x_age_low$m),
           x_age_low$m)

  s.val <-
    ifelse(diff.age > 0,
           x_age_low$s + diff.age * (x_age_upp$s - x_age_low$s),
           x_age_low$s)

  mat$zhfa <- (((mat$height / m.val) ^ l.val) - 1) / (s.val * l.val)

  mat$zhfa <- ifelse(!is.na(mat$age.mo) &
                       mat$age.mo >= 61 &
                       mat$age.mo < 229,
                     mat$zhfa,
                     NA_real_)
  mat

}

######################################################################################
### Function for calculating individual weight-for-age z-scores
######################################################################################


calc.zwei2007_vec <- function(mat, wfawho2007) {
  low.age <- trunc(mat$age.mo)
  upp.age <- trunc(mat$age.mo + 1)
  diff.age <- (mat$age.mo - low.age)
  age_sex <- wfawho2007$age * 100 + wfawho2007$sex

  x_age_low <-
    wfawho2007[match(low.age * 100 + mat$sex, age_sex), ]
  x_age_upp <-
    wfawho2007[match(upp.age * 100 + mat$sex, age_sex), ]

  l.val <-
    ifelse(diff.age > 0,
           x_age_low$l + diff.age * (x_age_upp$l - x_age_low$l),
           x_age_low$l)

  m.val <-
    ifelse(diff.age > 0,
           x_age_low$m + diff.age * (x_age_upp$m - x_age_low$m),
           x_age_low$m)

  s.val <-
    ifelse(diff.age > 0,
           x_age_low$s + diff.age * (x_age_upp$s - x_age_low$s),
           x_age_low$s)

  mat$zwfa <- (((mat$weight / m.val) ^ l.val) - 1) / (s.val * l.val)

  sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
  sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
  sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
  sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg

  mat$zwfa <- ifelse(mat$zwfa > 3,
                     3 + ((mat$weight - sd3pos) / sd23pos),
                     ifelse(mat$zwfa < -3,
                            -3 + ((mat$weight - sd3neg) / sd23neg),
                            mat$zwfa))

  mat$zwfa <- ifelse(
    !is.na(mat$age.mo) &
      mat$age.mo >= 61 &
      mat$age.mo < 121 &
      mat$oedema != "y",
    mat$zwfa,
    NA_real_
  )
  mat

}

######################################################################################
### Function for calulating individual BMI-for-age z-scores
######################################################################################


calc.zbmi2007_vec <- function(mat, bfawho2007) {
  low.age <- trunc(mat$age.mo)
  upp.age <- trunc(mat$age.mo + 1)
  diff.age <- (mat$age.mo - low.age)

  age_sex <- bfawho2007$age * 100 + bfawho2007$sex

  x_age_low <-
    bfawho2007[match(low.age * 100 + mat$sex, age_sex),]
  x_age_upp <-
    bfawho2007[match(upp.age * 100 + mat$sex, age_sex),]

  l.val <-
    ifelse(diff.age > 0,
           x_age_low$l + diff.age * (x_age_upp$l - x_age_low$l),
           x_age_low$l)

  m.val <-
    ifelse(diff.age > 0,
           x_age_low$m + diff.age * (x_age_upp$m - x_age_low$m),
           x_age_low$m)

  s.val <-
    ifelse(diff.age > 0,
           x_age_low$s + diff.age * (x_age_upp$s - x_age_low$s),
           x_age_low$s)

  mat$zbfa <- (((mat$cbmi / m.val) ^ l.val) - 1) / (s.val * l.val)

  sd3pos <- m.val * ((1 + l.val * s.val * 3) ^ (1 / l.val))
  sd23pos <- sd3pos - m.val * ((1 + l.val * s.val * 2) ^ (1 / l.val))
  sd3neg <- m.val * ((1 + l.val * s.val * (-3)) ** (1 / l.val))
  sd23neg <- m.val * ((1 + l.val * s.val * (-2)) ** (1 / l.val)) - sd3neg

  mat$zbfa <- ifelse(mat$zbfa > 3,
                     3 + ((mat$cbmi - sd3pos) / sd23pos),
                     ifelse(mat$zbfa < -3,-3 + ((mat$cbmi - sd3neg) / sd23neg),
                            mat$zbfa))

  mat$zbfa <- ifelse(
    !is.na(mat$age.mo) &
      mat$age.mo >= 61 &
      mat$age.mo < 229 &
      mat$oedema != "y",
    mat$zbfa,
    NA_real_
  )
  mat

}


###################################################################################
#### Main function starts here: who2007
###################################################################################

###############################################################################################################################################
#### This function can be used to:
#### 1. Calculate the z-scores for the indicators: height-for-age, weight-for-age and body mass index-for-age
####    The output file with z-scores values is exported the file to an Excel spreadsheet (see readme file);
#### 2. Calculate the prevalence rates of stunting, underweight, wasting and overweight, and z-scores means and standard deviations. Results
####    are exported to an Excel spreadsheet, displayed by age group.
###############################################################################################################################################


#############################################################################
##### Function for calculating the z-scores for all indicators
#############################################################################

who2007_vec <- function(
  mydf,
  sex,
  age,
  weight,
  height,
  oedema="n",
  sw=1) {

  #############################################################################
  ###########   Calculating the z-scores for all indicators
  #############################################################################

  sex.x<-as.character(mydf[,deparse(substitute(sex))])
  age.x<-as.double(mydf[,deparse(substitute(age))])
  weight.x<-as.double(mydf[,deparse(substitute(weight))])
  height.x<-as.double(mydf[,deparse(substitute(height))])
  if(!missing(oedema)) oedema.vec<-as.character(mydf[,deparse(substitute(oedema))]) else oedema.vec<-oedema
  if(!missing(sw))	sw<-as.double(mydf[,deparse(substitute(sw))])	else sw<-as.double(sw)
  sw<-ifelse(is.na(sw),0,sw)

  sex.vec<-NULL
  sex.vec<-ifelse(sex.x!="NA" & (sex.x=="m" | sex.x=="M" | sex.x=="1"),1,ifelse(sex.x!="NA" & (sex.x=="f" | sex.x=="F" | sex.x=="2"),2,NA))
  age.vec<-age.x
  height.vec<-height.x
  oedema.vec<-ifelse(oedema.vec=="n" | oedema.vec=="N","n",ifelse(oedema.vec=="y" | oedema.vec=="Y","y","n"))

  mat<-cbind.data.frame(age.x,as.double(sex.vec),weight.x,height.x,oedema.vec,sw,stringsAsFactors=F)
  names(mat)<-c("age.mo","sex","weight","height","oedema","sw")

  mat$cbmi<-mat$weight/((height.vec/100)^2)
  mat$zhfa<-NULL
  mat$fhfa<-NULL
  mat$zwfa<-NULL
  mat$fwfa<-NULL
  mat$zbfa<-NULL
  mat$fbfa<-NULL

  #############################################################################
  ###########   Calculating the z-scores for all indicators
  #############################################################################
  ### Height-for-age z-score

  mat<-calc.zhfa2007_vec(mat,hfawho2007)

  ### Weight-for-age z-score

  mat<-calc.zwei2007_vec(mat,wfawho2007)

  ### BMI-for-age z-score

  mat<-calc.zbmi2007_vec(mat,bfawho2007)


  #### rounding the z-scores to two decimals

  mat$zhfa<-round(mat$zhfa,digits=2)
  mat$zwfa<-round(mat$zwfa,digits=2)
  mat$zbfa<-round(mat$zbfa,digits=2)

  #### Flagging z-score values for individual indicators

  mat$fhfa<-ifelse(abs(mat$zhfa) > 6,1,0)
  mat$fwfa<-ifelse(mat$zwfa > 5 | mat$zwfa < (-6),1,0)
  mat$fbfa<-ifelse(abs(mat$zbfa) > 5,1,0)

  ifelse(is.na(mat$age.mo) & mat$oedema=="y", function () {
    mat$fhfa<-NA
    mat$zwfa<-NA
    mat$zbfa<-NA
  }, NA)

  mat<-cbind.data.frame(mydf,mat[,-c(2:6)])

  ###################################################################################################
  ######### Export data frame with z-scores and flag variables
  ###################################################################################################

  return(mat)
}














