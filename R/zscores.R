#' Calculate z-scores for anthrompometric indicators
#'
#' `zscores()` computes z-scores indicating nutritional status, adding new
#'  columns to the end of a dataframe.
#'
#' `zscores()` is a more streamlined implementation of the WHO scripts
#' `igrowup_standard.R`, `igrowup_restricted.R`, and `who2007.R`.
#'
#' @param data A dataframe with ID, AGE, SEX, WT, and HT columns.
#' @param units Units for AGE. Default is "months".
#' @param missing_flag Value used to replace missing z-scores Default is NA.
#' @param extreme_flag Value used to replace extreme/implausible z-scores.
#' Default is NA.
#'
#' @importFrom magrittr %>%
#' @export

# TODO: Include some authorship info for WHO functions
# TODO: Clean up WHO function docs
# TODO: Make lines wrap at 80 characters
# TODO: Preserve column order
<<<<<<< HEAD
=======
# TODO: Preserve row order
>>>>>>> f90b9fbeed27750b6b21f88690a13096e50c7007

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
    data$WHZ <- NULL
    data$WAZ <- NULL
    data$BAZ <- NULL
    data$HAZ <- NULL
    data$BMI <- NULL
    data$WHZ_F <- NULL
    data$WAZ_F <- NULL
    data$BAZ_F <- NULL
    data$HAZ_F <- NULL


    # Check for missing
    below_five <- data %>%
      dplyr::filter(AGE <= 60) %>%
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
      matz_below_5 <- igrowup.standard(
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
      matz_above_5 <- who2007(
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
    if (nrow(below_five_frame) > 0 && nrow(above_five_frame) > 0) {
      zvars_full <- dplyr::bind_rows(zvars_below_5, zvars_above_5)
    } else if (nrow(below_five_frame) > 0) {
      zvars_full <- zvars_below_5
    } else if (nrow(above_five_frame) > 0) {
      zvars_full <- zvars_above_5
    }

    # zvars_full <- zvars_below_5
    result <- dplyr::left_join(data, zvars_full, by = "rownum") %>%
      dplyr::rename(ID = ID.x) %>% dplyr::select(-ID.y,-rownum)

    # deal with missing and  values

    extreme_parsed <- result %>%  dplyr::mutate(
      WAZ = ifelse(WAZ_F == 1, extreme_flag, WAZ),
      HAZ = ifelse(HAZ_F == 1, extreme_flag, HAZ),
      BAZ = ifelse(BAZ_F == 1, extreme_flag, BAZ),
      WHZ = ifelse(WHZ_F == 1, extreme_flag, WHZ)
    ) %>%
      dplyr::select(-WAZ_F,-HAZ_F,-BAZ_F,-WHZ_F)


    na_parsed <- extreme_parsed %>% tidyr::replace_na(
      list(
        WAZ = missing_flag,
        HAZ = missing_flag,
        BAZ = missing_flag,
        WHZ = missing_flag
      )
    )

    fully_parsed <- na_parsed %>%  dplyr::mutate(
      WAZ = ifelse(WAZ_F == 1, extreme_flag, WAZ),
      HAZ = ifelse(HAZ_F == 1, extreme_flag, HAZ),
      BAZ = ifelse(BAZ_F == 1, extreme_flag, BAZ),
      WHZ = ifelse(WHZ_F == 1, extreme_flag, WHZ)
    ) %>%
      dplyr::select(-WAZ_F,-HAZ_F,-BAZ_F,-WHZ_F)

    return(fully_parsed)
  }


#' `rounde()` is a rounding function that rounds up when rounding off a 5,
#' instead of to the nearest even number, which is what `round()` does.
#' @param x A numeric vector
#' @param digits Number of significant significant digits to round to
#' @export
rounde <- function(x, digits = 0) {
  expo <- 10 ^ digits
  return(ifelse(abs(x * expo) - floor(abs(x * expo)) < 0.5,
                sign(x * expo) * floor(abs(x * expo)),
                sign(x * expo) * (floor(abs(x * expo)) + 1)) / expo)
}

calc.zlen<-function(mat, lenanthro) {

  for(i in 1:length(mat$age.days)) {

    if(!is.na(mat$age.days[i]) & mat$age.days[i]>=0 & mat$age.days[i]<=1856) {

      l.val<-lenanthro$l[lenanthro$age==mat$age.days[i] & lenanthro$sex==mat$sex[i]]
      m.val<-lenanthro$m[lenanthro$age==mat$age.days[i] & lenanthro$sex==mat$sex[i]]
      s.val<-lenanthro$s[lenanthro$age==mat$age.days[i] & lenanthro$sex==mat$sex[i]]
      mat$zlen[i]<-(((mat$clenhei[i]/m.val)^l.val)-1)/(s.val*l.val)

    }	else mat$zlen[i]<- NA

  }
  return(mat)
}

######################################################################################
### Function for calculating individual Head circumference-for-age z-scores
######################################################################################

calc.zhc<-function(mat,hcanthro){

  for(i in 1:length(mat$age.days)) {

    if(!is.na(mat$age.days[i]) & mat$age.days[i]>=0 & mat$age.days[i]<=1856) {

      l.val<-hcanthro$l[hcanthro$age==mat$age.days[i] & hcanthro$sex==mat$sex[i]]
      m.val<-hcanthro$m[hcanthro$age==mat$age.days[i] & hcanthro$sex==mat$sex[i]]
      s.val<-hcanthro$s[hcanthro$age==mat$age.days[i] & hcanthro$sex==mat$sex[i]]
      mat$zhc[i]<-(((mat$headc[i]/m.val)^l.val)-1)/(s.val*l.val)

    }	else mat$zhc[i]<- NA

  }
  return(mat)
}

######################################################################################
### Function for calculating individual Weight-for-age z-scores
######################################################################################

calc.zwei<-function(mat,weianthro){

  for(i in 1:length(mat$age.days)) {

    if(!is.na(mat$age.days[i]) & mat$age.days[i]>=0 & mat$age.days[i]<=1856 & mat$oedema[i]!="y") {

      l.val<-weianthro$l[weianthro$age==mat$age.days[i] & weianthro$sex==mat$sex[i]]
      m.val<-weianthro$m[weianthro$age==mat$age.days[i] & weianthro$sex==mat$sex[i]]
      s.val<-weianthro$s[weianthro$age==mat$age.days[i] & weianthro$sex==mat$sex[i]]

      mat$zwei[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zwei[i]) & mat$zwei[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zwei[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zwei[i]) & mat$zwei[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zwei[i]<- (-3)+((mat$weight[i]-sd3neg)/sd23neg)
      }

    } else mat$zwei[i]<-NA
  }
  return(mat)
}



######################################################################################
### Function for calculating individual Arm circumference-for-age z-scores
######################################################################################

calc.zac<-function(mat,acanthro){

  for(i in 1:length(mat$age.days)) {

    if(!is.na(mat$age.days[i]) & mat$age.days[i]>=91 & mat$age.days[i]<=1856) {

      l.val<-acanthro$l[acanthro$age==mat$age.days[i] & acanthro$sex==mat$sex[i]]
      m.val<-acanthro$m[acanthro$age==mat$age.days[i] & acanthro$sex==mat$sex[i]]
      s.val<-acanthro$s[acanthro$age==mat$age.days[i] & acanthro$sex==mat$sex[i]]
      mat$zac[i]<-(((mat$armc[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zac[i]) & mat$zac[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zac[i]<- 3+((mat$armc[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zac[i]) & mat$zac[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zac[i]<- (-3)+((mat$armc[i]-sd3neg)/sd23neg)
      }

    } else mat$zac[i]<-NA

  }
  return(mat)
}

######################################################################################
### Function for calculating individual Triceps skinfold-for-age z-scores
######################################################################################

calc.zts<-function(mat,tsanthro){

  for(i in 1:length(mat$age.days)) {

    if(!is.na(mat$age.days[i]) & mat$age.days[i]>=91 & mat$age.days[i]<=1856) {

      l.val<-tsanthro$l[tsanthro$age==mat$age.days[i] & tsanthro$sex==mat$sex[i]]
      m.val<-tsanthro$m[tsanthro$age==mat$age.days[i] & tsanthro$sex==mat$sex[i]]
      s.val<-tsanthro$s[tsanthro$age==mat$age.days[i] & tsanthro$sex==mat$sex[i]]

      mat$zts[i]<-(((mat$triskin[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zts[i]) & mat$zts[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zts[i]<- 3+((mat$triskin[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zts[i]) & mat$zts[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zts[i]<- (-3)+((mat$triskin[i]-sd3neg)/sd23neg)
      }

    } else mat$zts[i]<-NA

  }

  return(mat)
}


######################################################################################
### Function for calculating individual Subscapular skinfold-for-age z-scores
######################################################################################

calc.zss<-function(mat,ssanthro){

  for(i in 1:length(mat$age.days)) {

    if(!is.na(mat$age.days[i]) & mat$age.days[i]>=91 & mat$age.days[i]<=1856) {

      l.val<-ssanthro$l[ssanthro$age==mat$age.days[i] & ssanthro$sex==mat$sex[i]]
      m.val<-ssanthro$m[ssanthro$age==mat$age.days[i] & ssanthro$sex==mat$sex[i]]
      s.val<-ssanthro$s[ssanthro$age==mat$age.days[i] & ssanthro$sex==mat$sex[i]]

      mat$zss[i]<-(((mat$subskin[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zss[i]) & mat$zss[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zss[i]<- 3+((mat$subskin[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zss[i]) & mat$zss[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zss[i]<- (-3)+((mat$subskin[i]-sd3neg)/sd23neg)
      }

    } else mat$zss[i]<-NA
  }

  return(mat)
}


######################################################################################
### Function for calculating individual Weight-for-length/height z-scores
######################################################################################

calc.zwfl<-function(mat,wflanthro,wfhanthro){

  for(i in 1:length(mat$age.days)) {

    mat$zwfl[i]<-NA

    if(mat$oedema[i]!="y") {

      if( (!is.na(mat$age.days[i]) & mat$age.days[i]<731) | (is.na(mat$age.days[i]) & !is.na(mat$l.h[i]) & (mat$l.h[i]=="l" | mat$l.h[i]=="L")) | (is.na(mat$age.days[i]) & is.na(mat$l.h[i]) & !is.na(mat$clenhei[i]) & mat$clenhei[i]<87) ) {

        if(!is.na(mat$clenhei[i]) & mat$clenhei[i]>=45 & mat$clenhei[i]<=110) {

          ### Interpolated l,m,s values

          low.len<-trunc(mat$clenhei[i]*10)/10
          upp.len<-trunc(mat$clenhei[i]*10+1)/10
          diff.len<-(mat$clenhei[i]-low.len)/0.1

          if(diff.len>0) {
            l.val<-wflanthro$l[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]+diff.len*( wflanthro$l[wflanthro$length==upp.len & wflanthro$sex==mat$sex[i]]-wflanthro$l[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]] )
            m.val<-wflanthro$m[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]+diff.len*( wflanthro$m[wflanthro$length==upp.len & wflanthro$sex==mat$sex[i]]-wflanthro$m[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]] )
            s.val<-wflanthro$s[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]+diff.len*( wflanthro$s[wflanthro$length==upp.len & wflanthro$sex==mat$sex[i]]-wflanthro$s[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]] )
          } else {
            l.val<-wflanthro$l[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]
            m.val<-wflanthro$m[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]
            s.val<-wflanthro$s[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]
          }

          mat$zwfl[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
          if(!is.na(mat$zwfl[i]) & mat$zwfl[i]>3) {
            sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
            sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
            mat$zwfl[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
          }
          if(!is.na(mat$zwfl[i]) & mat$zwfl[i]<(-3)) {
            sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
            sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
            mat$zwfl[i]<- (-3)-((sd3neg-mat$weight[i])/sd23neg)
          }
        }
      }

      else 		if( (!is.na(mat$age.days[i]) & mat$age.days[i]>=731) | (is.na(mat$age.days[i]) & !is.na(mat$l.h[i]) & (mat$l.h[i]=="h" | mat$l.h[i]=="H"))  | (is.na(mat$age.days[i]) & is.na(mat$l.h[i]) & !is.na(mat$clenhei[i]) & mat$clenhei[i]>=87) ) {

        if(!is.na(mat$clenhei[i]) & mat$clenhei[i]>=65 & mat$clenhei[i]<=120) {

          ### Interpolated l,m,s values

          low.len<-trunc(mat$clenhei[i]*10)/10
          upp.len<-trunc(mat$clenhei[i]*10+1)/10
          diff.len<-(mat$clenhei[i]-low.len)/0.1

          if(diff.len>0) {
            l.val<-wfhanthro$l[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]+diff.len*( wfhanthro$l[wfhanthro$height==upp.len & wfhanthro$sex==mat$sex[i]]-wfhanthro$l[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]] )
            m.val<-wfhanthro$m[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]+diff.len*( wfhanthro$m[wfhanthro$height==upp.len & wfhanthro$sex==mat$sex[i]]-wfhanthro$m[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]] )
            s.val<-wfhanthro$s[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]+diff.len*( wfhanthro$s[wfhanthro$height==upp.len & wfhanthro$sex==mat$sex[i]]-wfhanthro$s[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]] )
          } else {
            l.val<-wfhanthro$l[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]
            m.val<-wfhanthro$m[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]
            s.val<-wfhanthro$s[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]
          }

          mat$zwfl[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
          if(!is.na(mat$zwfl[i]) & mat$zwfl[i]>3) {
            sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
            sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
            mat$zwfl[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
          }
          if(!is.na(mat$zwfl[i]) & mat$zwfl[i]<(-3)) {
            sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
            sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
            mat$zwfl[i]<- (-3)-((sd3neg-mat$weight[i])/sd23neg)
          }
        }
      }
    }

    if(!is.na(mat$age.day[i]) & mat$age.days[i]>1856) mat$zwfl[i]<-NA

  }

  return(mat)
}


######################################################################################
### Function for calulating individual BMI-for-age z-scores
######################################################################################

calc.zbmi<-function(mat,bmianthro){

  for(i in 1:length(mat$age.days)) {

    if(!is.na(mat$age.days[i]) & mat$age.days[i]>=0 & mat$age.days[i]<=1856 & mat$oedema[i]!="y") {

      l.val<-bmianthro$l[bmianthro$age==mat$age.days[i] & bmianthro$sex==mat$sex[i]]
      m.val<-bmianthro$m[bmianthro$age==mat$age.days[i] & bmianthro$sex==mat$sex[i]]
      s.val<-bmianthro$s[bmianthro$age==mat$age.days[i] & bmianthro$sex==mat$sex[i]]

      mat$zbmi[i]<-(((mat$cbmi[i]/m.val)^l.val)-1)/(s.val*l.val)

      if(!is.na(mat$zbmi[i]) & mat$zbmi[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zbmi[i]<- 3+((mat$cbmi[i]-sd3pos)/sd23pos)
      }

      if(!is.na(mat$zbmi[i]) & mat$zbmi[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zbmi[i]<- (-3)+((mat$cbmi[i]-sd3neg)/sd23neg)
      }

    } else { mat$zbmi[i]<-NA }

  }

  return(mat)
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

igrowup.standard <- function(
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

  mat<-calc.zlen(mat,lenanthro)
  ### Head circumference-for-age z-score

  mat<-calc.zhc(mat,hcanthro)

  ### Weight-for-age z-score

  mat<-calc.zwei(mat,weianthro)

  ### Arm circumference-for-age z-score

  mat<-calc.zac(mat,acanthro)

  ### Triceps skinfold-for-age z-score

  mat<-calc.zts(mat,tsanthro)

  ### Subscapular skinfold-for-age z-score

  mat<-calc.zss(mat,ssanthro)

  ### Weight-for-length/height z-score

  mat<-calc.zwfl(mat,wflanthro,wfhanthro)

  ### BMI-for-age z-score

  mat<-calc.zbmi(mat,bmianthro)

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


calc.zhfa2007<-function(mat,hfawho2007){

  for(i in 1:length(mat$age.mo)) {

    if(!is.na(mat$age.mo[i]) & mat$age.mo[i]>=61 & mat$age.mo[i]<229) {

      ### Interpolated l,m,s values

      low.age<-trunc(mat$age.mo[i])
      upp.age<-trunc(mat$age.mo[i]+1)
      diff.age<-(mat$age.mo[i]-low.age)

      if(diff.age>0) {
        l.val<-hfawho2007$l[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]+diff.age*( hfawho2007$l[hfawho2007$age==upp.age & hfawho2007$sex==mat$sex[i]]-hfawho2007$l[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]] )
        m.val<-hfawho2007$m[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]+diff.age*( hfawho2007$m[hfawho2007$age==upp.age & hfawho2007$sex==mat$sex[i]]-hfawho2007$m[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]] )
        s.val<-hfawho2007$s[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]+diff.age*( hfawho2007$s[hfawho2007$age==upp.age & hfawho2007$sex==mat$sex[i]]-hfawho2007$s[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]] )
      } else {
        l.val<-hfawho2007$l[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]
        m.val<-hfawho2007$m[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]
        s.val<-hfawho2007$s[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]
      }
      mat$zhfa[i]<-(((mat$height[i]/m.val)^l.val)-1)/(s.val*l.val)

    }	else mat$zhfa[i]<- NA

  }
  return(mat)
}

######################################################################################
### Function for calculating individual weight-for-age z-scores
######################################################################################

calc.zwei2007<-function(mat,wfawho2007){

  for(i in 1:length(mat$age.mo)) {

    if(!is.na(mat$age.mo[i])  & mat$age.mo[i]>=61 & mat$age.mo[i]<121 & mat$oedema[i]!="y") {

      ### Interpolated l,m,s values

      low.age<-trunc(mat$age.mo[i])
      upp.age<-trunc(mat$age.mo[i]+1)
      diff.age<-(mat$age.mo[i]-low.age)

      if(diff.age>0) {
        l.val<-wfawho2007$l[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]+diff.age*( wfawho2007$l[wfawho2007$age==upp.age & wfawho2007$sex==mat$sex[i]]-wfawho2007$l[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]] )
        m.val<-wfawho2007$m[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]+diff.age*( wfawho2007$m[wfawho2007$age==upp.age & wfawho2007$sex==mat$sex[i]]-wfawho2007$m[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]] )
        s.val<-wfawho2007$s[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]+diff.age*( wfawho2007$s[wfawho2007$age==upp.age & wfawho2007$sex==mat$sex[i]]-wfawho2007$s[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]] )
      } else {
        l.val<-wfawho2007$l[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]
        m.val<-wfawho2007$m[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]
        s.val<-wfawho2007$s[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]
      }

      mat$zwfa[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zwfa[i]) & mat$zwfa[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zwfa[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zwfa[i]) & mat$zwfa[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zwfa[i]<- (-3)+((mat$weight[i]-sd3neg)/sd23neg)
      }

    } else mat$zwfa[i]<-NA
  }
  return(mat)
}

######################################################################################
### Function for calulating individual BMI-for-age z-scores
######################################################################################

calc.zbmi2007<-function(mat,bfawho2007){

  for(i in 1:length(mat$age.mo)) {

    if(!is.na(mat$age.mo[i]) & mat$age.mo[i]>=61 & mat$age.mo[i]<229 & mat$oedema[i]!="y") {

      ### Interpolated l,m,s values

      low.age<-trunc(mat$age.mo[i])
      upp.age<-trunc(mat$age.mo[i]+1)
      diff.age<-(mat$age.mo[i]-low.age)

      if(diff.age>0) {
        l.val<-bfawho2007$l[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]+diff.age*( bfawho2007$l[bfawho2007$age==upp.age & bfawho2007$sex==mat$sex[i]]-bfawho2007$l[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]] )
        m.val<-bfawho2007$m[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]+diff.age*( bfawho2007$m[bfawho2007$age==upp.age & bfawho2007$sex==mat$sex[i]]-bfawho2007$m[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]] )
        s.val<-bfawho2007$s[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]+diff.age*( bfawho2007$s[bfawho2007$age==upp.age & bfawho2007$sex==mat$sex[i]]-bfawho2007$s[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]] )
      } else {
        l.val<-bfawho2007$l[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]
        m.val<-bfawho2007$m[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]
        s.val<-bfawho2007$s[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]
      }

      mat$zbfa[i]<-(((mat$cbmi[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zbfa[i]) & mat$zbfa[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zbfa[i]<- 3+((mat$cbmi[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zbfa[i]) & mat$zbfa[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zbfa[i]<- (-3)+((mat$cbmi[i]-sd3neg)/sd23neg)
      }

    } else mat$zbfa[i]<-NA

  }

  return(mat)
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

who2007 <- function(
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

  mat<-calc.zhfa2007(mat,hfawho2007)

  ### Weight-for-age z-score

  mat<-calc.zwei2007(mat,wfawho2007)

  ### BMI-for-age z-score

  mat<-calc.zbmi2007(mat,bfawho2007)


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





