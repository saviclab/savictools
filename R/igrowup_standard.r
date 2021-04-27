#' @title who2007
#'
#' @description
#' WHO 2007
#' Department of Nutrition for Health and Development
#' World Health Organization
#' Last modified on 08/10/2013 - Developed using R version 3.0.1 (2013-05-16)
#' This code concerns the standard approach for the prevalences, i.e. the calculation of the prevalences takes into account all the valid (non-missing) z-scores for each of the indicators.

##################################################################################################################
#########  Function for calculating the z-scores and prevalences for a nutritional survey                    #####
##################################################################################################################

######################################################################################
#### roundeing function - SPlus roundeing function uses the nearest even number rule
######################################################################################

rounde <- function(x,digits=0) { # function not behaving properly
 expo<-10^digits
 return(ifelse(abs(x*expo) - floor(abs(x*expo)) < 0.5, sign(x*expo) * floor(abs(x*expo)), sign(x*expo) * (floor(abs(x*expo)) + 1))/expo)
}

######################################################################################
### Function for calculating individual Length-for-age z-scores
######################################################################################

calc.zlen<-function(mat,lenanthro){

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

   sex.x<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(sex))])
   age.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(age))])
   if(!missing(weight)) weight.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(weight))]) else weight.x<-as.double(weight)
   if(!missing(lenhei)) lenhei.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(lenhei))]) else lenhei.x<-as.double(lenhei)
   if(!missing(headc)) headc.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(headc))]) else headc.x<-as.double(headc)
   if(!missing(armc)) armc.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(armc))]) else armc.x<-as.double(armc)
   if(!missing(triskin)) triskin.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(triskin))]) else triskin.x<-as.double(triskin)
   if(!missing(subskin)) subskin.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(subskin))]) else subskin.x<-as.double(subskin)
   if(!missing(measure)) lorh.vec<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(measure))]) else lorh.vec<-as.character(measure)
   if(!missing(oedema)) oedema.vec<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(oedema))]) else oedema.vec<-oedema
   if(!missing(sw))	sw<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(sw))])	else sw<-as.double(sw)
   sw<-ifelse(is.na(sw),0,sw)

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



