#' @title who2007
#'
#' @description
#' WHO 2007
#' Department of Nutrition for Health and Development
#' World Health Organization
#' Last modified on 08/10/2013 - Developed using R version 3.0.1 (2013-05-16)
#' This code concerns the  the calculation of prevalences using all vallid z-scores (non-missing)
#' for three indicators: weight-for-age (5 to 10 years), height-for-age (5 to 19 years) and BMI-for-age (5 to 19 years) based on the WHO 2007 references.
#' Exact age must be given in months (no rounding necessary), height in centimeters and weight in kilograms.

######################################################################################
### Function for calculating individual height-for-age z-scores
######################################################################################

calc.zhfa<-function(mat,hfawho2007){

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

calc.zwei<-function(mat,wfawho2007){

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

calc.zbmi<-function(mat,bfawho2007){

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

  sex.x<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(sex))])
  age.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(age))])
  weight.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(weight))])
  height.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(height))])
  if(!missing(oedema)) oedema.vec<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(oedema))]) else oedema.vec<-oedema
  if(!missing(sw))	sw<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(sw))])	else sw<-as.double(sw)
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

mat<-calc.zhfa(mat,hfawho2007)

### Weight-for-age z-score

mat<-calc.zwei(mat,wfawho2007)

### BMI-for-age z-score

mat<-calc.zbmi(mat,bfawho2007)


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
