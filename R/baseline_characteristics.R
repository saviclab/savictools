#' @title: Baseline characteristics table
#' @author Kendra Radtke
# output: word_document

#pull in data
dat <- read.csv('../../data/parsed/MoxifloxacinPKData_2020Aug7_DT_imp.csv', stringsAsFactors = F)
unique <- dat %>% mutate(DOSE = ifelse(EVID==1, AMT, NA)) %>%
  group_by(ID) %>% fill(DOSE) %>% mutate(DOSEMGKG=DOSE/WEIGHT) %>%
  dplyr::filter(EVID==0) %>% distinct(ID,.keep_all=T) %>% ungroup() %>%
  # transform ART_REG
  mutate(ART_REG = ifelse(ART_REG==0, 'No ART',
                          ifelse(ART_REG==1 | ART_REG==2, 'LPV/r','EFV')))
unique.occ <- dat %>% dplyr::filter(EVID==0) %>% distinct(ID,PKOCC,.keep_all=T)
# replace -99 with NA
unique <- na_if(unique,-99)
unique.occ <- na_if(unique.occ,-99)
colnames(unique)
cat.var <- c('ROUTE','FORMULATION','HIV','ART_REG','GENDER') # categorical variables
con.var <- c('WEIGHT','HEIGHT','DOSEMGKG','WAZW','HAZW','BAZW','AGE') # continuous variables

# Continuous covariates:

con_table <- function(x, var){
  arm1 <- x[x$ARM==1,]
  tab.1 <- data.frame('Median' = median(unlist(arm1[,var]), na.rm=TRUE),
                      'min' = min(arm1[,var], na.rm=TRUE),
                      'max' = max(arm1[,var], na.rm=TRUE))
  arm2 <- x[x$ARM==2,]
  tab.2 <- data.frame('Median' = median(unlist(arm2[,var]), na.rm=TRUE),
                      'min' = min(arm2[,var], na.rm=TRUE),
                      'max' = max(arm2[,var], na.rm=TRUE))

  tab.all <- data.frame('Median' = median(unlist(x[,var]), na.rm=TRUE),
                        'min' = min(x[,var], na.rm=TRUE),
                        'max' = max(x[,var], na.rm=TRUE))
  tab.combined <- rbind(tab.1,tab.2,tab.all)
  tab.combined$Median <- signif(tab.combined$Median,3)
  tab.combined$min <- signif(tab.combined$min,3)
  tab.combined$max <- signif(tab.combined$max,3)
  tab.combined$Group = c('MDR-PK1','MDR-PK2','Total')
  return(tab.combined)
}

# Table Output:

full.table <- data.frame()
con.anal <- dplyr::select(unique, con.var, ARM)
for(j in 1:length(con.var)){
  t <- con_table(con.anal,var=j)
  t$Variable <- con.var[j]
  full.table <- rbind(full.table,t)
}

# Formatted table:

format.tab <- full.table %>%
  unite(RANGE, c(min,max), sep=' to ') %>%
  unite(MED_RANGE, c(Median,RANGE), sep=' (')%>%
  mutate(MED_RANGE = paste(MED_RANGE, ')',sep='')) %>%
  pivot_wider(names_from=Group, values_from=MED_RANGE)

# Stats:

stat_con <- function(x, var){
  arm1 <- x[x$ARM==1,]
  arm2 <- x[x$ARM==2,]
  res<- wilcox.test(unlist(arm1[,var]),unlist(arm2[,var]))
  return(res$p.value)
}

stats <- data.frame()
for(j in 1:length(con.var)){
  p <- stat_con(con.anal,var=j)
  p <- as.data.frame(p)
  p$Variable <- con.var[j]
  stats <- rbind(stats,p)
}
format.tab.p <- left_join(format.tab,stats,by='Variable')
format.tab.p$p <- signif(format.tab.p$p,3)

# Categorical covariates:

cat_table <- function(x, var){
  varx <- colnames(x)[var]
  if(varx=='ART_REG'){
    x <- filter(x, ART_REG!='No ART')
  } else {
    x <- x
  }
  var1 <- sym(varx)
  arm1 <- x[x$ARM==1,]
  n.arm1 <- count(arm1,!!var1) %>%
    mutate(p = round(n/sum(n)*100,1),
           p = paste0('(',paste0(p,')'))) %>%
    unite(n_p, c(n,p),sep=' ') %>%
    mutate(Label = paste(names(arm1[var]), !!var1, sep=': '))
  n.arm1 <- n.arm1[,2:3]
  n.arm1$Group <- 'MDR-PK1'

  arm2 <- x[x$ARM==2,]
  n.arm2 <- count(arm2,!!var1) %>%
    mutate(p = round(n/sum(n)*100,1),
           p = paste0('(',paste0(p,')'))) %>%
    unite(n_p, c(n,p),sep=' ') %>%
    mutate(Label = paste(names(arm2[var]), !!var1, sep=': '))
  n.arm2 <- n.arm2[,2:3]
  n.arm2$Group <- 'MDR-PK2'

  n.all <- count(x,!!var1) %>%
    mutate(p = round(n/sum(n)*100,1),
           p = paste0('(',paste0(p,')'))) %>%
    unite(n_p, c(n,p),sep=' ') %>%
    mutate(Label = paste(names(x[var]), !!var1, sep=': '))
  n.all <- n.all[,2:3]
  n.all$Group <- 'Total'

  tab.combined <- rbind(n.arm1,n.arm2,n.all)
  return(tab.combined)
}

# Table Output:

# do not differ by occasion
cat.table <- data.frame()
cat.anal <- dplyr::select(unique, cat.var, ARM)
for(j in 1:length(cat.var)){
  t <- cat_table(cat.anal,var=j)
  t$Variable <- names(cat.anal[j])
  cat.table <- rbind(cat.table,t)
}
# remove route, form
cat.table <- dplyr::filter(cat.table, substr(Label,0,4) != 'FORM', substr(Label,0,4) != 'ROUT')
# add route, form -->differ by occasion
cat.anal.occ <- dplyr::select(unique.occ, cat.var[1:2], ARM)
for(j in 1:length(colnames(cat.anal.occ))){
  t <- cat_table(cat.anal.occ,var=j)
  t$Variable <- names(cat.anal[j])
  cat.table <- rbind(cat.table,t)
}
# remove ARM
cat.table <- dplyr::filter(cat.table, substr(Label,0,3) != 'ARM')

# Formatted table:

format.cat <- cat.table %>%
  pivot_wider(names_from=Group, values_from=n_p)

# Stats:

stat_cat <- function(x, var){
  test<- chisq.test(table(x$varx, x$ARM))
  return(test$statistic)
}


stats <- data.frame()
for(j in 3:length(cat.var)){
  x <- table(cat.anal[,c(j,6)])
  if(any(x<5)==TRUE){
    test <- fisher.test(x)
    p<- test$p.value
  } else {
    test<- chisq.test(x)
    p<- test$statistic
  }
  p <- as.data.frame(p)
  p$Variable <- cat.var[j]
  stats <- rbind(stats,p)
}

# route and formulation
for(j in 1:2){
  x <- table(cat.anal.occ[,c(j,3)])
  if(any(x<5)==TRUE){
    test <- fisher.test(x)
    p<- test$p.value
  } else {
    test<- chisq.test(x)
    p<- test$statistic
  }
  p <- as.data.frame(p)
  p$Variable <- cat.var[j]
  stats <- rbind(stats,p)
}
format.cat.p <- left_join(format.cat,stats,by='Variable')
format.cat.p$p <- signif(format.cat.p$p,3)

# Output

write.csv(format.tab.p, '../../produced/bl_tab_cont.csv', row.names = F)
write.csv(format.cat.p, '../../produced/bl_tab_cat.csv', row.names = F)
