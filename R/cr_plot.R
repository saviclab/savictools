#' @title cr_plot
#' @author Thomas





# -----------------------------------------------------------------------------
# Step 2: Using variance-covariance matrix form nonmem output sample parameters
# Toal number of thetas will depend on the number of covariates in the full model
# Make sure indexing and referencing of the parameters is appropriate for your model
# (eg. the 1:9 - corresponds to 9 thetas in the full model,including covariates effects
#  			look varCov dataframe)
# -----------------------------------------------------------------------------

# The following functions are copied from MIFuns, which is not supported in
# the current R version.
# author: Tim Bergsma, Metrum Institute
reapply <- function (x, INDEX, FUN, ...)
{
  if(!is.list(INDEX)) INDEX <- list(INDEX)
  INDEX <- lapply(INDEX,function(x)as.integer(factor(x)))
  INDEX <- as.integer(do.call(interaction,c(INDEX,drop=TRUE)))
  form <- tapply(x, INDEX)
  calc <- tapply(x, INDEX, FUN, ...,simplify=FALSE)
  need <- table(form)
  calc <- lapply(
    seq_along(calc),
    function(cell)rep(
      calc[[cell]],
      length.out=need[[
        as.character(cell)
        ]]
    )
  )
  calc <- c(calc,list(rep(NA,sum(is.na(form)))))
  form[is.na(form)] <- length(calc)
  grps <- split(form,form)
  grps <- lapply(
    grps,
    seq_along
  )
  elem <- unsplit(grps,form)
  sapply(
    seq_along(form),
    function(i)calc[[
      form[[i]]
      ]][[
        elem[[i]]
        ]]
  )
}

unitDensity <- function(x,...){
  res <- safe.call(density.default,x=x,...)
  res$y <- with(res, y/max(y,na.rm=TRUE))
  res
}

panel.densitystrip <- function(x,y,horizontal,col.line,fill,factor,border=col.line,col=fill,...){
  ordinal <- if(horizontal) x else y
  level <- if(horizontal) unique(y)[[1]] else unique(x)[[1]]
  data <- unitDensity(ordinal,...)
  data$y <- data$y * factor + level
  if(missing(col))col <- fill
  if(is.na(col))col <- fill
  if(horizontal)panel.polygon(x=data$x,y=data$y,border=border,col=col,...)
  else          panel.polygon(x=data$y,y=data$x,border=border,col=col,...)
}

panel.ref <- function(x,y,col='grey90',horizontal,rlim,...){
  x <- as.numeric(x)
  y <- as.numeric(y)
  if(horizontal)panel.rect(xleft=rlim[1],ybottom=0,xright=rlim[2],ytop=max(y) + 1,border='transparent',col=col)
  else panel.rect(xleft=0,ybottom=rlim[1],xright=max(x) + 1, ytop=rlim[2],border='transparent',
                  col=col)
}

panel.cuts <- function(
  x,
  y,
  cuts,
  col,
  col.line,
  text=col.line,
  horizontal=TRUE,
  offset=-0.2,
  increment=0,
  format=function(x,...)as.numeric(round(x/sum(x)*100)),
  include.range=TRUE,
  zero.rm=TRUE,
  cex=0.7,
  ...
){
  ordinal <- if(horizontal) x else y
  level <- if(horizontal) unique(y)[[1]] else unique(x)[[1]]
  cuts <- cuts[cuts >= min(ordinal,na.rm=TRUE) & cuts <= max(ordinal,na.rm=TRUE)]
  if(include.range) cuts <- c(range(ordinal),cuts)
  cuts <- sort(unique(cuts))
  midpoints <- (cuts[1:length(cuts)-1] + cuts[-1])/2
  count <- bin(ordinal,breaks=cuts,...)
  value <- format(count,...)
  if(zero.rm)value[value==0] <- NA
  value <- as.character(value)
  value[is.na(value)] <- ''
  level <- level + offset
  midpoints <- midpoints + increment
  if(horizontal)ltext(
    x=midpoints,
    y=rep(level,length(midpoints)),
    labels=value,
    cex=cex,
    col=text,
    ...
  )
  else ltext(
    y=midpoints,
    x=rep(level,length(midpoints)),
    labels=value,
    cex=cex,
    col=text,
    ...
  )
}

panel.covplot <- function(
  x,
  y,
  ref=1,
  rlim=ref * c(0.75,1.25),
  cuts=ref * c(0.75,1,1.25),
  horizontal=TRUE,
  border='black',
  fill='grey',
  text='black',
  shade='grey90',
  col='white',
  ...
){
  x <- as.numeric(x)
  y <- as.numeric(y)
  panel.ref(x,y,rlim=rlim,horizontal=horizontal,col=shade,...)
  panel.stratify(panel.levels=panel.densitystrip,  x=x,y=y,horizontal=horizontal,border=border,col=fill,...)
  panel.stratify(panel.levels=panel.cuts,          x=x,y=y,horizontal=horizontal,cuts=cuts,    text=text,...)
  if(horizontal)args <- list(v=cuts,col=col,...)else args<-list(h=cuts,col=col,...)
  do.call(panel.abline,args)
  if(horizontal)args <- list(v=ref,...)else args<-list(h=ref,...)
  do.call(panel.abline,args)
}




cr_plot <- function(x) {
# Read in the variance-covariance matrix from the appropriate .cov file (ex:"run2.cov")
# run002: 1-comp
varCov <- read.table(x,skip=1,as.is=T,header=T)
theta.cov <- varCov[1:6,c(2:7)]

boot <- MASS::mvrnorm(n=1000, mu=c(3.67E-01, 0.00E+00,  8.23E-01,  1.19E+02,  8.52E+00,  1.10E+00), Sigma=theta.cov)
boot <- data.frame(boot[,c(3:6)])
names(boot) <- c("KA","V","CL","VWT")

# boot <- within(boot, BA <- BA/median(BA))
boot <- within(boot, VWT_5 <- (46.92/57.9)^VWT) # within is base R and is mainly for interactive use
boot <- within(boot, VWT_95 <- (79.015/57.9)^VWT)

boot$KA <- NULL
boot$V <- NULL
boot$CL <- NULL
boot$VWT <- NULL

boot <- reshape::melt(rev(boot))

boot <- boot[
  with(
    boot,
    value >= reapply(value,variable,quantile,0.05) &
      value <= reapply(value,variable,quantile,0.95)
  ),
]

pl1 <- lattice::stripplot(
  variable~value,
  boot,
  panel=panel.covplot,
  rlim=c(0.80,1.20),
  xlim=c(0,2),
  cuts=c(0.80,1,1.20),
  xlab='Change in parameter value relative to reference',
  shade='skyblue'
)

pl1
}
