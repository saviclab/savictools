
# The following functios are copied from MIFuns and metrumrg, which were removed
# from CRAN..
# Author: Tim Bergsma, Metrum Institute

reapply <- function(x, INDEX, FUN, ...)
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


safe.call <- function(what,...){
  extras <- list(...)
  legal <- names(formals(what))
  extras <- extras[names(extras) %in% legal]
  do.call(what=what,args=extras)
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
  if(horizontal)lattice::panel.polygon(x=data$x,y=data$y,border=border,col=col,...)
  else          lattice::panel.polygon(x=data$y,y=data$x,border=border,col=col,...)
}
panel.ref <- function(x,y,col='grey90',horizontal,rlim,...){
  x <- as.numeric(x)
  y <- as.numeric(y)
  if(horizontal)lattice::panel.rect(xleft=rlim[1],ybottom=0,xright=rlim[2],ytop=max(y) + 1,border='transparent',col=col)
  else lattice::panel.rect(xleft=0,ybottom=rlim[1],xright=max(x) + 1, ytop=rlim[2],border='transparent',
                  col=col)
}

bin <-
  function(
    x,
    population=x,
    breaks=quantile(
      population,
      probs=probs,
      ...
    ),
    probs=c(0,0.25,0.5,0.75,1),
    include.lowest=TRUE,
    ...
  )table(
    cut(
      x,
      breaks=breaks,
      include.lowest=include.lowest,
      ...
    )
  )

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
  if(horizontal)lattice::ltext(
    x=midpoints,
    y=rep(level,length(midpoints)),
    labels=value,
    cex=cex,
    col=text,
    ...
  )
  else lattice::ltext(
    y=midpoints,
    x=rep(level,length(midpoints)),
    labels=value,
    cex=cex,
    col=text,
    ...
  )
}
panel.stratify <-
  function(
    x,
    y,
    type = "p",
    groups = NULL,
    pch = if (is.null(groups)) plot.symbol$pch else superpose.symbol$pch,
    col,
    col.line = if (is.null(groups)) plot.line$col else superpose.line$col,
    col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col,
    font = if (is.null(groups)) plot.symbol$font else superpose.symbol$font,
    fontfamily = if (is.null(groups)) plot.symbol$fontfamily else superpose.symbol$fontfamily,
    fontface = if (is.null(groups)) plot.symbol$fontface else superpose.symbol$fontface,
    lty = if (is.null(groups)) plot.line$lty else superpose.line$lty,
    cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex,
    fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill,
    lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd,
    horizontal = FALSE,
    panel.levels='panel.xyplot',
    ...,
    jitter.x = FALSE,
    jitter.y = FALSE,
    factor = 0.5,
    amount = NULL
  ){
    if (all(is.na(x) | is.na(y)))
      return()
    x <- as.numeric(x)
    y <- as.numeric(y)
    plot.symbol <- lattice::trellis.par.get("plot.symbol")
    plot.line <- lattice::trellis.par.get("plot.line")
    superpose.symbol <- lattice::trellis.par.get("superpose.symbol")
    superpose.line <- lattice::trellis.par.get("superpose.line")
    if (!missing(col)) {
      if (missing(col.line))
        col.line <- col
      if (missing(col.symbol))
        col.symbol <- col
    }
    if (!is.null(groups))
      panel.superpose(x, y, type = type, groups = groups, pch = pch,
                      col.line = col.line, col.symbol = col.symbol, font = font,
                      fontfamily = fontfamily, fontface = fontface, lty = lty,
                      cex = cex, fill = fill, lwd = lwd,horizontal = horizontal,
                      panel.groups = panel.stratify, jitter.x = jitter.x,
                      jitter.y = jitter.y, factor = factor, amount = amount,
                      panel.levels=panel.levels,...
      )
    else{
      levels <- if(horizontal) y else x
      x <- split(x,levels)
      y <- split(y,levels)
      panel.levels <- if (is.function(panel.levels)) panel.levels
      else if (is.character(panel.levels)) get(panel.levels)
      else eval(panel.levels)
      for (level in unique(levels)) {
        panel.levels(
          x[[as.character(level)]],
          y[[as.character(level)]],
          type = type,
          pch = pch,
          col = col,
          col.line = col.line,
          col.symbol = col.symbol,
          font = font,
          fontfamily = fontfamily,
          fontface = fontface,
          lty = lty,
          cex = cex,
          fill = fill,
          lwd = lwd,
          horizontal = horizontal,
          jitter.x = jitter.x,
          jitter.y = jitter.y,
          factor = factor,
          amount = amount,
          level=level,
          levels=levels,
          ...
        )
      }
    }
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
  do.call(lattice::panel.abline,args)
  if(horizontal)args <- list(v=ref,...)else args<-list(h=ref,...)
  do.call(lattice::panel.abline,args)
}


