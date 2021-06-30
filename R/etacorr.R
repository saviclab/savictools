
#' @title Plotting eta correlation for continuous and categorical variables
#' @author Kendra Radtke, Dhruv Vaish, Alexander Floren
#'
#' @description Function that can be using to check the ditributions of eta against certain variables.
#' Properly coded models will have normal distributions with mean zero.
#'
#' @param data Either a xpose compliant run number, nonmem data in the form of a dataframe,
#' or string representing the path to a nonmem tablefile
#' @param eta numeric vector of the desires etas
#' @param ... unpacked, arbitrry number of symbols which represent desired covariates to plot
#' @param cat Default = FALSE. Set to TRUE if plotting a categorical variable. Default set to plot
#' continuous variables
#'
#' @return Outputs plot and returns ggplot object
#'
#' @examples
#' etacorr(data, c(1, 2), WT, HT, AGE)
#' # Shows a scatterplot of WT and HT against ETA1 and ETA2 continuously.
#'
#' etacorr("~/Documents/run5/simtab", c(1, 2), HIV, cat = TRUE)
#' # Shows a boxplot of the associated ETA1 and ETA2 with those with HIV (1) and those without (0)
#'
#' @export
etacorr <- function(data, eta, ..., cat = FALSE) {

  if (is.numeric(data)) {
    xpdb <- xpose::get_data(data)
    data <- xpdb$data$data[[1]]
  } else {
    if (!is.data.frame(data)) {
      data <- xpose::read_nm_tables(data)
    }
  }

  etas <- purrr::map(as.character(eta), function(num) { sym(paste0("ETA", num)) }) # dplyr compatible format
  ss <- data[!duplicated(data$ID),] # 1 obs per ind
  vars <- sapply(enquos(...), rlang::quo_text)

  for (eta in etas) {
    plots <- list() # create blank
    for(i in 1:length(vars)) {
      var.select <- vars[i]
      var.select <- sym(var.select) # dplyr compatible format
      if (!cat) {
        plots[[i]] <- ggplot(ss, aes(x=!!var.select, y=!!eta))+geom_point(shape=21)+geom_smooth(se=F)+
          geom_smooth(method='lm',se=F,linetype='dashed',color='red')
      } else {
        plots[[i]] <- ggplot(ss, aes(x=as.factor(!!var.select), y=!!eta))+
          geom_boxplot(outlier.colour = NA)
      }

    }
    do.call(gridExtra::grid.arrange,plots)
  }
}
