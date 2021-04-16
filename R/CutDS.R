#'
#' @title Converts a numeric vector into a factor with intervals
#' @description This function is an assign DataSHIELD function that converts a numeric vector into
#' a factor type with levels as intervals of the numeric values.
#' @details Divides the range of x into intervals and codes the values in x 
#' according to which interval they fall. 
#' The leftmost interval corresponds to level one, 
#' the next leftmost to level two and so on. 
#' The inclusion of the breaks in the lower or upper intervall 
#' can be changed withg \code{right}.
#' The function implements the base R function \code{cut}
#' @param input.var.name a character string which provides 
#' the name of the variable to be converted to a factor. 
#' @param breaks.transmit a numeric vector of  unique cut points, if length is 1, boarders will be added.
#' @param labels.transmit labels for the levels of the resulting category. 
#' By default (NULL), labels are constructed using "(a,b]" 
#' (brackets interchanged when right=FALSE) interval notation. 
#' If labels = FALSE, simple integer codes are returned instead of a factor.
#' @param right logical, indicating if the intervals should be closed on 
#' the right and open on the left (default,  \code{breaks}=TRUE) or vice versa.
#' @param dig.lab integer which demermines the numer of digits to be rounded to before categorization.
#' It is also used when labels are not given and then determines the number of 
#' digits used in formatting the break numbers.
#' Default is 0.
#' @param ordered_results logical, should the result be an ordered factor?
#' Default is FALSE
#' @param var.min minimal value of all categories. Default: -99999999
#' @param var.max maximal value of all categories. Default: 99999999
#' @return an object of class factor
#' @export
#'
cutDS <- function(input.var.name,  
                   breaks.transmit = NULL, labels.transmit = NULL,
                   right = TRUE, dig.lab = 3,
                   ordered_result = FALSE,
                   var.min = -99999999, var.max = 9999999){
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS                     #
  #thr <- listDisclosureSettingsDS()                           #
  nfilter.tab <- 5#as.numeric(thr$nfilter.tab)                  #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                 #
  #nfilter.noise <- as.numeric(thr$nfilter.noise)             #
  nfilter.levels <- 5#as.numeric(thr$nfilter.levels)            #
  #############################################################
  
  # Round input variable to check if number of individuals at break is enough
  input.var <- round(eval(parse(text=input.var.name,), envir = parent.frame()), dig.lab)
  
  
  breaks <- as.numeric(unlist(strsplit(breaks.transmit, split=",")))
  labels <- unlist(strsplit(labels.transmit, split=","))
  
  error.message <- list()
  
  ########################################################################
  #################MODULE CHECKING DISCLOSURE RULES#######################
  ########################################################################
  
  # CHECK IF MINIMA AND MAXIMA ARE WITHIN THE CATEGORIES
  if((var.min>min(input.var,na.rm = TRUE) | var.max<max(input.var,na.rm=TRUE))){
    error.message[[length(error.message)+1]]<-
      paste0("FAILED: the minimum value and/or the maximum value for the breaks
              is higher / lower than the value in the continuous variable. Please adjust those values.")
  }
  
  
  # CHECK IF THE NUMBER OF LEVELS IS LOWER THAN THE THRESHOLD OF MAXIMUM NUMBER OF LEVELS
  num.levels<-length(breaks)+1
  max.allowed.levels<-length(input.var)*nfilter.levels
  
  if(num.levels>max.allowed.levels){
    error.message[[length(error.message)+1]]<-
      paste0("FAILED: this variable has too many levels and may be disclosive. The ds.cut function allows no more than ",
             max.allowed.levels," levels in this particular study. This variable would have ",num.levels)
  }
  
  # CHECK IF THE NUMBER OF INDIVUALS AT THE BREAKPOINTS IS HIGH ENOUGH
  min.allowed.atcut <- nfilter.tab #Maybe multiply with factor?
  breaks_check <- rep(FALSE,length(breaks))
  for(j in 1:length(breaks)){
    if(sum(input.var == breaks[j], na.rm = TRUE) < min.allowed.atcut){
      breaks_check <- TRUE
    }
  }
  
  
  if(any(breaks_check)){
    break_prob <- which(breaks_check)
    error.message[[length(error.message)+1]]<-
      paste0("FAILED: there are not enough observations at one or more break points. 
             The problematic ones are at entry number(s) ",
             break_prob," of the entered break vector.")
  }
  
  
  ########################################################################
  ###############MODULE CHECKING DISCLOSURE RULES END#####################
  ########################################################################
  
  if(length(error.message == 0)){ #No errors => Categorization will be performed 
    breaks <- c(var.min,breaks,var.max)
    factor.obj <- cut(input.var, breaks = breaks, labels = labels,
                      include.lowest = TRUE, right = right, dig.lab = dig.lab,
                      ordered_result = ordered_result)
  } else {
    return(list(error.message=as.character(error.message)))
  }
  
  return(factor.obj)
  
  
}
#ASSIGN FUNCTION
# CutDS