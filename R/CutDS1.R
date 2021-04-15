#'
#' @title Checks for disclosure problems for 
#' converting a numeric vector into a factor with intervals (CutDS2)
#' @description This function is an aggregate DataSHIELD function that returns 
#' descriptions of the failed disclosure checks.
#' @details Checks for number of levels, minima and maxima of categories, and 
#' number of individuals at breaks.
#' @param input.var.name a character string which provides 
#' the name of the variable to be converted to a factor. 
#' @param breaks a numeric vector of  unique cut points, if length is 1, boarders will be added.
#' @param dig.lab integer which demermines the numer of digits to be rounded to before categorization.
#' Default is 0.
#' @param var.min minimal value of all categories. Default: -99999999
#' @param var.max maximal value of all categories. Default: 99999999
#' @return an object of class list
#' @export
#'
cutDS1 <- function(input.var.name=NULL,  
                  breaks = NULL, dig.lab = 0,
                  var.min = -99999999, var.max = 99999999){
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS                     #
  thr <- listDisclosureSettingsDS()                           #
  nfilter.tab <- as.numeric(thr$nfilter.tab)                  #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                 #
  #nfilter.noise <- as.numeric(thr$nfilter.noise)             #
  nfilter.levels <- as.numeric(thr$nfilter.levels)            #
  #############################################################
  
  # Round input variable to check if number of individuals at break is enough
  input.var <- round(eval(parse(text=input.var.name), envir = parent.frame()), dig.lab)
  
  error.message <- list()
  
  ########################################################################
  #################MODULE CHECKING DISCLOSURE RULES#######################
  ########################################################################
  
  # CHECK IF MINIMA AND MAXIMA ARE WITHIN THE CATEGORIES
  if((var.min>min(input.var) | var.max<max(input.var))){
    error.message[[length(error.message)+1]]<-
      paste0("FAILED: the minimum value and/or the maximum value for the breaks
              is higher / lower than the value in the continuous variable. Please adjust those values.")
  }
  
  
  # CHECK IF THE NUMBER OF LEVELS IS LOWER THAN THE THRESHOLD OF MAXIMUM NUMBER OF LEVELS
  num.levels<-length(breaks)-1
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
    if(sum(input.var == breaks[j]) < min.allowed.atcut){
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
  
  return(error.message)
}
#AGGREGATE FUNCTION
# CutDS1