#' Title
#'
#' @param data
#' @param fitFunction
#' @param startList
#' @param lowerBounds
#' @param upperBounds
#' @param min_Factor
#' @param n_iterations
#' @param n_runs_max
#' @param n_runs_min
#'
#' @return Describe here what the function returns.
#' @export
#'
#' @examples
#' # add usage examples here
fit_main <-
function(data,
                   fitFunction,
                   startList,
                   lowerBounds,
                   upperBounds,
                   min_Factor,
                   n_iterations,
                   n_runs_max,
                   n_runs_min)
  {

  count01<-1
  print(count01)
  count01 <- count01 + 1


  counter02a<-0
  counter02b<-0
  flag<-0
  storage<-NULL

  while(counter02a < n_runs_max & counter02b < n_runs_min){
    counter02a<-counter02a+1;
    m01<-try(fit_S1(data,
                  fitFunction,
                  startList,
                  lowerBounds,
                  upperBounds,
                  min_Factor,
                  n_iterations),silent = FALSE)


      nameList<-names(startList)
      startList<-runif(length(startList), 0, 1)*(upperBounds-lowerBounds)+lowerBounds;
      names(startList) <- nameList
      startList<-as.list(startList)
     # change startList

    if(class(m01)!="try-error"){
      flag<-1;
      counter02b<-counter02b+1;
      if (is.null(storage)==1){storage<-m01}
      else {if(m01$residual_Sum_of_Squares < storage$residual_Sum_of_Squares){storage<-m01}}
    }
  }

  m01<-storage
  if(flag==0){m01<-fit_noSol(startList)}  # run "no solution function"
                                          # if while loop does not end
                                          # up with a good explanation


  m01$n_runs_actual <-counter02a
  m01$n_runs_meaningful<-counter02b
  m01$minGFP <-min(data$GFP)
  m01$maxGFP <-max(data$GFP)


#   m01$residual_Sum_of_Squares <- as.character(m01$residual_Sum_of_Squares)
#   m01$log_likelihood <- as.character(m01$log_likelihood)
#   m01$AIC_value <- as.character(m01$AIC_value)
#   m01$BIC_value <- as.character(m01$BIC_value)

  print(m01)
  if(!is.numeric(m01$residual_Sum_of_Squares)){browser()}
  if(!is.numeric(m01$log_likelihood)){browser()}
  if(!is.numeric(m01$AIC_value)){browser()}
  if(!is.numeric(m01$BIC_value)){browser()}


  return(m01)
  }
