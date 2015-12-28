#' @title Counter function
#'
#' @param data Normalized input data that will be fitted transferred into related functions
#' @param fitFunction type of fit function that will be used. Can be "linear", "sigmoidal", "double_sigmoidal", "test"
#' @param n_runs_max number of maximum runs that the algorithm can run
#' @param n_runs_min number of minimum runs that the algorithm can run
#'
#' @details the algorithm calls the fitting algorithms. to make the fits with random initial parameters. This multiple runs are necessary to avoid local minimums that LM fits can stuck. Fitting algorithms can either gives a fit with related parameters and isThisaFit=TRUE value or just give isThisaFit=FALSE corresponding to not a fit. n_runs_min represent minimum number of fits that are necessary to give a result, n_runs_max limits the number of runs (successful or unsuccessful) that the it algorithm can run
#' @return The function returns the parameters related with fitted curve to input data
#' @export
#'
#' @examples
#' # add usage examples here
counterFunction <-
  function(data,fitFunction, n_runs_min, n_runs_max, ...)
  {
    if(!(fitFunction %in% c("linear", "sigmoidal", "double_sigmoidal", "test")) )
    {stop("fitFunction should be one of linear, sigmoidal, double_sigmoidal, test")}
    
    counterBetterFit=0
    counterCorrectFit=0
    counterTotalFit=0
    residual_Sum_of_Squares_min=Inf
    storedFitFunctionOutput=list()
    storedFitFunctionOutput$residual_Sum_of_Squares=Inf
 
    while(counterCorrectFit<n_runs_min & counterTotalFit<n_runs_max)
    {
      
      
      
      if(fitFunction == "test"){fitFunctionOutput=exampleFitFunction(randomParameter)}
      
      if(fitFunctionOutput[[1]]){
        counterCorrectFit=counterCorrectFit+1
        if(residual_Sum_of_Squares_min>fitFunctionOutput$residual_Sum_of_Squares){
          counterBetterFit=counterBetterFit+1
          residual_Sum_of_Squares_min=fitFunctionOutput$residual_Sum_of_Squares
          storedFitFunctionOutput=fitFunctionOutput
        }
      }
      
      counterTotalFit=counterTotalFit+1
      
      print(c(counterBetterFit,
              counterCorrectFit,
              counterTotalFit,
              storedFitFunctionOutput$residual_Sum_of_Squares))
      
    }
    return(storedFitFunctionOutput)
  }

#' exampleFitFunction
#'
#' @param randomParameter This parameter defines the probability that the exampleFitFunction returns TRUE values for isThisaFit parameter
#' @details This is the exampleFitFunction that generates TRUE values for isThisaFit parameter whit given probability
#' @return The function returns TRUE or FALSE for isThisaFit parameter and also residual_Sum_of_Squares parameter that determines the goodness of the fit
#'
#' @examples
#' # add usage examples here
exampleFitFunction<-
  function(randomParameter)
  {
    randomNumber=runif(1, 0, 1)
    if(randomNumber<randomParameter){isThisaFit=TRUE; residual_Sum_of_Squares=runif(1, 0, 1)}
    if(randomNumber>randomParameter){isThisaFit=FALSE; residual_Sum_of_Squares=NA}
    outputList=list(isThisaFit=isThisaFit,
                    randomParameter=randomParameter,
                    residual_Sum_of_Squares=residual_Sum_of_Squares)
    return(outputList)
  }
