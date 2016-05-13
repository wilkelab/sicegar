#' @title fit function
#'
#' @param dataInput Normalized input data that will be fitted transferred into related functions
#' @param model type of fit function that will be used. Can be "linear", "sigmoidal", "double_sigmoidal", "test"
#' @param n_runs_max number of maximum runs that the algorithm can run
#' @param n_runs_min number of minimum runs that the algorithm can run
#' @param showDetails If set to True (default is false) prints details of intermediate steps of individual fits
#' @param dataInputName is the input name container with a default of 'NA'. The functions will be used with massive for loops so put a personal data indicator might be a good idea.
#' @param randomParameter is a paramter that is needed to run the test model. Default is 'NA'
#' @param ... all other arguments that model functions ("exampleFitFunction", "lineFitFunction", "sigmoidalFitFunction", "doublesigmoidalFitFunction") may need
#'
#' @description The algorithm calls the fitting algorithms. to make the fits with random initial parameters. This multiple runs are necessary to avoid local minimums that LM fits can stuck. Fitting algorithms can either gives a fit with related parameters and isThisaFit=TRUE value or just give isThisaFit=FALSE corresponding to not a fit. n_runs_min represent minimum number of fits that are necessary to give a result, n_runs_max limits the number of runs (successful or unsuccessful) that the it algorithm can run
#' @return The function returns the parameters related with fitted curve to input data
#' @export
#'
#' @examples
#' # Example 1 (test function without normalization)
#' # data sent to algorithm directly as data frame
#' # a- Generate data
#' time = seq(3,48,0.5)
#' intensity=stats::runif(length(time), 3.0, 7.5)
#' dataInput = data.frame(time,intensity)
#' # b- generate "random Parameter" for model "test"
#' randomParameterValue=0.7 # it should be a parameter between 0 and 1
#' # c- use the function "test"
#' parameterOutput=fitFunction(dataInput=dataInput,
#'                             model="test",
#'                             n_runs_min=5,
#'                             n_runs_max=15,
#'                             randomParameter=randomParameterValue)
#'
#' # Example 2 (test function with normalization)
#' # data sent to algorithm after normalization
#' # a- Generate data
#' time = seq(3,48,0.5)
#' intensity=stats::runif(length(time), 3.0, 7.5)
#' dataInput = data.frame(time,intensity)
#' # b- normalize data
#' dataOutput = normalizeData(dataInput)
#' # c- generate "random Parameter" for model "test"
#' randomParameter=0.7 # it should be a parameter between 0 and 1
#' # d- use the function "test"
#' dataInput2=dataOutput
#' parameterOutput=fitFunction(dataInput=dataInput2,
#'                             model="test",
#'                             n_runs_min=5,
#'                             n_runs_max=15,
#'                             randomParameter=randomParameterValue)
#'
#' # Example 3 (linear function without normalization)
#' # data sent to algorithm directly as data frame
#' # a- Generate data
#' time = seq(3,48,0.5)
#' intensity=stats::runif(length(time), 3.0, 7.5)
#' dataInput = data.frame(time,intensity)
#' # b- use the function "linear"
#' parameterOutput=fitFunction(dataInput=dataInput,
#'                             model="linear",
#'                             n_runs_min=5,
#'                             n_runs_max=15)
#'
#'
#'# Example 4 (linear function with normalization)
#'# Initial Command to Reset the System
#'rm(list = ls())
#'if (is.integer(dev.list())){dev.off()}
#'cat("\014")
#'
#'time=seq(3,24,0.5)
#'
#'#intensity with Noise
#'noise_parameter=20
#'intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity=lineFitFormula(time, slope=4, intersection=-2)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'dataOutput = normalizeData(dataInput)
#'dataInput2=dataOutput
#'parameterVector=fitFunction(dataInput=dataInput2,
#'                            model="linear",
#'                            n_runs_min=5,
#'                            n_runs_max=15)
#'
#'#Check the results
#'if(parameterVector$isThisaFit){
#'  intensityTheoretical=lineFitFormula(time,
#'                                      slope=parameterVector$slope_Estimate,
#'                                      intersection=parameterVector$intersection_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'
#'  print(parameterVector$residual_Sum_of_Squares)
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_line(aes(x=time,y=intensityTheoretical))+
#'    expand_limits(x = 0, y = 0)}
#'
#'if(!parameterVector$isThisaFit){print(parameterVector)}
#'
#'# Example 5 (sigmoidal function with normalization)
#'# Initial Command to Reset the System
#'rm(list = ls())
#'if (is.integer(dev.list())){dev.off()}
#'cat("\014")
#'
#'time=seq(3,24,0.5)
#'
#'#intensity with Noise
#'noise_parameter=2.5
#'intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity=sigmoidalFitFormula(time, maximum=4, slope=1, midPoint=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'dataOutput = normalizeData(dataInput, dataInputName="batch_01_21_2016_samp007623")
#'dataInput2=dataOutput
#'parameterVector=fitFunction(dataInput=dataInput2,
#'                            model="sigmoidal",
#'                            n_runs_min=20,
#'                            n_runs_max=500)
#'
#'#Check the results
#'if(parameterVector$isThisaFit){
#'  intensityTheoretical=sigmoidalFitFormula(time,
#'                                           maximum=parameterVector$maximum_Estimate,
#'                                           slope=parameterVector$slope_Estimate,
#'                                           midPoint=parameterVector$midPoint_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'
#'  print(parameterVector$residual_Sum_of_Squares)
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_line(aes(x=time,y=intensityTheoretical),color="orange")+
#'    expand_limits(x = 0, y = 0)}
#'
#'
#'
#'if(!parameterVector$isThisaFit){print(parameterVector)}
#'
#'
#'
#'# Example 6 (doublesigmoidal function with normalization)
#'# Initial Command to Reset the System
#'rm(list = ls())
#'if (is.integer(dev.list())){dev.off()}
#'cat("\014")
#'
#'time=seq(3,24,0.1)
#'
#'#intensity with Noise
#'noise_parameter=0.2
#'intensity_noise=stats::runif(n = length(time),min = 0,max = 1)*noise_parameter
#'intensity=doublesigmoidalFitFormula(time,
#'                                    finalAsymptoteIntensity=.3,
#'                                    maximum=4,
#'                                    slope1=1,
#'                                    midPoint1=7,
#'                                    slope2=1,
#'                                    midPointDistance=8)
#'intensity=intensity+intensity_noise
#'
#'dataInput=data.frame(intensity=intensity,time=time)
#'dataOutput = normalizeData(dataInput)
#'dataInput2=dataOutput
#'parameterVector=fitFunction(dataInput=dataInput2,
#'                            dataInputName="batch_01_21_2016_samp007623",
#'                            model="doublesigmoidal",
#'                            n_runs_min=20,
#'                            n_runs_max=500,
#'                            showDetails=FALSE)
#'
#'
#'#Check the results
#'if(parameterVector$isThisaFit){
#'  intensityTheoretical=
#'          doublesigmoidalFitFormula(
#'                  time,
#'                  finalAsymptoteIntensity=parameterVector$finalAsymptoteIntensity_Estimate,
#'                  maximum=parameterVector$maximum_Estimate,
#'                  slope1=parameterVector$slope1_Estimate,
#'                  midPoint1=parameterVector$midPoint1_Estimate,
#'                  slope2=parameterVector$slope2_Estimate,
#'                  midPointDistance=parameterVector$midPointDistance_Estimate)
#'
#'  comparisonData=cbind(dataInput,intensityTheoretical)
#'  require(ggplot2)
#'  ggplot(comparisonData)+
#'    geom_point(aes(x=time, y=intensity))+
#'    geom_line(aes(x=time,y=intensityTheoretical),color="orange")+
#'    expand_limits(x = 0, y = 0)}
#'
#'if(!parameterVector$isThisaFit){print(parameterVector)}
fitFunction <-
  function(dataInput,
           dataInputName=NA,
           model,
           n_runs_min,
           n_runs_max,
           showDetails=FALSE,
           randomParameter=NA, ...)
  {
    dataInputCheck=dataCheck(dataInput)

    if(!(model %in% c("linear", "sigmoidal", "doublesigmoidal", "test")) )
    {stop("model should be one of linear, sigmoidal, doublesigmoidal, test")}

    counterBetterFit=0
    counterCorrectFit=0
    counterTotalFit=0
    residual_Sum_of_Squares_min=Inf
    storedModelOutput=list()
    storedModelOutput$residual_Sum_of_Squares=Inf

    while(counterCorrectFit<n_runs_min & counterTotalFit<n_runs_max)
    {
      counterTotalFit=counterTotalFit+1
      if(model == "test"){modelOutput=exampleFitFunction(randomParameter,...)}
      if(model == "linear"){modelOutput=lineFitFunction(dataInput=dataInput,tryCounter=counterTotalFit,...)}
      if(model == "sigmoidal"){modelOutput=sigmoidalFitFunction(dataInput=dataInput,tryCounter=counterTotalFit,...)}
      if(model == "doublesigmoidal"){modelOutput=doublesigmoidalFitFunction(dataInput=dataInput,tryCounter=counterTotalFit,...)}

      if(is.na(dataInputName))
      {
        isalist=(is.list(dataInput) & !is.data.frame(dataInput))
        if(isalist)
        {
          modelOutput$dataInputName=dataInput$dataInputName
        }
        if(!isalist)
        {
          modelOutput$dataInputName=NA
        }
      }

      if(!is.na(dataInputName))
      {
        isalist=(is.list(dataInput) & !is.data.frame(dataInput))
        if(isalist)
        {
          if(is.na(dataInput$dataInputName))
          {
            modelOutput$dataInputName=dataInputName
          }
          if(!is.na(dataInput$dataInputName))
          {
            if(dataInput$dataInputName!=dataInputName)
              {stop("the input data has already have a name")}
            if(dataInput$dataInputName==dataInputName)
            {modelOutput$dataInputName=dataInputName}
          }
        }
        if(!isalist)
        {modelOutput$dataInputName=dataInputName}
      }


      if(modelOutput[["isThisaFit"]]){
        counterCorrectFit=counterCorrectFit+1
        if(residual_Sum_of_Squares_min>modelOutput$residual_Sum_of_Squares){
          counterBetterFit=counterBetterFit+1
          residual_Sum_of_Squares_min=modelOutput$residual_Sum_of_Squares
          storedModelOutput=modelOutput
        }
      }

      if(showDetails){
        print(c(betterFit=counterBetterFit,
                correctFit=counterCorrectFit,
                totalFit=counterTotalFit,
                currentOutput=modelOutput$residual_Sum_of_Squares,
                bestOutput=storedModelOutput$residual_Sum_of_Squares))}

    }

    # add number off independent runs to outputs
    # might be important for quality checks
    storedModelOutput=c(storedModelOutput,
                        betterFit=counterBetterFit,
                        correctFit=counterCorrectFit,
                        totalFit=counterTotalFit)

    return(storedModelOutput)
  }

#' @title exampleFitFunction
#'
#' @param randomParameter This parameter defines the probability that the exampleFitFunction returns TRUE values for isThisaFit parameter. The aparemeter should be in the interval of 0 and 1
#' @description This is the exampleFitFunction that generates TRUE values for isThisaFit parameter whit given probability
#' @return The function returns TRUE or FALSE for isThisaFit parameter and also residual_Sum_of_Squares parameter that determines the goodness of the fit
#'
#' @examples
#' # add usage examples here
exampleFitFunction<-
  function(randomParameter)
  {
    if(randomParameter<0 | randomParameter>1)
      {stop("the random parameter for model test should be between 0 and 1")}
    randomNumber=stats::runif(1, 0, 1)
    if(randomNumber<randomParameter){isThisaFit=TRUE; residual_Sum_of_Squares=stats::runif(1, 0, 1)}
    if(randomNumber>randomParameter){isThisaFit=FALSE; residual_Sum_of_Squares=NA}
    outputList=list(isThisaFit=isThisaFit,
                    randomParameter=randomParameter,
                    residual_Sum_of_Squares=residual_Sum_of_Squares)
    return(outputList)
  }
