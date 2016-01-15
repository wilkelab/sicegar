#' @title DataCheck Function
#'
#' @param data it is the input data file. In can be either a list that contains a data frame in .$timeIntensityData or a data frame by itself.
#'
#' @details The dataCheck checks if the input data is appropirate and covert it into an appropirate form. Data frame should contain two columns named time and intensity for timeData and intensityData. If the data frame is in a list its name in the list should be $timeIntensityData
#' @export
#'
#' @examples
#'
#' # Example 1
#' # generate data frame
#' time = seq(3,48,0.5)
#' intensity=runif(length(time), 3.0, 7.5)
#' dataInput = data.frame(time,intensity)
#' dataOutputVariable = dataCheck(dataInput)
#'
#' Example 2
#' # generate data frame
#' time = seq(3,48,0.5)
#' intensity=runif(length(time), 3.0, 7.5)
#' dataInput = data.frame(time,intensity)
#' dataOutput = normalizeData(dataInput)
#' dataInput2=dataOutput
#' dataOutputVariable2 = dataCheck(dataInput2)

dataCheck <-function(data){

  isalist=(is.list(data) & !is.data.frame(data))

  if(isalist){

    doesitcontainTID=("timeIntensityData" %in% names(data))

    if(doesitcontainTID){
      issection_dataframe=is.data.frame(data$timeIntensityData)

      if(issection_dataframe){
        data_next_check=data$timeIntensityData
      }

      if(!issection_dataframe){
        stop("the timeIntensityData section should be a data frame")
      }

    }

    if(!doesitcontainTID){
      stop("the list should contain timeIntensityData section")
    }

  }

  if(!isalist){
    isadataframe=is.data.frame(data)

    if(isadataframe){
      data_next_check=data
      data2=data
      data=list(timeIntensityData=data2)
    }

    if(!isadataframe){
      stop("It should either be a list or a data frame")
    }
  }

  num_columns=ncol(data_next_check)
  if(num_columns==2){
    doescolnamescorrect=("time" %in% names(data_next_check) & "intensity" %in% names(data_next_check))

    if(doescolnamescorrect){
      arecolumnsnumeric=(is.numeric(data_next_check$time) & is.numeric(data_next_check$intensity))

      if(arecolumnsnumeric){

        doesIntesityHaveVariation=(max(data_next_check$intensity)-min(data_next_check$intensity)>0)

        if(!doesIntesityHaveVariation){
          stop("min intensity = max intensity i.e intensity do not have variation")
        }

        if(doesIntesityHaveVariation){
          print("check done")
          return(dataCheckVariable="pass")
        }

      }

      if(!arecolumnsnumeric){
        stop("The columns should include numbers")
      }

    }

    if(!doescolnamescorrect){
      stop("The colum names should be time and intensity")
    }

  }

  if(!num_columns==2){
    stop("The data frame should contain 2 columns")
  }

}
