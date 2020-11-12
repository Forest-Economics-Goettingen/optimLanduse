##--##################--##
#### Data preparation ####
##--##################--##

# Tue Oct 13 15:41:41 2020 ------------------------------


#' Preparation of the data for the robust optimization
#'
#'
#' @param dat Untransformed data table as shown in the example.
#' @param uncertainty Indicates whether the uncertainty shall be repesented by SE or SD. Please be aware that the respective chosen uncertainty must be included in the data. Best would be to consider the format of the exemplary data (database.xlsx) in the GitLab.
#' @param expVAL Indicates how the expected Value should be represented.
#' @return An initialized landUse object ready for initScenario.


#' @import dplyr

#' @export
dataPreparation <- function(dat, uncertainty = "SE", expVAL = "mean"){   #added expected value

    ## Convert input Data to dat.final ##
    ## Filter all Rows with only NA ##
    dat.final <- dat[rowSums(is.na(dat)) != ncol(dat), ]
    if(any(is.na(dat.final[, 1]))){dat.final <- dat.final[-1, ]}

    ## Create column names ##
    colnames(dat.final) <- dat.final[1, ]
    dat.final <- dat.final[-1, ]

    ## rename duplicated Columnnames ##
    names(dat.final) <- make.unique(colnames(dat.final))

    ## detect and set classes of a dat.final
    dat.final <- lapply(dat.final, type.convert) %>% bind_cols()

    ## rename first columns for initScenario function and define data structure ##
    chtr.cols <- unlist(lapply(dat.final[1,],is.numeric))
    chtr.cols <- length(chtr.cols[chtr.cols == FALSE])
    dat.final[, (chtr.cols+1) : ncol(dat.final)][is.na(dat.final[, (chtr.cols + 1) : ncol(dat.final)])] <- 0

    ## warn and delete factor rows with NA ##
    if(any(is.na(dat.final[, 1:chtr.cols]))){warning("Some Indicators have missing value, rows got deleted")}
    # which(is.na(dat.final[, 1:chtr.cols]), arr.ind = TRUE)
    dat.final <- na.omit(dat.final)

    ## select landUse names ##
    landUse <- dat[1, ]
    landUse <- landUse[, colSums(is.na(landUse)) != nrow(landUse)]
    colnames(landUse) <- landUse[1, ]
    landUse <- landUse[-1, ]
    landUse <- names(landUse)

    ## select mean values, rename columns and gather ##
    importValues <- dat.final %>% select((1:chtr.cols), starts_with(expVAL))
    colnames(importValues)[grepl(expVAL, colnames(importValues))] <- landUse
    importValues <- importValues %>%  gather(key = "landUse", value = "indicatorValue", landUse[1]:landUse[length(landUse)])

    ## select uncertainty, rename columns and gather ##
    importUnc <- dat.final %>% select((1:chtr.cols), starts_with(uncertainty))
    colnames(importUnc)[grepl(uncertainty, colnames(importUnc))] <- landUse
    importUnc <- importUnc %>%  gather(key = "landUse", value = "indicatorUncertainty", landUse[1]:landUse[length(landUse)])

    ## combine mean and uncertainty ##
    dataSource <- left_join(importValues, importUnc, by = c(names(dat.final)[1:chtr.cols], "landUse"))
  return(dataSource)
}

