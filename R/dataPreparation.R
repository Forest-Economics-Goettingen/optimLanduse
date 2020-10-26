##--##################--##
#### Data preparation ####
##--##################--##

# Tue Oct 13 15:41:41 2020 ------------------------------


#' Preparation of the data for the robust optimization
#'
#'
#' @param dat Untransformed data table as shown in the example.
#' @param uncertainty Indicates whether the uncertainty shall be repesented by SE or SD. Please be aware that the respective chosen uncertainty must be included in the data. Best would be to consider the format of the exemplary data (database.xlsx) in the GitLab.
#' @return An initialized landUse object ready for initScenario.


#' @import dplyr
#' @import janitor


#' @export
dataPreparation <- function(dat, uncertainty = "SE"){
  ## Filter all Rows with only NA ##
  dat.final <- dat[rowSums(is.na(dat)) != ncol(dat), ]
  dat.final <- dat.final %>% drop_na(...4)

  ## select landUse names ##
  landUse <- dat[1, ]
  landUse <- landUse[, colSums(is.na(landUse)) != nrow(landUse)]
  landUse <- landUse %>% row_to_names(1)
  landUse <- names(landUse)

  ## Create column names ##
  dat.final <- dat.final %>%  row_to_names(1)

  ## rename duplicated Columnnames ##
  names(dat.final) <- make.unique(colnames(dat.final))

  ## rename first columns for initScenario function and define data structure ##
  names(dat.final)[1:4] <- c("branch", "indicatorGroup", "indicator", "direction")
  dat.final[5:ncol(dat.final)] <- lapply(dat.final[5:ncol(dat.final)], as.numeric)
  dat.final[, 5:ncol(dat.final)][is.na(dat.final[, 5:ncol(dat.final)])] <- 0

  ## select mean values, rename columns and gather ##
  importValues <- dat.final %>% select(branch, indicatorGroup, indicator, direction, starts_with("mean"))
  colnames(importValues)[grepl("mean", colnames(importValues))] <- landUse
  importValues <- importValues %>%  gather(key = "landUse", value = "indicatorValue", Forest:`Oil palm plantation`)

  ## select uncertainty, rename columns and gather ##
  importUnc <- dat.final %>% select(branch, indicatorGroup, indicator, direction, starts_with(uncertainty))
  colnames(importUnc)[grepl(uncertainty, colnames(importUnc))] <- landUse
  importUnc <- importUnc %>%  gather(key = "landUse", value = "indicatorUncertainty", Forest:`Oil palm plantation`)

  ## combine mean and uncertainty ##
  dataSource <- left_join(importValues, importUnc, by = c("branch", "indicatorGroup", "indicator", "direction", "landUse"))
  return(dataSource)
}
