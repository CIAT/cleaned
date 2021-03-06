% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/differences.R
\name{calculate_differences}
\alias{calculate_differences}
\title{Calculate Differences}
\usage{
calculate_differences(iDir)
}
\arguments{
\item{iDir}{A path to a directory where output files are stored}
}
\value{
dataframe
}
\description{
It computes difference in environmental impact between scenarios
}
\examples{
\dontrun{
#' data(mufindi)
data(ghg_para)
feed_basket_quality <- feed_quality(mufindi)
energy_required <- energy_requirement(mufindi,feed_basket_quality)
land_required <- land_requirement(feed_basket_quality, energy_required, mufindi)
soil_erosion <- soil_health(mufindi, land_required)
water_required <- water_requirement(mufindi,land_required)
nitrogen_balance <- n_balance(mufindi, land_required, soil_erosion)
livestock_productivity <- land_productivity(mufindi)
economics <- economics_payback(mufindi, energy_required)
biomass <- biomass_calculation(mufindi, land_required)
soil_carbon <- soil_organic_carbon(para, land_required, biomass)
ghg_emission <- ghg_emission(mufindi,energy_required,ghg_para,land_required,nitrogen_balance)
combineOutputs(feed_basket_quality,energy_required,land_required,soil_erosion,water_required,
nitrogen_balance,livestock_productivity,economics,biomass,soil_carbon,ghg_emission)
calculate_differences(iDir)
}

}
