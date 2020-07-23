# CLEANED XtRa Feedbasket Coputation
#
#Preparing a working environment
#Loading packages
reqPackages = c("jsonlite","tidyverse")
numPackages = length(reqPackages)
for (pkg in 1:numPackages) {
  package = reqPackages[pkg]
  if (!is.element(package, installed.packages()[,1])) {
    install.packages(package,repos = 'http://cran.us.r-project.org')
  }else{
    Exists = paste(package, "...Package",".......E X I S T S ",sep = "")
    print(Exists)
  }
}

lapply(reqPackages,require,character.only=TRUE)

#Working directory
path <- "C:/Users/S.Oloo/Documents/ILRI Work/CLEANED R vs Ex/Gitpulledcleaned/cleaned-XtRa"

#Loading data
para <- fromJSON(paste0(path,"/data/example.json"))

#Computation of energy requirement
livestock <- para[["livestock"]]%>%
  mutate(energy_year=(((er_maintenance*365)+(er_grazing*365)+(er_pregnancy*365)+
                        (er_lactation*365)+(0*er_lactmilk)+(0*er_growth))*herd_composition),
         protein_req=(((cp_maintenance*365)+(cp_grazing*365)+(cp_pregnancy*365)+(cp_lactation*365)+
                         +(0*er_lactmilk)+(0*er_growth))*herd_composition),
         energy_s1=(energy_year*))


