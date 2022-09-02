rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#load function
source(paste0(thisdir,"/../../R/MergeFilterAndCollapse.R"))

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)


#load input
longitudinal_dataset <- fread(paste0(thisdir,"/input/longitudinal_dataset.csv"), sep = ",")
cohort <- fread(paste0(thisdir,"/input/cohort.csv"), sep = ",")

longitudinal_dataset[,date := as.character(date)]
longitudinal_dataset[,date := as.Date(date,"%Y%m%d")]
cohort[,study_entry_date := as.character(study_entry_date)]
cohort[,study_entry_date := as.Date(study_entry_date,"%Y%m%d")]


output <- MergeFilterAndCollapse(listdatasetL =  list(longitudinal_dataset),
                                 datasetS = cohort,
                                 condition= "date >= study_entry_date - 365 & date < study_entry_date",
                                 key = "person_id",
                                 saveintermediatedataset = F,
                                 strata=c("person_id","conceptset"),
                                 summarystat = list(
                                   list(c("max"),"n","at_least_one"),
                                   list(c("sum"),"n","number_of_diagnosis")
                                 )
)

fwrite(output,file=paste0(thisdir,"/output/output.csv"))
