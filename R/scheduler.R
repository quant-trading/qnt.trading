library(taskscheduleR)
myscript <- system.file("extdata", "nlu_indicators_parsing.R", package = "taskscheduleR")


## run script every day at 09:10
taskscheduler_create(taskname = "nlu_indicators_daily", rscript = myscript,
                     schedule = "DAILY", starttime = "21:00")

## delete the tasks
#taskscheduler_delete(taskname = "nlu_indicators_daily") 





#install.packages("devtools")
#library(devtools)
#dev_mode(on=T)
#install_github("jwijffels/taskscheduleR")
# when finished do:
#dev_mode(on=F)  #and you are back to having stable ggplot2

