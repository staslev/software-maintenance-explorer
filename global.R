library(data.table)
rawData = read.csv(paste0(getwd(),"/","data-vis-final-prj.csv"), sep = "#", header = TRUE, comment.char = '')
setDT(rawData)
