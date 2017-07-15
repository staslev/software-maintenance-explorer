library(data.table)
rawData = read.table(paste0(getwd(),"/","data-vis-final-prj.csv"), sep = "#", header = TRUE, comment.char = "")
setDT(rawData)
