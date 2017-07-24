library(data.table)
rawData = read.table(
        paste0(getwd(), "/", "data.csv"),
        header = TRUE,
        sep = '#',
        row.names = NULL,
        comment.char = "",
        quote = ""
)
setDT(rawData)
