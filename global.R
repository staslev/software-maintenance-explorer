library(data.table)
rawData = read.table(
        paste0(getwd(), "/", "short-data.csv"),
        header = TRUE,
        sep = '#',
        row.names = NULL,
        comment.char = "",
        quote = ""
)
setDT(rawData)
repoInfo = read.csv(paste0(getwd(), "/", "repo-info.csv"), header = TRUE, row.names = NULL)
setDT(repoInfo)