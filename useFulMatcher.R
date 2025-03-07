library(plyr)

df <- as.data.frame(gbu)
x <- as.data.frame(unique(is.na(df[!complete.cases(df),])))

# Iterate through rows of x

rownames(match_df(as.data.frame(is.na(df[!complete.cases(df),])), x))
