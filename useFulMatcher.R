library(plyr)

df <- as.data.frame(gbu)

rownames(match_df(as.data.frame(is.na(df[!complete.cases(df),])), 
         as.data.frame(unique(is.na(df[!complete.cases(df),])))))
