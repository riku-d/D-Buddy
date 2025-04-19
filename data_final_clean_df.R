
library(dplyr)
setwd("C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy")
df1<-read.csv("diabetes_cleaned.csv")
df2<-read.csv("diabetes_cleaned1.csv")
for (col in setdiff(colnames(df2), colnames(df1))) {
  df1[[col]] <- NA
}

for (col in setdiff(colnames(df1), colnames(df2))) {
  df2[[col]] <- NA
}

# Bind rows after aligning columns
df <- bind_rows(df1, df2)

#Data entry
imputed_data <- mice(df, method = "pmm", m = 5, seed = 500)
df <- complete(imputed_data)

df$smoking_history <- as.character(df$smoking_history)
df$smoking_history[df$smoking_history %in% c("No Info") | is.na(df$smoking_history)] <- "Unknown"
df$smoking_history <- as.factor(df$smoking_history)
write.csv(df, "C:/Users/rohit/OneDrive/DesktopProjects/D-buddy/diabetes_cleaned2.csv", row.names = FALSE)
