setwd("C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/Datasets")
f<- read.csv("Food_Cleaned.csv")
str(f)
colSums(is.na(f))
sum(duplicated(f$Food_Item))
f1<- f[!duplicated(f$Food_Item), ]
sum(duplicated(f1$Food_Item))
write.csv(f1, "C:/Users/rohit/OneDrive/Desktop/Projects/D-buddy/Food_Cleaned.csv", row.names = FALSE)
