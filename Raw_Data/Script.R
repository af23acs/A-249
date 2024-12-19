library(tidyr)
combined_df <- read_csv("Raw_Data/combined_df.csv")
View(combined_df)
df<-combined_df[,1]
df<-combined_df[,c(1,2,3)]
df
df2<-spread(df1,key=Action,value=X2018.19)
View(df2)
cor.test(df2$import, df2$consumption)
df2 <- df2[is.finite(df2$import) & is.finite(df2$consumption), ]
hist(df2$import, breaks=10, col=rgb(1,0,0,0.5), xlim=range(c(df2$import, df2$consumption), na.rm=TRUE), main="Histogram of Sugar Imports and Consumption", xlab="Value", ylab="Frequency")
hist(df2$consumption, breaks=10, col=rgb(0,0,1,0.5), add=TRUE)
legend("topright", legend=c("Sugar Imports", "Sugar Consumption"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
plot(df2$import, df2$consumption, 
     main="Scatter Plot of Sugar Imports vs. Consumption", 
     xlab="Sugar Imports", 
     ylab="Sugar Consumption", 
     pch=19, col="blue")
abline(lm(df2$consumption ~ df2$import), col="red")
hist(df2$import, breaks=10, col=rgb(1,0,0,0.5), xlim=range(c(df2$import, df2$consumption), na.rm=TRUE), main="Histogram of Sugar Imports and Consumption", xlab="Value", ylab="Frequency", probability=TRUE)
import_mean <- mean(df2$import, na.rm=TRUE)
import_sd <- sd(df2$import, na.rm=TRUE)
curve(dnorm(x, mean=import_mean, sd=import_sd), col="darkblue", lwd=2, add=TRUE)
hist(df2$consumption, breaks=10, col=rgb(0,0,1,0.5), add=TRUE, probability=TRUE)
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd), col="darkgreen", lwd=2, add=TRUE)
legend("topright", legend=c("Sugar Imports", "Normal Curve (Imports)", "Sugar Consumption", "Normal Curve (Consumption)"), fill=c(rgb(1,0,0,0.5), "darkblue", rgb(0,0,1,0.5), "darkgreen"))
plot(df2$import, df2$consumption, 
     main="Scatter Plot of Sugar Imports vs. Consumption", 
     xlab="Sugar Imports", 
     ylab="Sugar Consumption", 
     pch=19, col="blue")
abline(lm(df2$consumption ~ df2$import), col="red")