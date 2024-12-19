library(readr)
combined_df <- read_csv("Raw_Data/combined_df.csv")
View(combined_df)
df<-combined_df[,1]
df<-combined_df[,c(1,2,3)]
df
df2<-spread(df,key=Action,value=`2018/19`)
library(tidyr)
nstall.packages('tidyr')
install.packages('tidyr')
library(tidyr)
df2<-spread(df,key=Action,value=`2018/19`)
View(df2)
cor.test(df2$import,df2$consumption)
hist(df2$import, breaks=10, col=rgb(1,0,0,0.5), xlim=range(c(df2$import, df2$consumption)), main="Histogram of Sugar Imports and Consumption", xlab="Value", ylab="Frequency")
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
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency",
     probability=TRUE)
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd),
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Density",
     probability=TRUE)
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd),
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency")
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
df2 <- df2[-16, ]
cor.test(df2$import,df2$consumption)
cor.test(df2$import,df2$consumption)
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency")
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="purple")
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=20,
     col="purple")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=18,
     col="purple")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="purple")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) - 10, max(df2$import, na.rm=TRUE) + 10),
     ylim=c(min(df2$consumption, na.rm=TRUE) - 10, max(df2$consumption, na.rm=TRUE) + 10))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 13000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 35000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 5000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 20000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 20000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 15000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 1000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 2000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1800),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 2000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 2500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 3000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 4000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 6000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 10000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 10000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 10000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 8000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 9000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 5000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 20000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 20000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 15000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 1000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 2000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1800),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 2000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 2500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 3000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 4000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 6000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 3000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 10000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 10000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 10000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 8000))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 9000))
abline(lm(df2$consumption ~ df2$import), col="red")
df2 <- df2[-14, ]
cor.test(df2$import,df2$consumption)
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency")
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Density",
     probability=TRUE,
     xlim=c(min(df2$consumption, na.rm=TRUE) - 20, max(df2$consumption, na.rm=TRUE) + 20))
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Density",
     probability=TRUE,
     xlim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 2000))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Density",
     probability=TRUE,
     xlim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 1000))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Density",
     probability=TRUE,
     xlim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 1500))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Density",
     probability=TRUE,
     xlim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 1200))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Density",
     probability=TRUE,
     xlim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 1500))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Density",
     probability=TRUE,
     xlim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 1450))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency", xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1450),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 1450))
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency", xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1450),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 0))
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency", xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1450)
)
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency", xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1000)
)
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency")
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency", xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 500)
)
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=10,
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency")
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(hist(df2$consumption, breaks=10, plot=FALSE)$breaks)[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=seq(0, 30000, by=5000),
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Density",
     probability=TRUE,
     xlim=c(0, 30000))
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd),
      col="darkgreen",
      lwd=2,
      add=TRUE)
hist(df2$consumption,
     breaks=seq(0, 30000, by=5000),
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency",
     xlim=c(0, 30000))
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(seq(0, 30000, by=5000))[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
hist(df2$consumption,
     breaks=seq(0, 30000, by=5000),
     col=rgb(0,0,1,0.5),
     main="Histogram of Sugar Consumption",
     xlab="Sugar Consumption",
     ylab="Frequency",
     xlim=c(0, 30000),
     ylim=c(0, 12))
consumption_mean <- mean(df2$consumption, na.rm=TRUE)
consumption_sd <- sd(df2$consumption, na.rm=TRUE)
curve(dnorm(x, mean=consumption_mean, sd=consumption_sd) * length(df2$consumption) * diff(seq(0, 30000, by=5000))[1],
      col="darkgreen",
      lwd=2,
      add=TRUE)
legend("topright",
       legend=c("Sugar Consumption", "Normal Curve (Consumption)"),
       fill=c(rgb(0,0,1,0.5), "darkgreen"))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 9000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 5000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 6000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 8000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 9000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 3000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 7000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 3000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 6000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 3000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 7000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 2000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 7000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 1000),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 7000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 800),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 7000))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 6500))
abline(lm(df2$consumption ~ df2$import), col="red")
spearman_result <- cor.test(df2$import, df2$consumption, method = "spearman")
print(spearman_result)
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     cex=2,
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 6500))
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     cex=1.5,
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 6500))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     cex=1.0,
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 6500))
abline(lm(df2$consumption ~ df2$import), col="red")
plot(df2$import, df2$consumption,
     main="Scatter Plot of Sugar Imports vs. Consumption",
     xlab="Sugar Imports",
     ylab="Sugar Consumption",
     pch=19,
     col="blue",
     cex=1.5,
     xlim=c(min(df2$import, na.rm=TRUE) + 0, max(df2$import, na.rm=TRUE) + 500),
     ylim=c(min(df2$consumption, na.rm=TRUE) + 0, max(df2$consumption, na.rm=TRUE) + 6500))
abline(lm(df2$consumption ~ df2$import), col="red")
git_log <- read.csv("git_log.csv", header = FALSE)
colnames(git_log) <- c("Commit Hash", "Author", "Date", "Message")
git_log <- read.csv("git_log.csv", header = FALSE)
colnames(git_log) <- c("Commit Hash", "Author", "Date", "Message")
install.packages("writexl")
library(writexl)
write_xlsx(git_log, "git_log.xlsx")