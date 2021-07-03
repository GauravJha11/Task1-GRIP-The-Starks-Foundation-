
datap = read.csv(file.choose())

plot(datap$Hours,datap$Scores,
     main = "Hours vs Scores",
     xlab = "Hours",
     ylab = "Scores",
     pch = 19,
     cex = 2)# Scatter plot to virtually observe relationship between Hours and Scores
cor(datap$Hours,datap$Scores)# Finding Correlation
linmod = lm(Scores~Hours,data = datap )# Linear Regression Model
abline(linmod,lty = 'dotted', lwd = 2, col = 'blue')# For plotting best-fit line
summary(linmod)# Showing various statistical values
names(linmod)# showing names of statistical values
linmod$fitted.values# For representing fitted values
plot(datap$Hours,linmod$fitted.values,main = "Hours vs Scores",
     xlab = "Hours",
     ylab = "Fitted Values",
     pch = 19,
     cex = 2)# Scatter plot to virtually observe relationship between Hours and fitted values
Checkhours = Hours[1:5]# Storing first 5 entries of Hours 
Scorepredictor  = predict(linmod,list(Hours))# Storing Score Predictions 
a = Scorepredictor[1:5]# Storing Score Predictions of first 5 entries of Hours 
b = Scores[1:5]# Storing first 5 entries of Actual Scores
c= Hours[1:5]# Storing first 5 entries of Hours
comparison = data.frame(c,b,a)# Comparing first 5 Actual Scores and Predicted Scores
names(comparison) = c("Hours","Actual Score","Predicted Score")# Giving Appropriate names to columns
comparison# Showing Comparison Table.
Scorepredictorq  = predict(linmod,list(Hours = 9.25))
Scorepredictorq 
MAE(Scorepredictor,Scores)# Calculating Mean Absolute Error.
