#First Exploration of the data

summary(`Aids2ann.(1)`)
str(`Aids2ann.(1)`)
range(`Aids2ann.(1)`$age) 
mean(`Aids2ann.(1)`$age)
median(`Aids2ann.(1)`$age)
quantile(`Aids2ann.(1)`$age)
sd(`Aids2ann.(1)`$age)

#Exploratory Data Analysis for Age

hist(`Aids2ann.(1)`$age,col="black",border="white",xlab = "Age",ylab = "frequency")
boxplot(`Aids2ann.(1)`$age) 
x <- seq(0,82)  
y <-dnorm(x,mean=37.74,sd=9.77)   
plot(x,y) 
title("Dnorm of Age")

#Exploratory Data Analysis for Year

range(`Aids2ann.(1)`$year)
mean(`Aids2ann.(1)`$year)
median(`Aids2ann.(1)`$year)
quantile(`Aids2ann.(1)`$year)  
sd(`Aids2ann.(1)`$year) 


hist(`Aids2ann.(1)`$year,col="black",border="white",xlab = "Year",ylab = "frequency") 
plot(`Aids2ann.(1)`$year) 
title("Index of Year") 
boxplot(`Aids2ann.(1)`$year) 
title("Boxplot of Year") 


#Exploratory Data Analysis for Sex, Status and State
table(`Aids2ann.(1)`$sex) 

F    M   202 5812  
table(`Aids2ann.(1)`$sex,`Aids2ann.(1)`$status) 

A    D 
F   97  105   M 2384 3428 

summary(`Aids2ann.(1)`$state) 

NSW Other   QLD   VIC  
3775   544   446  1249 

table(`Aids2ann.(1)`$status) 

A    D  
2481 3533   

plot(`Aids2ann.(1)`$sex,col="black",border="white",xlab = "Sex",ylab = "Frequency") 
title("State") 
plot(`Aids2ann.(1)`$status,col="black",border="white",xlab = "Status",ylab = "Frequency") 
title("Status") 
plot(`Aids2ann.(1)`$sex,col="black",border="white",xlab = "Sex",ylab = "Frequency") 
title("Sex")  

sum(`Aids2ann.(1)`$death>`Aids2ann.(1)`$diag) 

6014-5985 

table(`Aids2ann.(1)`$T.categ) 


table(`Aids2ann.(1)`$T.categ=="het") 

FALSE  TRUE  
5912   102  

table(`Aids2ann.(1)`$T.categ=="het",`Aids2ann.(1 )`$sex) 

F    M 
FALSE  147 5765 
TRUE    55   47 

table(`Aids2ann.(1)`$T.categ=="hs",`Aids2ann.(1)
      `$sex) 

F    M 
FALSE  201  596 
TRUE     1 5216 


boxplot(`Aids2ann.(1)`$year~`Aids2ann.(1)`$T.categ,col = "grey", 
        main="Box plot of Year by Reported Transmission Category",
        xlab="Reported Transmission Category", ylab = "Year") 

boxplot(`Aids2ann.(1)`$age~`Aids2ann.(1)`$T.categ,col = "grey",
        main="Box plot of Age by Reported Transmission Category",
        xlab="Reported Transmission Categor y", ylab ="Age") 



#Pairwise association between Incidents per Year/per Sex 
colnames(`Aids2ann.(1)`)[1] <- "Incidents"
incidents <- as.numeric(`Aids2ann.(1)`$Incidents)
plot(`Aids2ann.(1)`$year, incidents) 
index <- `Aids2ann.(1)`$sex=="M" 
points(`Aids2ann.(1)`$year[index],incidents[index],col="red",pch=3) 
index <- `Aids2ann.(1)`$sex=="F" 
points(`Aids2ann.(1)`$year[index],incidents[index],col="blue",pch=3) 
legend("topleft", legend=c("red=male","blue=fem ale"), pch=c(1,3)) title("Incidents per Year/ per Se x") 

#Pairwise association between Death and Diagnosis    

pairs(`Aids2ann.(1)`$diag~`Aids2ann.(1)`$death)

cor(`Aids2ann.(1)`$death,`Aids2ann.(1)`$diag)

cor.test(`Aids2ann.(1)`$death,`Aids2ann.(1)$diag)


#Chi-Square test for categorical variables

chisq.test(`Aids2ann.(1)`$state,`Aids2ann.(1)`$status) 

#Logistic Regression Model 1

dataoriginal <- glm(outcome~factor(T.categ=="het")+factor(year=="2000")+diag+factor(state),family = binomial,`Aids2ann.(1)`) 
summary(dataoriginal) 

#Logistic Regression Model 2

data <- glm(outcome~factor(T.categ=="het")+factor(year=="2000")+age+diag,family = binomial,`Aids2ann.(1)`) 
summary(data)  

#Likelihood ratio test

lrtest(data,dataoriginal) 

#Alternative way to check the goodness of fit

x2= 2*(logLik(data)-logLik(dataoriginal))

as.numeric(x2) 
pval=1-pchisq(x2,2)
as.numeric(pval) 

Model 1 vs Model 2 = p-value 0.00001623543 

#Binned residual plot for Models 1 and 2 
binnedplot(fitted(dataoriginal),  residuals(dataoriginal, type = "response"),  nclass = NULL,  
           xlab = "Expected Values",  ylab = "Average residual",  main = "Binned residual plot for Model 1",  
           cex.pts = 0.7,  col.pts = 2,  col.int = "blue") 

binnedplot(fitted(data),  residuals(data, type = "response"),nclass = NULL,  
           xlab = "Expected Values",  ylab = "Average residual",  main = "Binned residual plot for Model 2",  
           cex.pts = 0.7,  col.pts = 2,  col.int = "blue") 

#Checking the Deviance betweet the two models

deviance(dataoriginal) 
deviance(data) 

#Checking the Confidence Interval for the strongest model
confint(data)  


#Percentage of Status per Transmission category 

table <- table(`Aids2ann.(1)`$status,`Aids2ann.(1)`$T.categ) 
barplot(prop.table(table,2)*100,xlab='Transmission 
Category',ylab='Percentage of Status', beside=T,col=c("grey","black"), 
        legend=rownames(table), args.legend = list(x = "topright")) 
title("Percentage of Status per Transmis-sion category
") 

#Percentage of survival per Year 

table <- table(`Aids2ann.(1)`$status,`Aids2ann.(1)`$year) 
barplot(prop.table(table,2)*100,
        xlab='Year',ylab='Percentage of Survival',beside=T,col=c("gray","black"),legend = rownames(table), args.legend = list(x = "topright"))  
title("Percentage of survival per Year") 

#Percentage of T. Category per State

state <- table(`Aids2ann.(1)`$state,`Aids2ann.(1)`$T.categ) 
barplot(prop.table(state,2)*100,xlab='Tranmission Category',ylab='Percentage of each T.Categ',beside=T,col =c("gray","black","red","blue"),legend=rownames(state), args.legend = list(x = "topright")) 
title("Percentage of Transmission Category per State") 




