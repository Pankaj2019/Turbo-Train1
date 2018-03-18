#Question-1
#########################################################
x<-rnorm(100,0,1)
x
x1<-mean(x)
x1
y<-x1*rexp(100, rate=abs(1/x1))
y
z<--x1*rexp(100, rate=abs(1/-x1))
z

k <- sum( abs(y) > abs(x) )9
k  
list(xVec=xvec,yvec=yvec,kvec=kvec)

myfunction<- Function(n)
  {
    x<-rnorm(n)
  x1<-mean(x)
  y<-x1*rexp(n, rate=abs(1/x1))
  z<--x1*rexp(100, rate=abs(1/-x1))
  k<- sum(abs(y) > abs(x))
  list(xVec=xvec,yvec=yvec,kvec=kvec)
  }

################################################

Question-2

age<-c(23,45,12,42,69,70)
height<-c(160,162,147,156,145,162)
name<-c("Yatindra","Alok","Payal","Kabir","Meghana","Anil")
sex<-c("Male","Male","Female","Male","Female","Male")
family<-data.frame(age,height,name,sex)
family
apply(family,2,mean)
apply(family[,1:2],2,mean)
####################################################################

Question-3

n<-(1:100)
n
n[as.logical(isprime(n))]  ### library(matlab)

######################################################################
#Question-4
#Elecmart Sales data

getwd()
setwd("E:/R")
setwd
read.csv("Elecmart Sales.csv")
cricData<-read.csv("Elecmart Sales.csv")
cricData
nrow(cricData)
ncol(cricData)
cricData[,3]
data.frame(cricData$Gender)
cricData[1:2,]
cricData[,c(1,3,4)]
Female<- subset(cricData, Gender=="female")
Female
Female <- cricData[cricData$Gender=="female", , drop = FALSE]
Female
cricData<-subset(cricData,Gender=="Female")
cricDataFem<-subset(cricData,Gender=="Female")
cricDataFem
# by using command remove all $ sign from sales 
cricDatasmall<-subset(cricData,TotalCost<100)
cricDatasmall
cricDatamed<-subset(cricData,TotalCost>100,TotalCost<500)
cricDatamed
cricDatabig<-subset(cricData,TotalCost>500)
cricDatabig
summary(cricData)
cor(cricData$ItemsOrdered
    ,cricData$TotalCost
)

cov(cricData$ItemsOrdered
    ,cricData$TotalCost
)

mean(cricData$TotalCost
)

median(cricData$TotalCost
)
range(cricData$TotalCost
)
var(cricData$TotalCost
)
sd(cricData$TotalCost)
#################################################################
Question-5

# Draw a vertical bar plot and a horizontal bar plot on the number of "None", "Some" and "Marked". Give a title and label the axes.

#Install VCD package

(counts <- table(Arthritis$Improved))
barplot(counts,main="Simple Bar Plot",xlab="Improvement", ylab="Frequency")

# Horizontal Bar-Graph
barplot(counts, main="Horizontal Bar Plot", xlab="Frequency", ylab="Improvement", horiz=TRUE)

# Draw the graph as a stacked bar plot and a grouped bar plot.

(counts <- table(Arthritis$Improved, Arthritis$Treatment))
head(Arthritis$Improved)
head(Arthritis$Treatment)
(counts <- table(Arthritis$Improved, Arthritis$Treatment))
# Stacked Bar-Plot

barplot(counts,
        main="Stacked Bar Plot",
        xlab="Treatment", ylab="Frequency",
        col=c("red", "Blue","green"),
        legend=rownames(counts))

# Grouped Bar Plot

barplot(counts,
        main="Grouped Bar Plot",
        xlab="Treatment", ylab="Frequency",
        col=c("red", "yellow", "green"),
        legend=rownames(counts), beside=TRUE)

#Draw bar plots for sorted mean and median values.
(means <- aggregate(Arthritis$Frequency, by=list(Treatment), FUN=mean))

# Draw a spinogram.
counts <- table(Treatment, Improved)

spine(counts, main="Spinogram Example")

Arthritis

counts <- table(Treatment, Improved)

spine(counts, main="Spinogram Example")

attach(Arthritis)

counts <- table(Treatment, Improved)

spine(counts, main="Spinogram Example")

######################################################################################

Question-6

par(mfrow=c(2, 2))
slices <-c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")

pie( slices, labels = lbls,main="Simple Pie Chart")

(pct <- round(slices/sum(slices)*100))

(lbls2 <- paste(lbls, " ", pct, "%", sep=""))

pie(slices, labels=lbls2, col=rainbow(length(lbls2)), main="Pie Chart with Percentages")

### install the package plotrix for 3D plot ##

pie3D(slices, labels=lbls,explode=0.1,
      main="3D Pie Chart ")
  
  
fan.plot(slices, labels = lbls, main="Fan Plot")

#############################################################################

Question-7

data(mtcars)
mtcars

#Draw a simple histogram

mtcars$mpg
hist(mtcars$mpg)  

#Draw another histogram with 12 bins of red color.

hist(mtcars$mpg, freq=FALSE,breaks=12, col="red",
     xlab="Miles Per Gallon", main="Histogram, rug plot, density curve")

#Draw another histogram with rug plot and frame.

hist(mtcars$mpg, freq=FALSE,breaks=12, col="red",
     xlab="Miles Per Gallon", main="Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg), col="blue", lwd=2)

#Draw the fourth histogram with normal curve.

x <- mtcars$mpg 
h<-hist(x, breaks=12, col="red", xlab="Miles Per Gallon",
        main="Histogram with normal curve and box") 

#Draw a kernel density plot

d <- density(mtcars$mpg)
par(mfrow=c(2,1))
d <- density(mtcars$mpg)
plot(d)
plot(d, main="Kernel Density of Miles Per Gallon")

#Draw a box plot, and then draw a parallel box plot to compare these three groups: four, six and eight cylinders.

boxplot(mpg ~ cyl, data=mtcars,
        main="Car Mileage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")

#########################################################################################

# question 8

Calper<-function(df_banksalari) {
  df_banksalari1<-df_banksalri[,c(3,6)]
  df_result<-table(df_banksalari1)
  df_result<-cbind(df_result,Total=rowsums(table(df_banksalari1)))
  df_result<-prop.table(df_result,margin=2)*100
  df_result<-apply(prop.table(df_result,2)*100,2,function(u) sprintf("%.2f%%", u))
  jobgrade<-seq(1:6)
  df_result<-çbind(jobGrade.df_result)
df_result
}

