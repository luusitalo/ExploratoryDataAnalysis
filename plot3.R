#Exploratory data analysis course @ Coursera
#Course Project 1, Week 1

#This code assumes that the unzipped data file exists in the working directory!
#Please download the data from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#and unzip it to the working directory beore running this script.


###################################
## Reding cleaning the data
###################################

#read in the data
dat<-read.table("household_power_consumption.txt", sep=";",header=TRUE)

#format date into R date format
dat$Date<-as.Date(dat$Date, format="%d/%m/%Y")

#take the subset we're using on this assignment: between 2007-02-01 and 2007-02-02
dat<-with(dat, dat[(Date >= "2007-02-01" & Date <= "2007-02-02"),])


#Make a dateTime variable with date and time info merged; change to POSIXct class
dat$dateTime<-paste(dat$Date, dat$Time)
dat$dateTime<-as.POSIXct(dat$dateTime, format="%Y-%m-%d %H:%M:%S")

#remove the Date and Time columns
dat<-dat[,-c(1:2)]

#There are no "?" characters in this subset, so they needn't be replaced, 
#but some of the columns are "factor" type due to the "?"s that were in he original data set.
#Therefore, we need to change the column types into numeric
for (i in 1:7) {
        dat[,i]<-as.numeric(as.character(dat[,i]))
}


####################
## Plotting Fig 3
####################

png('plot3.png', width=480, height=480)
with (dat, plot(Sub_metering_1 ~ dateTime, type='l', main = "", xlab= "", ylab = "Energy sub metering"))
lines(dat$Sub_metering_2  ~ dat$dateTime, col="red")
lines(dat$Sub_metering_3  ~ dat$dateTime, col="blue")
#add legend
leg.txt <- c("Sub_metering_3","Sub_metering_3","Sub_metering_3")
leg.col <- c("black", "red", "blue")
legend("topright", legend=leg.txt, col=leg.col, lwd=1.5)
dev.off()
