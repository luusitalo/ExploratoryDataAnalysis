#Exploratory data analysis course @ Coursera
#Course Project 1, Week 1

#read in the data (source: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip)
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
## Plotting Fig 1
####################

png('plot1.png', width=480, height=480)
with (dat, hist(Global_active_power, main = "Global Active Power", col="red",xlab="Global Active Power (kilowatts)"))
dev.off()

####################
## Plotting Fig 2
####################

png('plot2.png', width=480, height=480)
with (dat, plot(Global_active_power ~ dateTime, type='l', main = "", xlab= "", ylab = "Global Active Power (kilowatts)"))
dev.off()

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

####################
## Plotting Fig 4
####################

png('plot4.png', width=480, height=480)
par(mfrow=c(2,2))
#top left
with (dat, plot(Global_active_power ~ dateTime, type='l', main = "", xlab= "", ylab = "Global Active Power"))

#top right
with (dat, plot(Voltage ~ dateTime, type='l', main = "", xlab= "datetime", ylab = "Voltage"))

#bottom left
with (dat, plot(Sub_metering_1 ~ dateTime, type='l', main = "", xlab= "", ylab = "Energy sub metering"))
lines(dat$Sub_metering_2  ~ dat$dateTime, col="red")
lines(dat$Sub_metering_3  ~ dat$dateTime, col="blue")
  ###add legend
leg.txt <- c("Sub_metering_3","Sub_metering_3","Sub_metering_3")
leg.col <- c("black", "red", "blue")
legend("topright", legend=leg.txt, col=leg.col, lwd=1.5, bty="n")

#bottom right
with (dat, plot(Global_reactive_power ~ dateTime, type='l', main = "", xlab= "datetime"))

dev.off()
