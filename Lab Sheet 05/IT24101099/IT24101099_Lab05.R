setwd("C:\\Users\\ASUS\\Desktop\\PS_LAB05")
getwd()

#Importing the data set
data <- read.table("Data.txt",header =TRUE,sep = ",")
##View the filr in a separate  window
fix(data)
#Close the data window before you run the rest of the commands.
#Unless rest of the commands won't run.

#Attach the filr into R. so,you can call the variables by their names
attach(data)

##Part1
#Rename the variables (column headings ) of the data set as X1 and X2
names(data)<-c("X1","X2")
##Attach the file into R again as we renamed the variables.
attach (data)
##Obtain histogram for number of shareholders
hist(X2,main="Histogram for Number of Shareholders")

##Part 2
#Using "breaks command we can define the number of classes we need in the histogram
#along with the lower limit and upper limit
#Using "right" command we can define whether classes have closed intervals or open intervals.
histogarm <- hist(X2,main="Histogram for Number of Shareholders",breaks = seq(130,270,length=8),right = FALSE)
##Check how each argument inside "hist" command works using "help" command as follows
?hist


##Part3
##Assign class limits of the frequency distribution into a variable called "breaks"
breaks <- round(histogarm$breaks)
#Assign class frequencies of the histogram into a variable called"freq"
freq <- histogarm$counts
#Assign mid point of each class into a variable called"mids"
mids <- histogarm$mids

#Creating the variable called "Classes" for the frequency distribution
classes <- c()

##Creating a "for" loop to assign classes of the frequency distribution into "Classes" variable created above.
for (i in 1: length(breaks)-1){
  classes[i] <- paste0("[",breaks[i],",",breaks[i+1],")")
}
#Obtaining frequency distribution by combining the values of "Classes" & "freq" variables
##"cbind" command used to merge the columns with the columns with same length
cbind(Classes = classes,Frequency = freq)

#Part 4
##Draw  frequency polygon to the same plot.
lines(mids,freq)

#Draw frequency polygon in a new plot.
plot(mids,freq,type ='l',main = "Frequency Polygon for Shareholders",xlab = "Shareholders",ylab = "Frequency",ylim = c(0,max(freq)))

#Part 5
##Using "cumsum" command we can get cumulative frequencies
cum.freq <- cumsum(freq)
#Creating a null variable called "new"
new <- c()
## Using "for" loop to store cumulative frequencies in order to get the ogive
for(i in 1:length(breaks)){
  if(i==1){
    new[i] = 0
  }else{
    new[i]= cum.freq[i-1]
  }
}
#Draw cumulative frequency polygon in a new plot
plot(breaks,new,type="l",main = "Cumulative Frequency Polygon for Shareholders",xlab = "Shareholders",ylab ="Cumulative Frequency",ylim=c(0,max(cum.freq)))
#Obtain upper limit of each class along with its cumulative frequency in a table
cbind(Upper = breaks,CumFreq = new)

setwd("C:\\Users\\ASUS\\Desktop\\PS_LAB05")

# Import dataset (assuming the file is in working directory)
Delivery_Times <- read.table("Exercise - Lab 05.txt", header = TRUE)

head(Delivery_Times)

# Import without header
Delivery_Times <- read.table("Exercise - Lab 05.txt", header = FALSE)

# Rename column properly
colnames(Delivery_Times) <- "Time"


# Convert column to numeric
Delivery_Times$Time <- as.numeric(as.character(Delivery_Times$Time))

# Define breaks (20 to 70 with 9 equal intervals)
breaks <- seq(20, 70, length.out = 10)

# Draw histogram
hist(Delivery_Times$Time,
     breaks = breaks,
     right = FALSE,
     col = "lightblue",
     border = "black",
     main = "Histogram of Delivery Times",
     xlab = "Delivery Time",
     ylab = "Frequency")

# Build frequency table
freq_table <- hist(Delivery_Times$Time, 
                   breaks = breaks, 
                   right = FALSE, 
                   plot = FALSE)

# Cumulative frequencies
cum_freq <- cumsum(freq_table$counts)

# Ogive plot
plot(breaks[-1], cum_freq, type = "o", pch = 16, col = "blue",
     main = "Cumulative Frequency Polygon (Ogive)",
     xlab = "Delivery Time",
     ylab = "Cumulative Frequency")




          