library(reshape2)
library(ggplot2)
Beers = read.csv("Beers.csv", stringsAsFactors = FALSE) 
Breweries = read.csv("Breweries.csv", stringsAsFactors = FALSE)
# Normalize the variable names in the two data sets

#TODO

# Ensure values are right type: 

# TODO

# Remove leading spaces from States

Breweries$State = trimws(Breweries$State)


# How many breweries are present in each state

BreweriesPerState = as.data.frame(table(Breweries$State))
names(BreweriesPerState) = c("State", "Num of Breweries")

# Set a common Brewery ID variable to merge on: 

names(Breweries)[1] = "Brewery_id"

# Merge the two dataframes

MergedBrewing = merge(Beers,Breweries, by = "Brewery_id")

# Find number of NAs in each column

colSums(is.na(MergedBrewing))

# Find max ABV state

MergedBrewing[which(MergedBrewing$ABV == max(MergedBrewing$ABV, na.rm = TRUE)),"State"]

# Find most bitter IBU

MergedBrewing[which(MergedBrewing$IBU == max(MergedBrewing$IBU, na.rm = TRUE)),"State"]

# Plot the ABV vs Bitterness

plot(MergedBrewing$ABV, MergedBrewing$IBU)

# Get list of states

StateList = unique(MergedBrewing$State)
StateList = as.data.frame(StateList)

getStateIBU = function(State) { 
  ibuList = MergedBrewing[which(MergedBrewing$State == State),]$IBU
  medianIBU = median(ibuList, na.rm = TRUE)
  return(medianIBU)
  }

getStateABV = function(State) { 
  abvList = MergedBrewing[which(MergedBrewing$State == State),]$ABV
  medianABV = median(abvList, na.rm = TRUE)
  return(medianABV)
  }

getStateStats = function (State) { 
  ibuList = MergedBrewing[which(MergedBrewing$State == State),]$IBU
  abvList = MergedBrewing[which(MergedBrewing$State == State),]$ABV
  medianIBU = median(ibuList, na.rm = TRUE)
  medianABV = median(abvList, na.rm = TRUE)
  return (c(medianIBU, medianABV))
}

#Add the space in the StateList dataframe for the additional values
StateList$IBU = c(1:length(StateList$State))
StateList$ABV = c(1:length(StateList$State))

#Get the median values for IBU and ABV for each state
for (states in StateList$StateList) { 
  StateList[which(StateList$StateList == states),"IBU"] = getStateIBU(states)
  StateList[which(StateList$StateList == states),"ABV"] = getStateABV(states)*100
}

#Get rid of the NA IBU value for South Dakota
StateList[which(is.na(StateList$IBU)),"IBU"] = 0

#Fix the names
names(StateList) = c("State", "IBU", "ABV") 

#Now plot the data

PlotList = melt(StateList, id.vars = "State", measure.vars = c("IBU","ABV"))

#Bar plot of the IBU and ABV data by state

library(ggplot2)
ggplot(PlotList, aes(reorder(PlotList$State,PlotList$value), PlotList$value)) +   
  geom_bar(aes(fill = PlotList$variable), position = "dodge", stat="identity") + xlab("State") + ylab("IBU Value / ABV Percent") + ggtitle("Median Alcohol By Vol. (ABV) and Bitterness (IBU) By State") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme_minimal() + coord_flip()

#Scatterplot of ABV vs IBU

#Note: Plot drops NA Values

ggplot(MergedBrewing, aes(x=ABV*100, y=IBU)) +
  geom_point(shape=3) +    # Use hollow circles
  geom_smooth(method=lm) + xlab("Alcohol By Volume (%)") + ylab("International Bitterness Units (IBU)") + ggtitle("Relationship Between Bitterness and Alcohol By Volume") + theme_minimal()
