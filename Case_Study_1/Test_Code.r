library(reshape2)

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

getStateStats = function (State) { 
  ibuList = MergedBrewing[which(MergedBrewing$State == State),]$IBU
  abvList = MergedBrewing[which(MergedBrewing$State == State),]$ABV
  medianIBU = median(ibuList, na.rm = TRUE)
  medianABV = median(abvList, na.rm = TRUE)
  return (c(medianIBU, medianABV))
}

#This is messy, but I'm trying to avoid a for loop
StateList = cbind(StateList, t(as.data.frame(lapply(StateList, getStateStats))))
row.names(StateList) = c(1:length(row.names(StateList)))
StateList = as.data.frame(StateList)
names(StateList) = c("State", "IBU", "ABV") 
StateList$IBU = as.numeric(StateList$IBU)
StateList$ABV = as.numeric(StateList$ABV)
StateList[which(is.na(StateList$IBU)),"IBU"] = 0
#Now plot the data

PlotList = melt(StateList, id.vars = "State", measure.vars = c("IBU","ABV"))


library(ggplot2)
ggplot(PlotList, aes(State, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))