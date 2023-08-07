
Airbnb Project




aListings <- read.csv("~/Desktop/MSBX R/AIrbnb/denver_listings.csv")
aCalendar <- read.csv("~/Desktop/MSBX R/AIrbnb/denver_calendar.csv")
aReviews <- read.csv("~/Desktop/MSBX R/AIrbnb/denver_reviews.csv")
#----------------------------------------------------------------------
#QUESTION ONE

nRange <- subset(aListings, select=c('id', 'neighbourhood', 'neighbourhood_cleansed', 'price'))
nRange <- subset(nRange, nRange$neighbourhood == 'Denver, Colorado, United States')
#This will create a subset of the data with all neighborhoods in Denver, Colorado with columns
#we will need to refer to.

#This will convert the price column to numeric and remove any NA outputs

nRange$nRangeFixed <- as.numeric( sub("$", "", nRange$price, fixed = TRUE))
nRange <- subset(nRange,nRange$nRangeFixed != "NA")
#At this point we are incorporating nRangeFixed into the dataset and removing all empty data values

avgPrice <- aggregate (nRangeFixed ~ neighbourhood_cleansed, FUN = mean, data = nRange)

#Here we grouped by neighbourhoods to find the average price per neighbourhood to rent an airbnb

summary(avgPrice)
#here we want to know the summary of the averages of all the neighborhoods.
sd(avgPrice$nRangeFixed)
cheap<- round((mean(avgPrice$nRangeFixed) - (sd(avgPrice$nRangeFixed)*2)),2)
cheap
#here we want to know which neighbourhoods fall below 2 standard deviations from the mean to form our definition of cheap.
firstQ <- subset(avgPrice, nRangeFixed <= cheap)
firstQ
#This will return all properties below 2SD of mean. As there is only 1 Neighbourhood returned, no graph is necessary


expensive<- round((mean(avgPrice$nRangeFixed) + (sd(avgPrice$nRangeFixed)*2)),2)
expensive
#here we want to know which neighbourhoods fall above 2 standard deviations from the mean to form our definition of expensive.

thirdQ <- subset(avgPrice, nRangeFixed >= expensive)
thirdQ
#This will return all properties above 2SD of mean.
barplot(thirdQ$nRangeFixed, main = "Average Cost per Neighbourhood", xlab = "neighbourhoods", ylab = "Average Price", names.arg = thirdQ$neighbourhood_cleansed, col = "darkred", horiz = FALSE)

#----------------------------------------------------------------------
#QUESTION 2

#For this question we located the address of the house on google maps and verified that it is
#located in the neighborhood five points. Referring to the subset that we created, we can
#see that the average cost of an airbnb in this area is around $185.19

fivePoints <- subset(aListings, select=c('id','neighbourhood_cleansed','property_type','room_type','bedrooms','bathrooms_text','price'))

fivePoints <- subset(fivePoints, neighbourhood_cleansed == 'Five Points')
fivePoints <- subset(fivePoints, property_type == 'Entire home')
fivePoints <- subset(fivePoints, bedrooms == 3)
fivePoints <- subset(fivePoints, bathrooms_text == '2 baths')
priceFixed <- as.numeric( sub ('$',"",fivePoints$price, fixed = TRUE))

fivePointsAvg <- aggregate( priceFixed ~ neighbourhood_cleansed, FUN = mean, data = fivePoints)
fivePointsAvg
#In this line of code we created a subset across all listings of property that match the parameters of the property presented in Zillow.
# This left us with 6 points of data where we took the average of the remaining listings

fivePointsAlt <- subset(aListings,select=c('id','neighbourhood_cleansed','property_type','room_type','bedrooms','bathrooms_text','price'))
fivePointsAlt <- subset(fivePointsAlt, neighbourhood_cleansed == 'Five Points')
fivePointsAlt <- subset(fivePointsAlt, room_type == 'Private room')
fixedPrice <- as.numeric( sub ('$',"",fivePointsAlt$price, fixed = TRUE))
newAvg <- aggregate( fixedPrice ~ neighbourhood_cleansed, FUN = mean, data = fivePointsAlt)
newAvg
#here we took an alternative approach to list the price. We took the average price of a private room in five points and compared that against renting the entire property to one person.

roomRent <- newAvg$fixedPrice * 3
roomRent <- roomRent * 365

houseRent <- fivePointsAvg$priceFixed * 365

profDiff <- roomRent - houseRent
profDiff
#here we can see that it is more profitable to rent private rooms in the house as opposed to the entire property.
#[this is a good opportunity to plot out a payoff table based off the two different sets of data]

price_on_zillow <- 885000

room_pay_off<-round(price_on_zillow/roomRent,2)
room_pay_off

house_pay_off <- round(price_on_zillow/houseRent,2)
house_pay_off

comp_pay_off<- house_pay_off - room_pay_off
comp_pay_off
# Assumptions:
#1. Property is rented out year round 
#2. No downpayment is given at the time of purchase
#3. Each room in the house is rented in regard to room rentals
#4. All profit is being put into the principal amount and used to pay off the house loan

#conc: It would take the owner approx 7-8 years to pay off the loan as opposed to a standard 20-30 year loan pay off; with a difference of approx saving 3 months of choosing to rent each room individually rather than the entire property.


#----------------------------------------------------------------------
#QUESTION 3

shortTerm <- subset(aListings, select=c('id', 'neighbourhood', 'neighbourhood_cleansed', 'price', 'maximum_maximum_nights','review_scores_location'))
shortTerm <- subset(shortTerm, shortTerm$neighbourhood == 'Denver, Colorado, United States')
shortTerm <- subset(shortTerm,shortTerm$review_scores_location != "NA")
shortTerm<- subset(shortTerm, maximum_maximum_nights<30 )

#In this line of code, we are trying to find all short term listings, defined by Colorado state law : is any property rented for less than 30 nights

bestLoc <- aggregate(review_scores_location ~ neighbourhood_cleansed, FUN = mean, data = shortTerm)
summary(bestLoc)
top<- subset(bestLoc, review_scores_location >= 4.94)
# After condensing our list, we took 3Q of the mean ratings of each neighbourhood to identify which properties are in the most favourable locations
top <- merge(top, avgPrice, by = 'neighbourhood_cleansed')
top<-top[order(top$nRangeFixed,decreasing=TRUE),]
top3<-top[1:3,]
top3
# Here, we wanted to narrow our listings by incorporating Average price per neighbourhood back into the data, to analyze which favourable neighbourhoods are the most profitable.
# We limited our findings to the top 3 most favourable and profitable neighbourhoods in order to make it easier on those looking to invest. 








