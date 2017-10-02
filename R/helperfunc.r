##
## [helperfunc.r]
##
## author     : Ed Goodwin
## project    : helpeR
## createdate : 01.14.2015
##
## description:
##    a set of useful functions that I can drop into various projects
##
## version: 0.01
## changelog:
##

require(sqldf)
require(dplyr)

as.numeric.com = function(y) {
  ## converts string with commas into single number
  as.numeric(gsub(",","", y))
}

find.freq <- function(x) {
  n <- length(x)
  spec <- spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}


## convert factor vector to numeric
factor2numeric = function(facvect) {
  numeric = as.numeric(levels(facvect)[facvect])
  numeric
}

## useful for using ggplot with forecast package (Holt-Winters, etc.)


#--Produces a data.frame with the Source Data+Training Data, Fitted Values+Forecast Values, forecast data Confidence Intervals
funggcast<-function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
  return(pd)
  
}

## return the days of month in a year; need to improve to handle leap years
monthdays = function(year) {
  rep(c(31,28,31,30,31,30,31,31,30,31,30,31),1)
  monthdays[26 + (4*12)*(0:2)] <- 29  # leap years
}

## return number of days in month based on a date

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

## forecast accuracy functions
mape = function(y,yhat) {
  mean(abs(y-yhat)/y)
}

smape = function(y, yhat) {
  mean(200*abs(y-yhat)/(y+yhat))
}

# ## panel histogram for correlation matrix
# panel.hist = function(x){
#   usr = par(usr)
#   on.exit(par(usr))
#   par(usr = c(usr[1:2], 0, 1.5))
#   h = hist(x, plot=FALSE)
#   breaks = h$breaks; nB = length(breaks)
#   y = h$counts; y = y/max(y)
#   rect(breaks[-1], y, col=“cyan”)
# }
## example: paris(credit[,-(4:5)], diag.panel=panel.hist)


## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

# # Multiple plot function
# #
# # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# # - cols:   Number of columns in layout
# # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# #
# # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# # then plot 1 will go in the upper left, 2 will go in the upper right, and
# # 3 will go all the way across the bottom.
# #
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   p = 0
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     p = (plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       p = plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col)
#     }
#   }
#   return(p)
# }

## read small subset of the data for a file
## uses sqldf so that you can read large data files
readsubsetfile = function(fname, nlines) {
  sqlq = paste("select * from ", fname, "order by random(*) limit ", nlines)
  subfile = read.csv.sql(fname, sql = sqlq)
  subfile
}

# ## number of weekdays between two dates
# Dates1 <- as.Date("2011-01-30") + rep(0, 10)
# Dates2 <- as.Date("2011-02-04") + seq(0, 9)
# Nweekdays(Dates1, Dates2)
Nweekdays <- Vectorize(function(a, b) 
  sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday")))

## create test/training sets from dataframes
testtrainsplit = function(df, trainsize) {

  #Sample Indexes
  indexes = sample(1:nrow(df), size=trainsize*nrow(df))
  
  # Split data
  test = df[indexes,]
  train = df[-indexes,]
  return(c(train, test))
}
