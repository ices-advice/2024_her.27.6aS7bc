## Run analysis, write model results

## Before: survey_catch.csv, length_frequency.csv (data)
## After:   length_values.csv, length_frequency.csv, settings.RData (model)

library(icesTAF)
library(icesAdvice)
library (tidyverse)


mkdir("model")

## Get catch and survey data
survey_catch <- read.taf("data/survey_catch.csv")
# Get Length frequency data
length_frequency <- read.taf("data/length_frequency.csv")


## Settings -----
current_year <- max(survey_catch$Year) # last data year
advised_catch <- 2270 # advised catch in last year (change this to be read from the input file in the future)
multiplier <- 0.5 # from the ICES guidelines

## Set life history parameters and natural mortality estimates calculated externally -----
k_value <- 0.339 #von bertalanfy k calculated from catch sampling length data
Loo <- 30.5 #L infinity calculated from catch sampling length data
M <- 0.220 #average natural mortality ages 3-6
gamma <- 1 # gamma value set to 1
theta <- k_value/M # theta value set to k over M
last_3_years <- seq (current_year-2, current_year, 1) # last three years

# F  proxy MSY - calculate again or set the value
# unless major changes in fishery or assessment, keep the FproxyMSY calculated in the benchmark 2022
Calc_FproxyMSY <- FALSE #if true a new FproxyMSY will be calculated and subsequently used in the chr
FproxyMSY <- 0.034 #3 decimal places

## biomass and catch data values -----

# average of the last 3 years catch
C_last_3 <- mean (survey_catch$ICES_landings[survey_catch$Year %in% last_3_years])

# most recent index data value
I_last <- survey_catch$Survey_Index[survey_catch$Year == current_year]

# lowest biomass index value and the year that this occurred in
I_loss <- c(min(survey_catch$Survey_Index), survey_catch$Year[survey_catch$Survey_Index == min(survey_catch$Survey_Index)])

# Index trigger value
I_trigger <- 1.4*I_loss[1]

# Biomass safeguard - when the most recent index is greater than I_trigger safegaurd is set equal to 1
b_safeguard <- min (1, (I_last/I_trigger))


## extracting modal length, Lc and mean length >Lc from length frequency data -----

length_values <- data.frame(Year = NA, Modal_catch = NA, Lc = NA, MeanL_c = NA, Median = NA)

for(i in colnames(length_frequency[-1])){
  
  # subset for year
  data <- select(length_frequency, Length, all_of(i)) 
  colnames(data) <- c("Length", "Number")
  data <- na.omit (data)
  
  data$f_X <- data$Length * data$Number # f*x = length * number
  
  sum_n <- sum (data$Number) # sum of number
  sum_fx <- sum (data$f_X) # sum of f*x
  
  full_data_mean <- sum_fx/sum_n # full data mean = sum_fx/sum_n
  
  data$x_u_2 <- (data$Length-full_data_mean)^2 # length-full data mean to power of 2
  data$f_x_u_2 <- data$Number * data$x_u_2 # then multiply by number
  
  sum_x_u_2 <- sum (data$x_u_2)
  sum_f_x_u_2 <- sum (data$f_x_u_2)
  
  max_n <- max (data$Number) # max value of n
  max_n2 <- max_n/2 # divide this by 2
  
  mode <- data$Length[data$Number == max(data$Number)] # calculate the mode
  if(length(mode) > 1){ mode <- mode[ceiling(length(mode)/2)] } 
  
  data$cumulative_n <- cumsum (data$Number)# cumulative n
  
  median <- data$Length[which.min(abs(data$cumulative_n -  ((sum_n+1)/2)))] # median = halfpoint of sum of number
  
  st_dev <- sqrt(sum_f_x_u_2/sum_n) # standard dev
  
  # length at first capture 
  mode_cell <- data$Length[data$Length == mode] # length where we find the mode
  mode_number <- data$Number[data$Length == mode] # number where we find the mode
  
  target <- mode_number-max_n2 # modal number - half of the maximum of number
  
  Lc <- data$Length [which.min (abs(data$Number[data$Length<mode_cell] - target))]
  # length where the number (when length is below the mode)-'target' is
  
  # mean above length of first capture
  over_Lc_length <- Lc+0.5 # first length over Lc
  
  mean_over_Lc <- sum(data$f_X[data$Length>=over_Lc_length])/sum(data$Number[data$Length>=over_Lc_length])
  # sum of f_x divided by sum of number where length is over Lc
  
  # put modal catch, Lc and mean at lengths above Lc into data frame
  length_values <- rbind(length_values, c(c(as.numeric(i),mode, Lc, mean_over_Lc, median)))
  
  # remove variables from environment
  rm("data", "full_data_mean", "Lc","max_n","max_n2", "mean_over_Lc", "median","mode_cell",
     "mode_number", "over_Lc_length", "st_dev", "sum_f_x_u_2","sum_fx", "sum_n","sum_x_u_2" )
}


# Join length values to survey and catch dataframe
length_values <- left_join(survey_catch, length_values[-1,], by = 'Year')


## calculating FproxyMSY and rate of change -----
# calculate target reference length for each year using Jardim et al equation
length_values$TargetRefLen <- ( (theta*Loo) + (length_values$Lc*(gamma+1)) ) / (theta + gamma + 1)

length_values$f <- length_values$MeanL_c / length_values$TargetRefLen

length_values$CyIy <- length_values$ICES_landings / length_values$Survey_Index

length_values

# Calculate FproxyMSY and add to data frame (if Calc_FproxyMSY set to TRUE)
if(Calc_FproxyMSY == TRUE){
FproxyMSY <- mean (length_values$CyIy [length_values$f >= 1]) # Fproxy = Catch over biomass where f>1
}

# Add and round FproxyMSY and other length calculations to df
length_values$Fproxy <- round(FproxyMSY,3)
length_values$f <- round(length_values$f,3)
length_values$TargetRefLen <- round(length_values$TargetRefLen,3)
length_values$CyIy <- round(length_values$CyIy,3)
length_values$MeanL_c  <- round(length_values$MeanL_c ,3)



## Write out the csv files
write.taf(length_values, dir="model")
write.taf(length_frequency, dir="model")

# Save settings
save(advised_catch, b_safeguard, C_last_3, current_year, FproxyMSY, gamma, I_last,
     I_loss, I_trigger, k_value, last_3_years, Loo, M, multiplier, theta,
     file='model/settings.Rdata')









