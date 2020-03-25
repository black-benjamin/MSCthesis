library(tidyverse)

#remant chunk for if Decide to reintroduce the Outcome crosschecking after changing the data for it.
#read in 201x-201x control excluding NA values in Precip, temp and soils columns 
Control_201x_201x <- read.csv("D:/Bens stuff/MSCthesis/data/201x_201x_control_sample.csv") %>% subset(complete.cases(Precip & Temp & Soil)) 
#Converting NA values in the outcome variable columns to 0
Control_201x_201x$FCL_out[is.na(Control_201x_201x$FCL_out)]  <- 0
Control_201x_201x$Out_check[is.na(Control_201x_201x$Out_check)]  <- 0


#read in 2010-2012 control excluding NA values in Precip, temp and soils columns and the Out_check column 
Control_2010_2012 <- read.csv("D:/Bens stuff/MSCthesis/data/2010_2012_control_sample.csv") %>% subset(complete.cases(Precip & Temp & Soil), select = -c(Out_check)) 
#Converting NA values in the outcome variable columns to 0
Control_2010_2012$FCL_out[is.na(Control_2010_2012$FCL_out)]  <- 0

#Use this line to double check that NA values have been removed
which(is.na(Control_201X_201X$....))

#read in 2010-2012 treated 
Treated_2010_2012 <- read.csv("D:/Bens stuff/MSCthesis/data/2010_2012_treated_sample.csv") %>% subset(Precip != 0 & Temp !=0 & Soil !=0, select = -c(Out_check))

#read in 2013-2015 control excluding NA values in Precip, temp and soils columns and Out_check column 
Control_2013_2015 <- read.csv("D:/Bens stuff/MSCthesis/data/2013_2015_control_sample.csv") %>% subset(complete.cases(Precip & Temp & Soil), select = -c(Out_check)) 
#Converting NA values in the outcome variable columns to 0
Control_2013_2015$FCL_out[is.na(Control_2013_2015$FCL_out)]  <- 0


#read in 2013-2015 Treated excluding NA values in Precip, temp and soils columns and Out_check column 
Treated_2013_2015 <- read.csv("D:/Bens stuff/MSCthesis/data/2013_2015_treated_sample.csv") %>% subset(complete.cases(Precip & Temp & Soil), select = -c(Out_check)) 
#Converting NA values in the outcome variable columns to 0
Treated_2013_2015$FCL_out[is.na(Treated_2013_2015$FCL_out)]  <- 0


#read in 2016-2018 control excluding NA values in Precip, temp and soils columns and Out_check column
Control_2016_2018 <- read.csv("D:/Bens stuff/MSCthesis/data/2016_2018_control_sample.csv") %>% subset(complete.cases(Precip & Temp & Soil), select = -c(Out_check)) 
#Converting NA values in the outcome variable columns to 0
Control_2016_2018$FCL_out[is.na(Control_2016_2018$FCL_out)]  <- 0

#read in 2016-2018 treated excluding NA values in Precip, temp and soils columns and Out_check column
Treated_2016_2018 <- read.csv("D:/Bens stuff/MSCthesis/data/2016_2018_treated_sample.csv") %>% subset(complete.cases(Precip & Temp & Soil), select = -c(Out_check)) 
#Converting NA values in the outcome variable columns to 0
Treated_2016_2018$FCL_out[is.na(Treated_2016_2018$FCL_out)]  <- 0
str(Treated_2016_2018)

#Appending control and sample dataframes for each outcome period and creating a master dataset

## First it is necessary to remove the info related to the PAs from the treated sample as otherwise the columns numbers do not match.
Treated_2010_2012_redact <- subset(Treated_2010_2012, select = -c(PA_name,PA_cat, PA_year, PA_area))
Treated_2013_2015_redact <- subset(Treated_2013_2015, select = -c(PA_name,PA_cat, PA_year, PA_area))
Treated_2016_2018_redact <- subset(Treated_2016_2018, select = -c(PA_name,PA_cat, PA_year, PA_area))

#Then use Rbind to perform the appending
Combined_2010_2012 <- rbind(Control_2010_2012, Treated_2010_2012_redact)
Combined_2013_2015 <- rbind(Control_2013_2015, Treated_2013_2015_redact)
Combined_2016_2018 <- rbind(Control_2016_2018, Treated_2016_2018_redact) 
Master_dataset <- rbind(Combined_2010_2012, Combined_2013_2015, Combined_2016_2018)

#Building a generalised linear model including the covariates with a binomial distribution (logit link function)
model1 <- Combined_2010_2012 %>% glm(formula = FCL_out ~ SFCL + ELC_Dist + Pop + Slope + Precip + Temp + Elevation + Cap_dist + Border_dist + Road_dist + Soil, family = binomial())
summary(model1)
