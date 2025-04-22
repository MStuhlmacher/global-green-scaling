#AUTHOR: Michelle Stuhlmacher, PhD
#LAST UPDATED: 2022-08-29

#GOAL: Analysis and figures for results presented in text
#(see appendix.R for results presented in SI)

#STEPS:
#1. Load in data and packages
#2. Test data to see if it meets SMA assumptions
#3. Global green space scaling with population
#4. Local green space scaling with city size

## STEP 1 ---------------------------------------------------------------
# Load in data and packages
library(ggplot2)
library(SciViews)
library(moments)
library(FSA)
library(smatr)
library(RColorBrewer)
library(gridExtra)
library(matlab)
library(dplyr)
#library(raster)

#Set working directory
setwd("C:/Users/mstuhlm1/OneDrive - DePaul University/Research/GlobalUrbanGreenspace") #DePaul laptop

##load in metric data
#all concentric circles:
metricDF = read.csv('./Data/LandscapeMetrics/LandscapeMetrics_allCities_2023.03.21.csv')
metricDF$circleAreaKm2 = (pi*(metricDF$radius)^2)/1000000

#outer bounding circle (GEE export)
DF = read.csv('./Data/CityStats/BoundingCircle_allCities_2023.03.21.csv')

#outer bounding circle (metrics)
bcDF = read.csv('./Data/LandscapeMetrics/LandscapeMetrics_OuterBounds_allCities_2023.03.21.csv')

DF = merge(DF,bcDF,by = 'City')

#Calculate the area of the bounding circle from the radius
DF$circleAreaKm2 = (pi*(DF$radius.x)^2)/1000000
DF$IncGroup = factor(DF$IncomeClas, levels = c("H", "UM", "LM", "L"))

#Add biome names
DF$MOIST_NAME = ifelse(DF$MOIST_NUM == 1, "Desert",
                       ifelse(DF$MOIST_NUM == 2, "Dry",
                              ifelse(DF$MOIST_NUM == 3, "Moist",
                                     "ERROR")))
DF$MoistGroup = factor(DF$MOIST_NAME)

DF$TEMP_NAME = ifelse(DF$TEMP_NUM == 1, "Boreal",
                      ifelse(DF$TEMP_NUM == 2, "Cool Temperate",
                             ifelse(DF$TEMP_NUM == 3, "Polar",
                                    ifelse(DF$TEMP_NUM == 4, "Sub Tropical",
                                           ifelse(DF$TEMP_NUM == 5, "Tropical",
                                                  ifelse(DF$TEMP_NUM == 6, "Warm Temperate",
                                                         "ERROR"))))))
DF$TempGroup = factor(DF$TEMP_NAME, levels = c("Cool Temperate","Warm Temperate","Sub Tropical","Tropical","Boreal"))

#for local scaling (step 4) drop any cities with fewer than 3 radii (reduces the number of cities from 150 to 124). NEED TO NOTE THIS CHANGE
DF_3 = subset(DF,radius.x >= 8000)

## STEP 2 ---------------------------------------------------------------
# Test data to see if it meets SMA assumptions

#----1. Visualize if green space metrics are distributed normally----
#BOUNDING CIRCLE AREA
ggplot(DF, aes(x=ln(circleAreaKm2)))+
  ggtitle("Bounding Circle Area (ln)")+
  geom_density(alpha=0.3) 

#POPULATION
ggplot(DF, aes(x=ln(pop2015)))+
  ggtitle("Population (ln)") +
  geom_density(alpha=0.3) 

#PLAND GREEN
ggplot(DF, aes(x=ln(plandG)))+
  ggtitle("Percent Green (ln)") +
  geom_density(alpha=0.3) 

#MEAN PATCH AREA
ggplot(DF, aes(x=ln(areamnG)))+
  ggtitle("Mean Patch Area (ln)") +
  geom_density(alpha=0.3)

#AGGREGATION INDEX
ggplot(DF, aes(x=ln(aiG)))+
  ggtitle("AI (ln)") +
  geom_density(alpha=0.3)

#----2. calculate skewness----
#If skewness value lies above +1 or below -1, data is highly skewed.
#If it lies between +0.5 to -0.5, it is moderately skewed. If the value is 0, then the data is symmetric

skewness(ln(DF$circleAreaKm2))
skewness(ln(DF$pop2015))
skewness(ln(DF$plandG))
skewness(ln(DF$areamnG))
skewness(ln(DF$aiG))

#----3. Perform the shapiro test----
#p-value > 0.05 shows that the distribution of the data are not significantly different from normal distribution

shapiro.test(ln(DF$circleAreaKm2)) 
shapiro.test(ln(DF$pop2015)) 
shapiro.test(ln(DF$plandG)) 
shapiro.test(ln(DF$areamnG)) 
shapiro.test(ln(DF$aiG))

#----4. Plot using a qqplot----
qqnorm(ln(DF$circleAreaKm2));qqline(ln(DF$circleAreaKm2), col=2)
qqnorm(ln(DF$pop2015));qqline(ln(DF$pop2015), col=2)
qqnorm(ln(DF$plandG));qqline(ln(DF$plandG), col=2)
qqnorm(ln(DF$areamnG));qqline(ln(DF$areamnG), col=2)
qqnorm(ln(DF$aiG));qqline(ln(DF$aiG), col=2)

#----5. Examine the ranked correlation and residuals----
##BOUNDING CIRCLE AREA##
#Bounding circle area and PLAND green (no pattern)
ggplot(DF, aes(x = rank(circleAreaKm2), y = rank(plandG))) +
  geom_point()
#residuals:
BCA_1 = lm(rank(plandG)~rank(circleAreaKm2), data=DF)
BCA_1_res = resid(BCA_1)
plot(fitted(BCA_1),BCA_1_res)
qqnorm(BCA_1_res);qqline(BCA_1_res, col=2)

#Bounding circle area and AREA_MN (moderately V-shaped +)
ggplot(DF, aes(x = rank(circleAreaKm2), y = rank(areamnG))) +
  geom_point()
#residuals:
BCA_4 = lm(rank(areamnG)~rank(circleAreaKm2), data=DF)
BCA_4_res = resid(BCA_4)
plot(fitted(BCA_4),BCA_4_res)
qqnorm(BCA_4_res);qqline(BCA_4_res, col=2)

#Bounding circle area and AI (moderately V-shaped +)
ggplot(DF, aes(x = rank(circleAreaKm2), y = rank(aiG))) +
  geom_point()
#residuals:
BCA_6 = lm(rank(aiG)~rank(circleAreaKm2), data=DF)
BCA_6_res = resid(BCA_6)
plot(fitted(BCA_6),BCA_6_res)
qqnorm(BCA_6_res);qqline(BCA_6_res, col=2)

##POPULATION##
#Population and PLAND green (no pattern)
ggplot(DF, aes(x = rank(pop2015), y = rank(plandG))) +
  geom_point()
#residuals:
P_1 = lm(rank(plandG)~rank(pop2015), data=DF)
P_1_res = resid(P_1)
plot(fitted(P_1),P_1_res)
qqnorm(P_1_res);qqline(P_1_res, col=2)

#Population and AREA_MN (moderately V-shaped +)
ggplot(DF, aes(x = rank(pop2015), y = rank(areamnG))) +
  geom_point()
#residuals:
P_4 = lm(rank(areamnG)~rank(pop2015), data=DF)
P_4_res = resid(P_4)
plot(fitted(P_4),P_4_res)
qqnorm(P_4_res);qqline(P_4_res, col=2)

#Population and AI (slightly V-shaped)
ggplot(DF, aes(x = rank(pop2015), y = rank(aiG))) +
  geom_point()
#residuals:
P_6 = lm(rank(aiG)~rank(pop2015), data=DF)
P_6_res = resid(P_6)
plot(fitted(P_6),P_6_res)
qqnorm(P_6_res);qqline(P_6_res, col=2)

## STEP 3 ---------------------------------------------------------------
# Global green space scaling with population (Figure 2)

##----By income group----
#Population and PLAND green (natural log)
pv1 = sma(ln(DF$plandG)~ln(DF$pop2015))
summary(pv1)
plot(pv1)
plot(pv1, which = "residual")
plot(pv1, which = "qq")

pv2 = sma(ln(DF$plandG)~ln(DF$pop2015)*DF$IncGroup) 
summary(pv2)
plot(pv2)
#plot(pv2, which = "residual")
#plot(pv2, which = "qq")

pop_pland = ggplot(DF, aes(x = ln(pop2015),y = ln(plandG), color = IncGroup)) +
  geom_point() +
  scale_color_brewer(palette = "PuOr") +
  #scale_color_brewer(palette = "PuOr",name = "Income Group", labels = c("High", "Upper Middle", "Lower Middle", "Low")) + #for labeling legend
  xlab(expression(paste("ln(population)"))) +
  ylab(expression(paste("ln(percent area covered by green space)"))) +
  ggtitle("A. Area") +
  theme_minimal() +
  #H
  geom_abline(slope=as.numeric(pv2$coef$H[2,1]),intercept=as.numeric(pv2$coef$H[1,1]), size = 1, linetype = 2, color = "#e66101") +
  #UM
  geom_abline(slope=as.numeric(pv2$coef$UM[2,1]),intercept=as.numeric(pv2$coef$UM[1,1]), size = 1, color = "#fdb863") +
  #LM
  geom_abline(slope=as.numeric(pv2$coef$LM[2,1]),intercept=as.numeric(pv2$coef$LM[1,1]), size = 1, linetype = 2, color = "#b2abd2") +
  #L
  geom_abline(slope=as.numeric(pv2$coef$L[2,1]),intercept=as.numeric(pv2$coef$L[1,1]), size = 1, linetype = 2, color = "#5e3c99") +
  theme(legend.position = "none")

##Population and mean patch area (natural log)
gv1 = sma(ln(DF$areamnG)~ln(DF$pop2015))
summary(gv1)
plot(gv1)

gv2 = sma(ln(DF$areamnG)~ln(DF$pop2015)*DF$IncGroup)
summary(gv2)
plot(gv2)

pop_area = ggplot(DF, aes(x = ln(pop2015),y = ln(areamnG), color = IncGroup)) +
  geom_point() +
  scale_color_brewer(palette = "PuOr") +
  xlab(expression(paste("ln(population)"))) +
  ylab(expression("ln(average area of green space patches: ha)")) +
  theme_minimal() +
  ggtitle("B. Average Patch Area") +
  #H
  geom_abline(slope=as.numeric(gv2$coef$H[2,1]),intercept=as.numeric(gv2$coef$H[1,1]), size = 1, color = "#e66101") +
  #UM
  geom_abline(slope=as.numeric(gv2$coef$UM[2,1]),intercept=as.numeric(gv2$coef$UM[1,1]), size = 1, color = "#fdb863") +
  #LM
  geom_abline(slope=as.numeric(gv2$coef$LM[2,1]),intercept=as.numeric(gv2$coef$LM[1,1]), size = 1, color = "#b2abd2") +
  #L
  geom_abline(slope=as.numeric(gv2$coef$L[2,1]),intercept=as.numeric(gv2$coef$L[1,1]), size = 1, color = "#5e3c99") +
  theme(legend.position = "none")

##Population and ai 
gz1 = sma(ln(DF$aiG)~ln(DF$pop2015))
summary(gz1)

gz2 = sma(ln(DF$aiG)~ln(DF$pop2015)*DF$IncGroup)
summary(gz2)
plot(gz2)

pop_ai = ggplot(DF, aes(x = ln(pop2015),y = ln(aiG), color = IncGroup)) +
  geom_point() +
  scale_color_brewer(palette = "PuOr") +
  xlab(expression(paste("ln(population)"))) +
  ylab(expression(paste("ln(green space connectedness)"))) +
  theme_minimal() +
  ggtitle("C. Connectedness") +
  #H
  geom_abline(slope=as.numeric(gz2$coef$H[2,1]),intercept=as.numeric(gz2$coef$H[1,1]), size = 1, color = "#e66101") +
  #UM
  geom_abline(slope=as.numeric(gz2$coef$UM[2,1]),intercept=as.numeric(gz2$coef$UM[1,1]), size = 1, color = "#fdb863") +
  #LM
  geom_abline(slope=as.numeric(gz2$coef$LM[2,1]),intercept=as.numeric(gz2$coef$LM[1,1]), size = 1, color = "#b2abd2") +
  #L
  geom_abline(slope=as.numeric(gz2$coef$L[2,1]),intercept=as.numeric(gz2$coef$L[1,1]), size = 1, color = "#5e3c99") +
  theme(legend.position = "none")

#Combine in one plot:
grid.arrange(pop_pland,pop_area,pop_ai,ncol = 3)

##----By biome----
#PLAND
pb1_b = sma(ln(DF$plandG)~ln(DF$pop2015))
summary(pb1_b)

pb2_b = sma(ln(DF$plandG)~ln(DF$pop2015)*DF$TempGroup)
summary(pb2_b)
plot(pb2_b)
plot(pb2_b, which = "residual")
plot(pb2_b, which = "qq")

#Dropped because there are fewer than 3 cities:
#Boreal (and Polar)

#Biomes that are stat sig:
#Tropical 

#Biomes that are not stat sig:
#Cool Temperate 
#Sub Tropical 
#Warm Temperate 

PB_pland_b = ggplot(DF, aes(x = ln(pop2015),y = ln(plandG), color = TempGroup)) +
  geom_point() +
  scale_color_brewer(name = "Climate",palette = "Paired") +
  xlab(expression(paste("ln(population)"))) +
  ylab(expression(paste("ln(percent area covered by green space)"))) +
  ggtitle('D. Area')+
  theme_minimal() +
  scale_y_continuous(limits=c(min(ln(DF$plandG)),max(ln(DF$plandG))))+
  #Cool Temperate (not sig)
  geom_abline(slope=as.numeric(pb2_b$coef$`Cool Temperate`[2,1]),intercept=as.numeric(pb2_b$coef$`Cool Temperate`[1,1]),size = 1,linetype = 2,color = "#a6cee3") +
  #Warm Temperate (not sig)
  geom_abline(slope=as.numeric(pb2_b$coef$`Warm Temperate`[2,1]),intercept=as.numeric(pb2_b$coef$`Warm Temperate`[1,1]),size = 1,linetype = 2,color = "#1f78b4") +
  #Sub Tropical (not sig)
  geom_abline(slope=as.numeric(pb2_b$coef$`Sub Tropical`[2,1]),intercept=as.numeric(pb2_b$coef$`Sub Tropical`[1,1]), size = 1,linetype=2, color = "#b2df8a") +
  #Tropical (sig)
  geom_abline(slope=as.numeric(pb2_b$coef$Tropical[2,1]),intercept=as.numeric(pb2_b$coef$Tropical[1,1]),size = 1,color = "#33a02c") +
  theme(legend.position = "none")

#AREA_MN
area2_b = sma(ln(DF$areamnG)~ln(DF$pop2015)*DF$TempGroup)
summary(area2_b)
plot(area2_b)
#plot(pd2_b, which = "residual")
#plot(pd2_b, which = "qq")

#Dropped because there are fewer than 3 cities:
#Boreal (and Polar)

#Biomes that are stat sig:
#Sub Tropical
#Tropical
#Warm Temperate

#Biomes that are not stat sig:
#Cool Temperate

PB_area_b = ggplot(DF, aes(x = ln(pop2015),y = ln(areamnG), color = TempGroup)) +
  geom_point() +
  scale_color_brewer(name = "Temperature Regimes",palette = "Paired") +
  xlab(expression(paste("ln(population)"))) +
  ylab(expression("ln(average area of green space patches: ha)")) +
  theme_minimal() +
  ggtitle("E. Average Patch Area") +
  scale_y_continuous(limits=c(min(ln(DF$areamnG)),max(ln(DF$areamnG))))+
  #Cool Temperate (not sig)
  geom_abline(slope=as.numeric(area2_b$coef$`Cool Temperate`[2,1]),intercept=as.numeric(area2_b$coef$`Cool Temperate`[1,1]),size = 1,linetype = 2,color = "#a6cee3") +
  #Warm Temperate (sig)
  geom_abline(slope=as.numeric(area2_b$coef$`Warm Temperate`[2,1]),intercept=as.numeric(area2_b$coef$`Warm Temperate`[1,1]),size = 1,color = "#1f78b4") +
  #Sub Tropical (sig)
  geom_abline(slope=as.numeric(area2_b$coef$`Sub Tropical`[2,1]),intercept=as.numeric(area2_b$coef$`Sub Tropical`[1,1]), size = 1,color = "#b2df8a") +
  #Tropical (sig)
  geom_abline(slope=as.numeric(area2_b$coef$Tropical[2,1]),intercept=as.numeric(area2_b$coef$Tropical[1,1]),size = 1,color = "#33a02c") +
  theme(legend.position = "none")

#AI
ai2_b = sma(ln(DF$aiG)~ln(DF$pop2015)*DF$TempGroup)
summary(ai2_b)
plot(ai2_b)
#plot(ai2_b, which = "residual")
#plot(ai2_b, which = "qq")

#Dropped because there are fewer than 3 cities:
#Boreal (and Polar)

#Biomes that are stat sig:
#Sub Tropical
#Tropical
#Warm Temperate

#Biomes that are not stat sig:
#Cool Temperate 

PB_ai_b = ggplot(DF, aes(x = ln(pop2015),y = ln(aiG), color = TempGroup)) +
  geom_point() +
  scale_color_brewer(name = "Temperature Regimes",palette = "Paired") +
  xlab(expression(paste("ln(population)"))) +
  ylab(expression(paste("ln(green space connectedness)"))) +
  theme_minimal() +
  ggtitle("F. Connectedness")+
  scale_y_continuous(limits=c(min(ln(DF$aiG)),max(ln(DF$aiG))))+
  #Cool Temperate (not sig)
  geom_abline(slope=as.numeric(ai2_b$coef$`Cool Temperate`[2,1]),intercept=as.numeric(ai2_b$coef$`Cool Temperate`[1,1]),size = 1,linetype = 2,color = "#a6cee3") +
  #Warm Temperate (sig)
  geom_abline(slope=as.numeric(ai2_b$coef$`Warm Temperate`[2,1]),intercept=as.numeric(ai2_b$coef$`Warm Temperate`[1,1]),size = 1,color = "#1f78b4") +
  #Sub Tropical (sig)
  geom_abline(slope=as.numeric(ai2_b$coef$`Sub Tropical`[2,1]),intercept=as.numeric(ai2_b$coef$`Sub Tropical`[1,1]), size = 1,color = "#b2df8a") +
  #Tropical (sig)
  geom_abline(slope=as.numeric(ai2_b$coef$Tropical[2,1]),intercept=as.numeric(ai2_b$coef$Tropical[1,1]),size = 1,color = "#33a02c") +
  theme(legend.position = "none")

grid.arrange(PB_pland_b,PB_area_b,PB_ai_b,ncol=3)

grid.arrange(pop_pland,pop_area,pop_ai,PB_pland_b,PB_area_b,PB_ai_b,ncol = 3)

## STEP 4 ---------------------------------------------------------------
# Local green space scaling with city size (Figure 3)

#----1. Fit curves for each city and metric----
#Set up city list
cityList = DF_3$City
metricList = c('pland','ai','area_mn')

#create empty DF
curveDF = data.frame(matrix(ncol = 6, nrow = 0))
colnames(curveDF) = c("City","Metric","BoundCir_R2","BoundCir_pVal","BoundCir_exp","BoundCir_int")

for (city in cityList) {
  print(city)
  for (metric in metricList) {
    #set up the data frame where the values will be stored in the loop
    DF_loop = data.frame(matrix(ncol = 6, nrow = 1))
    colnames(DF_loop) = c("City","Metric","BoundCir_R2","BoundCir_pVal","BoundCir_exp","BoundCir_int")
    DF_loop$City = city
    DF_loop$Metric = metric
    
    #subset the metricDF to the city and metric
    curveDF_loop = metricDF[(metricDF$city == city) & (metricDF$metric == metric) & metricDF$LC == "green", ]
    
    #fit the curve
    model = lm(log(curveDF_loop$value) ~ log(curveDF_loop$circleAreaKm2)) #Bounding Circle Area
    
    #put them to the data frame
    DF_loop$BoundCir_R2 = summary(model)$r.squared
    DF_loop$BoundCir_pVal = as.numeric(summary(model)$coefficients[2,4])
    DF_loop$BoundCir_exp = as.numeric(model$coefficients[2])
    DF_loop$BoundCir_int = as.numeric(model$coefficients[1])
    
    #append the loop data frame to the data frame outside the loop
    curveDF = rbind(curveDF,DF_loop)
  }
}

#Add in the income class for export:
DFsub = DF[,c("City","IncGroup","TempGroup","MoistGroup","Region","pop2015","circleAreaKm2")]

exportTab = merge(curveDF,DFsub,by = "City",all.x = TRUE)
#exportTab$LC = "Green"

#----2. Isolate the cities/metrics with stat sig p-values and high R2----

exportTab_sig90 =  exportTab[(exportTab$BoundCir_pVal <= 0.05) & (exportTab$BoundCir_R2 >= 0.90),]
exportTab_sig70 =  exportTab[(exportTab$BoundCir_pVal <= 0.05) & (exportTab$BoundCir_R2 >= 0.70),]

exportTab_sig90PLAND = exportTab_sig90[(exportTab_sig90$Metric == "pland"),] #70/124
exportTab_sig90AREA = exportTab_sig90[(exportTab_sig90$Metric == "area_mn"),]
exportTab_sig90AI = exportTab_sig90[(exportTab_sig90$Metric == "ai"),]

#Summary for values reported in text and table:
exportTab_sig90AI %>%
  group_by(TempGroup) %>%
  summarise(n = n())

exportTab_sig90AREA %>%
  summarise(mean = mean(BoundCir_exp), sd = sd(BoundCir_exp))

#----3. Scatterplots with exponents on the y-axis and bounding circle area on the x-axis----

##PLAND
plandE = ggplot(exportTab_sig90PLAND, aes(x=ln(circleAreaKm2),y=BoundCir_exp, color = IncGroup))+
  geom_point()+
  theme_minimal()+
  scale_color_brewer(name = "Income Group",palette = "PuOr")+
  ggtitle("A. Area") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2},")"))) +
  geom_hline(yintercept = 1,linetype = 2, color = "#333333") +
  scale_y_continuous(limits = c(0, (max(exportTab_sig90PLAND$BoundCir_exp))))+
  ylab("b")+
  theme(legend.position = "none")

##AREA_MN
areamnE = ggplot(exportTab_sig90AREA, aes(x=ln(circleAreaKm2),y=BoundCir_exp, color = IncGroup))+
  geom_point()+
  theme_minimal()+
  scale_color_brewer(name = "Country Income Group",palette = "PuOr")+
  ggtitle("B. Average Patch Area") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2},")"))) +
  ylab("b")+
  geom_hline(yintercept = 1,linetype = 2, color = "#333333") +
  scale_y_continuous(limits = c(0, (max(exportTab_sig90AREA$BoundCir_exp))))+
  theme(legend.position = "none")

##AI
aiE = ggplot(exportTab_sig90AI, aes(x=ln(circleAreaKm2),y=BoundCir_exp, color = IncGroup))+
  geom_point()+
  theme_minimal()+
  scale_color_brewer(name = "Country Income Group",palette = "PuOr")+
  ggtitle("C. Connectedness") +
  xlab(expression(paste("ln(area of bounding circle: ", km^{2},")"))) +
  ylab("b")+
  theme(legend.position = "none")

grid.arrange(plandE,areamnE,aiE,ncol = 1)

#----4. Individual city scalograms----

#----Chicago, PLAND----#
DF_Chi_pland = subset(metricDF, city == "Chicago" & metric == "pland" & LC == "green")
options(scipen = 999)

#Non-linear plot
plandChi = ggplot(DF_Chi_pland, aes(x = circleAreaKm2,y = value, color = LC)) +
  geom_point(color = "#52854C") + 
  #scale_color_manual(values = c('#525252','#525252')) +
  #scale_color_manual(values = c("#999999", "#52854C")) +
  xlab(expression(paste("Total area of concentric circle (", km^{2}, ")"))) +
  ylab("Area (ha)") +
  ggtitle("D. Chicago, USA") + 
  theme_minimal() +
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = 'none')

#----Lahore, PLAND----#
DF_Lah_pland = subset(metricDF, city == "Lahore" & metric == "pland" & LC == "green")

#Non-linear plot
plandLah = ggplot(DF_Lah_pland, aes(x = circleAreaKm2,y = value, color = LC)) +
  geom_point(color = "#52854C") + 
  #scale_color_manual(values = c('#525252','#525252')) +
  #scale_color_manual(values = c("#999999", "#52854C")) +
  xlab(expression(paste("Total area of concentric circle (", km^{2}, ")"))) +
  ylab("Area (ha)") +
  ggtitle("E. Lahore, Pakistan") + 
  theme_minimal() +
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = 'none')

#----Curitiba, AREA_MN----#
DF_Cur_area = subset(metricDF, city == "Curitiba" & metric == "area_mn" & LC == "green")

#Non-linear plot
areaCur = ggplot(DF_Cur_area, aes(x = circleAreaKm2,y = value, color = LC)) +
  geom_point(color = "#52854C") + 
  #scale_color_manual(values = c('#525252','#525252')) +
  #scale_color_manual(values = c("#999999", "#52854C")) +
  xlab(expression(paste("Total area of concentric circle (", km^{2}, ")"))) +
  ylab("Average Patch Area (ha)") +
  ggtitle("F. Curitiba, Brazil") + 
  theme_minimal() +
  scale_y_continuous(limits = c(0,46))+
  theme(legend.position = 'none')

#----Seoul, AREA_MN----#
DF_Seo_area = subset(metricDF, city == "Seoul" & metric == "area_mn" & LC == "green")

#Non-linear plot
areaSeo = ggplot(DF_Seo_area, aes(x = circleAreaKm2,y = value, color = LC)) +
  geom_point(color = "#52854C") +  
  #scale_color_manual(values = c('#525252','#525252')) +
  #scale_color_manual(values = c("#999999", "#52854C")) +
  xlab(expression(paste("Total area of concentric circle (", km^{2}, ")"))) +
  ylab("Average Patch Area (ha)") +
  ggtitle("G. Seoul, South Korea") + 
  theme_minimal() +
  scale_y_continuous(limits = c(0,46))+
  theme(legend.position = 'none')

#----Lubumbashi, AI----#
DF_Lub_ai = subset(metricDF, city == "Lubumbashi" & metric == "ai" & LC == "green")

#Non-linear plot
aiLub = ggplot(DF_Lub_ai, aes(x = circleAreaKm2,y = value, color = LC)) +
  geom_point(color = "#52854C") +  
  #scale_color_manual(values = c('#525252','#525252')) +
  #scale_color_manual(values = c("#999999", "#52854C")) +
  xlab(expression(paste("Total area of concentric circle (", km^{2}, ")"))) +
  ylab("Connectedness (%)") +
  ggtitle("H. Lubumbashi, Democratic Republic of Congo") + 
  theme_minimal() +
  scale_y_continuous(limits = c(65,100))+
  theme(legend.position = 'none')

#----Singapore, AI----#
DF_Sin_ai = subset(metricDF, city == "Singapore" & metric == "ai" & LC == "green")

#Non-linear plot
aiSin = ggplot(DF_Sin_ai, aes(x = circleAreaKm2,y = value, color = LC)) +
  geom_point(color = "#52854C") +  
  #scale_color_manual(values = c('#525252','#525252')) +
  #scale_color_manual(values = c("#999999", "#52854C")) +
  xlab(expression(paste("Total area of concentric circle (", km^{2}, ")"))) +
  ylab("Connectedness (%)") +
  ggtitle("I. Singapore, Republic of Singapore") + 
  theme_minimal() +
  scale_y_continuous(limits = c(65,100))+
  theme(legend.position = 'none')

grid.arrange(plandChi,plandLah,areaCur,areaSeo,aiLub,aiSin,ncol=1)