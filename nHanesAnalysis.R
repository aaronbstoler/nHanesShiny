library(tidyverse)
library(ggplot2)
library(survey)

#### Read in data ####
nhanes_1718 <- read.csv("nHanes/nhanes_1718_DI.csv")

#### Establish Survey Design ####
nhanes_design <- svydesign (data=nhanes_1718, 
                            id=~SDMVPSU, 
                            strata=~SDMVSTRA, 
                            weights=~WTSB2YR, 
                            nest=TRUE) 

####placeholder####

quantileCount <- 3
gender <- "F"
pthalates <- c("DBP_DI_ug_g_d","DiBP_DI_ug_g_d","BBP_DI_ug_g_d","DEHP_DI_ug_g_d","DINP_chap_DI_ug_g_d","DINP_DI_ug_g_d","DIDP_DI_ug_g_d","DEHTP_DI_ug_g_d")
quantileVals <- c("X25th","X50th","X95th")



surveydesign <- function(lowerage,upperage,gender,ethnicity) {
  
  sub_design <- subset(nhanes_design, Age_yr>=lowerage & Age_yr<=upperage & Gender %in% gender & Race %in% ethnicity)
  quantiledf <- data.frame(matrix(data=NA,ncol=(quantileCount+1),nrow=chemicalCount))
  colnames(quantiledf)[1] <- "Chemical"

  for (i in 2:(quantileCount+1)) {
    colnames(quantiledf)[i] <- quantileVals[i-1]
  }

  for (i in 1:length(pthalates)) {
    quantiledata <- svyquantile(eval(parse(text=paste0("~",pthalates[i]))), sub_design, c(.25,.5,.95), ci=FALSE, na.rm=TRUE)
    quantiledf[i,1] <- pthalates[i]
    for (j in 2:(quantileCount+1)) {
      quantiledf[i,j] <- eval(parse(text=paste0("quantiledata$",pthalates[i],"[j-1]")))
    }
  }
  return(quantiledf)
}

### visualization

BMDL5_NAS2017=c(8,22.5,13,11,49,49,99,99)


# add 
createPlot <- function(quantiledf) {
quantilePlot <- ggplot(quantiledf) + coord_trans(y="log2")
quantilePlot <- quantilePlot +
  geom_segment( aes(x=Chemical, xend=Chemical, y=X25th, yend=X95th), color="grey", linewidth=1)

quantilePlot <- quantilePlot +
  geom_point( aes(x=Chemical, y=X25th), color="blue", size=3 )

quantilePlot <- quantilePlot +
  geom_point( aes(x=Chemical, y=X95th), color="red", size=3 )

quantilePlot <- quantilePlot +
  geom_point(aes(x=Chemical, y=BMDL5_NAS2017), shape=8, size=2)
  
quantilePlot <- quantilePlot +
  labs(title = "Daily Intakes among US Women (15-46 years) based on NHANES 2017-18", subtitle = "Stars: BMDL5 POD values based on NAS2017 (to be added for DIDP and DEHTP) 
Blue dots: 25th percentiles, Red dots: 95th percentiles")+
  ylab("Daily Intakes in ug/g/day")
print("hello")
return(quantilePlot)
}
