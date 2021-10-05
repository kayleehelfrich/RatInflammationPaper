#Change Cq File Biological Set Names to IS-MD, IS-PAE, etc. before beginning
#Average together any Cq values from same sample (leaving only biological replicates, no technical replicates)
#Copy and paste correct upper and lower bounds, and means to numbers (ex. all IS-0 get same upper, lower, and mean)
#Delete standard error for each sample
rm(list=ls())
library(ggplot2)
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/InflammationPaperFigures/FetalLiverIL6_ISvIF")

#reads a file in a table format and creates a data frame from it
datafile <- "FetLiv_IL6_CtCalcs1_Combined_For_ddCtDotPlot.csv"
data1 <- read.csv(datafile, header=TRUE) 
dataframe_Original <- data.frame(data1)

#write in the calibrator sample mean from the average of your Ct calculations for your calibrator sample
IS_MD_mean <-  4.982341046

#this function is the calculation for the new Ct's that have been compared to IS-0
Function1 <- function(Ct) {
  FUN <- ((2^-(Ct - IS_MD_mean)))
  return(FUN)
}
#for each sample, this subtracts the calibrator sample mean, and calculates the 2^-ddCt
Adj_Ct <- sapply(dataframe_Original$Cq.Averages, match.fun("Function1"), USE.NAMES = TRUE)

#convert values from above into individual dataframes
dataframe_AdjCtCalcs <- data.frame(Adj_Ct)

#creates one final dataframe
dataframe_AdjCtCalcs$Biological.Sets<-dataframe_Original$Biological.Sets #adds the Biological.Sets labels back to the dataframe
dataframe_AdjCtCalcs$Sample.IDs<-dataframe_Original$Sample.IDs #adds the sample IDs back to the dataframe
dataframe_AdjCtCalcs$lower<-dataframe_Original$lower
dataframe_AdjCtCalcs$upper<-dataframe_Original$upper
dataframe_AdjCtCalcs$mean<-dataframe_Original$mean
dataframe_AdjCtCalcs <- dataframe_AdjCtCalcs[,c(2,3,1,4,5,6)] #rearranges the columns so Biological.Sets is first

#initialize a high resolution graph
png("rFetLiv_IL6_ISvsIF_ddCtDotPlot_TEST.png", units="mm", width=120, height=120, res=600)

#sets order of samples for future steps
order <- c("IS-MD", "IS-PAE", "IF-MD", "IF-PAE")
#cols <- c("IS-MD" = "deepskyblue4", "IS-PAE" = "deepskyblue3", "IF-MD" = "springgreen4", "IF-PAE" = "springgreen3" )

p <- ggplot(data= dataframe_AdjCtCalcs, aes(x=Biological.Sets, y=Adj_Ct), fill=order) + #creates original graph
  geom_point(size = 1.25, shape = 19, position = position_jitter(width=.15, height=0.1)) + #adds individual points
  ggtitle("IL-6 Expression") + #adds title
  #xlab("Treatment") + #adds x-axis label
  ylab("Fetal Liver IL6/Gapdh Relative to IS-MD") + #adds y-axis label
  geom_errorbar(aes(ymin = lower, ymax = upper), col = "black", width =  .15) + #adjusts errorbars
  geom_point(aes(x = Biological.Sets, y = mean), size = 4, shape = 95) + #adjusts mean line
  scale_y_continuous(limits = c(-0.1,12)) +
  scale_x_discrete (limits = order) + #rearranges the x-axis in correct order
  theme(
    plot.title = element_text(hjust = .5, size = 16), #sets size and position of graph title
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_blank(), #gets rid of major gridlines
    panel.grid.minor = element_blank(), #gets rid of minor gridlines
    panel.background = element_blank(), #turns background white instead of gray
    axis.line = element_line(colour = "black"), #turns axes to black
    legend.position="none", #gets rid of legend
    axis.text=element_text(size=12, color = "black")) #sets size of x and y axis labels
print(p)

#release PNG file graph into directory
dev.off() 
