rm(list=ls())
library(ggplot2)
#library(ggsignif)
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/InflammationPaperFigures/FetalBrainIL6_ISvID")

### Reads a file in a table format and creates a data frame from it. This figures will be in the first panel.
datafile <- "FetBrain_IL6_CtCalcs_ISvID_woutOutliers.csv"
data1 <- read.csv(datafile, header=TRUE) 
dataframe_Original <- data.frame(data1)

# Write in the calibrator sample mean from the average of your Ct calculations for your calibrator sample
# For this, just go to "IL1b_Regraphing RT-qPCR Data for Publication.csv" file, on the Cq.Average column, use "=average" function to determine
# the group mean and input it below
IS_MD_mean <-  12.198

# This function is the calculation for the new Ct's that have been compared to IS-0
Function1 <- function(Ct) {
  FUN <- ((2^-(Ct - IS_MD_mean)))
  return(FUN)
}
# For each sample, this subtracts the calibrator sample mean, and calculates the 2^-ddCt
Adj_Ct <- sapply(dataframe_Original$Cq.Averages, match.fun("Function1"), USE.NAMES = TRUE)

# Convert values from above into individual dataframes
dataframe_AdjCtCalcs <- data.frame(Adj_Ct)

# Creates one final dataframe
dataframe_AdjCtCalcs$Biological.Sets<-dataframe_Original$Biological.Sets #adds the Biological.Sets labels back to the dataframe
dataframe_AdjCtCalcs$Sample.IDs<-dataframe_Original$Sample.IDs #adds the sample IDs back to the dataframe
dataframe_AdjCtCalcs$lower<-dataframe_Original$lower
dataframe_AdjCtCalcs$upper<-dataframe_Original$upper
dataframe_AdjCtCalcs$mean<-dataframe_Original$mean
dataframe_AdjCtCalcs <- dataframe_AdjCtCalcs[,c(2,3,1,4,5,6)] #rearranges the columns so Biological.Sets is first

## Create a dot plot overlaid on a bar chart
# Initialize an individual high resolution graph
png("rFetBrain_IL6_ISvsID_PubPlot.png", units="mm", width=100, height=120, res=600)

# Sets order of samples for future steps
order <- c("IS-M", "IS-PAE", "ID-M", "ID-PAE")

plot <- ggplot(data= dataframe_AdjCtCalcs, aes(x=Biological.Sets, y=mean), fill=order) + #creates original graph
  geom_bar(stat="identity", color="black", fill = "#FFFFFF", #this "color" sets the outline color and the interior color
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(2)) + 
  ggtitle("Il-6 Expression") +
  #geom_signif(comparisons = list(c("ID-MD", "IF-MD")), annotations="*", y_position = 3.4, textsize = 6, tip_length = 0, vjust=0.4) + #this allows you to use symbols to designate statistical significance
  #geom_signif(comparisons = list(c("IF-MD", "IF-PAE")), annotations="*", y_position = 3.9, textsize = 6, tip_length = 0, vjust=0.4) +
  scale_y_continuous(limits = c(0,8.0), expand = c(0,0)) + #this sets the minimum and maximum values shown on the y-axis
  ylab(expression("Fetal Il-6/Gapdh Relative to IS-M")) + #adds y-axis label
  scale_x_discrete (limits = order) + #rearranges the x-axis in correct order
  geom_point(aes(x=Biological.Sets, y=Adj_Ct),
             size = 1.25, shape = 19, position = position_jitter(width=.15, height=0.1)) + #adds individual points
  theme(
    plot.title = element_text(hjust = .5, size = 18), #sets size and position of graph title
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    panel.grid.major = element_blank(), #gets rid of major gridlines
    panel.grid.minor = element_blank(), #gets rid of minor gridlines
    panel.background = element_blank(), #turns background white instead of gray
    axis.line = element_line(colour = "black"), #turns axes to black
    legend.position="none", #gets rid of legend
    axis.text=element_text(size=16, color = "black")) #sets size of x and y axis labels

# Print and release PNG file graph into directory
print(plot)
dev.off() 

#The annotations below add in the text for showing model P values/statistics summary
#hjust stands for horizontal justification, with 0 being left-justified, 0.5 being centered, and 1 being right-justified
#parse = TRUE so that it will turn the text in the label "" into an equation
#p1D <- p1d + annotate ("text", x = 1, y = 4.2, label = "Iron: italic(P) == 0.0084", parse = TRUE, hjust = 0, family = "serif")  
#p1DD <- p1D + annotate ("text", x = 1, y = 4.0, label = "EtOH: italic(P) == 0.0337", parse = TRUE, hjust = 0, family = "serif")  
#p1DDD <- p1DD + annotate ("text", x = 1, y = 3.8, label = "Iron X EtOH:", parse = FALSE, hjust = 0, family = "serif") +
#                annotate ("text", x = 2.3, y=3.8, label = "italic(P) == 0.0005", parse = TRUE, hjust = 0, family = "serif")

