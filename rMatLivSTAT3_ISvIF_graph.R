rm(list=ls())
library(ggplot2)
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/InflammationPaperFigures/MaternalLiverSTAT_ISvIF")
PCR_file <- "MatLiv_STAT3_ISvIF.csv"

#reads a file in a table format and creates a data frame from it
data <- read.csv(PCR_file, header=TRUE) 
data_frame <- data.frame(data)

#initialize a high resolution graph
png("rMatLiv_STAT_ISvIF_PubPlot.png", units="mm", width=100, height=120, res=600) 

sets <- c("IS-M", "IS-PAE", "IF-M", "IF-PAE")
p<- ggplot(data_frame, aes(x=Treatments, y=mean, fill=sets)) + 
  geom_bar(stat="identity", color="black", #this "color" sets the outline color
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) + 
  scale_y_continuous(limits = c(0,300), expand = c(0,0)) + #to make the graph sit on the bottom without the space
  theme(plot.title = element_text(hjust = 0.5, size=16), 
        axis.title.x = element_blank(), #gets rid of x-axis label
        panel.grid.major = element_blank(), #gets rid of major gridlines
        panel.grid.minor = element_blank(), #gets rid of minor gridlines
        panel.background = element_blank(), #turns background white instead of gray
        axis.line = element_line(colour = "black"), 
        legend.position="none", #gets rid of legend
        axis.text=element_text(size=15, color = "black"), #sets size of x and y axis labels
        axis.title=element_text(size=17)) + scale_x_discrete (limits = sets) +
  labs(title=("Maternal STAT3 Activation"), y=("% Phosphorylation Relative to IS-M")) +
  scale_fill_manual(values = c("#333333", "#CCCCCC", "#000000", "#666666"))

#release PNG file graph into directory
print(p)
dev.off() 
