rm(list=ls())
library("ggpubr")
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/InflammationPaperFigures/FetalLiverSTAT_ISvIF")
PCR_file <- "Fetal_STAT3_Westerns_ISvIF_ForStats.csv"

data <- read.csv(PCR_file, header=TRUE)
data_frame <- data.frame(data)

IS_0 <- data_frame[is.element(data_frame$Biological.Sets, "IS-M"),]
IS_5 <- data_frame[is.element(data_frame$Biological.Sets, "IS-PAE"),]
IF_0 <- data_frame[is.element(data_frame$Biological.Sets, "IF-M"),]
IF_5 <- data_frame[is.element(data_frame$Biological.Sets, "IF-PAE"),]

sets <- data.frame(IS_0$STAT)
sets$IS_5 <- IS_5$STAT
sets$IF_0 <- IF_0$STAT
sets$IF_5 <- IF_5$STAT
colnames(sets)[1] <- "IS_0" #renaming the first column

treatments <- c(sets$IS_0, sets$IS_5, sets$IF_0, sets$IF_5)
tech_repl <- 7 #number of biological replicates
set_nums <- 1 #number of technical plate replicates
reps <- tech_repl*set_nums #reps should match the number of rows
groups <- c(rep("IS_0", reps), rep("IS_5", reps), rep("IF_0", reps), rep("IF_5", reps))

join <- data.frame(treatments, groups)
#Checking for the assumption of equal variance
bartlett.test(treatments, groups)
#checking for assumption of normality
shapiro.test(data_frame$STAT)
hist(data_frame$STAT)
qqnorm(data_frame$STAT)
qqline(data_frame$STAT)

####2-way ANOVA####
#Use if data is normal and does have equal variances
#new_data_frame <- data_frame[order(data_frame$Biological.Sets),]
iron_group <- c(rep("S", reps), rep("S", reps), rep("F", reps), rep("F", reps))
alcohol_group <- c(rep("0", reps), rep("5", reps), rep("0", reps), rep("5", reps))#remove both iron and alcohol groups if say ID is not there
join_aov <- data.frame(treatments, iron_group, alcohol_group)
model <- aov(treatments ~ iron_group + alcohol_group + iron_group:alcohol_group, data = join_aov)
summary(model)
#running tukey test for pairwise comparisons. Only use if the ANOVA is significant
TukeyHSD(model)

#If data is not normal and doesn't have equal variances run below
join$groups <- ordered(join$groups, levels = c("IS_0", "IS_5", "ID_0", "ID_5"))
kruskal.test(join$treatments ~ join$groups, data = join) #tells you if there is a difference between any of the groups
pairwise.wilcox.test(join$treatments, join$groups, p.adjust.method = "BH") #tells you which groups are significantly different from each other

#Plotting a dotplot colored by litter within a treatment group
merge <- merge(x=join, y=data_frame, by.x = "treatments", by.y = "Cq.Averages")
merge$Litter.Number <- as.character(merge$Litter.Number)
ggline(merge, x = "groups", y = "treatments", 
       add = c("none"), 
       color = "Litter.Number", palette = c("black", "red", "blue", "green"),
       order = c("IS_0", "IS_5", "IF_0", "IF_5"),
       ylab = "Cq Averages", xlab = "Treatments",
       size = 1,
       plot_type = c("p"))

#Plotting a means plot- just one experiment
ggline(merge, x = "groups", y = "treatments", 
       add = c("mean_se", "jitter"), 
       order = c("IS_0", "IS_5", "IF_0", "IF_5"),
       ylab = "Cq Averages", xlab = "Treatments", 
       plot_type = c("p"))

#Plotting a boxplot representation of the data - includes all data points from file (both experiments)
ggboxplot(join, x = "groups", y = "treatments", 
          color = "groups", palette = c("black", "red", "blue", "green"),
          order = c("IS_0", "IS_5", "IF_0", "IF_5"),
          ylab = "Cq Averages", xlab = "Treatments")
