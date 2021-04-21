# Code used by Chuyin

# live dangerously, get rid of pesky warnings
oldw <- getOption("warn")
options(warn = -1)
Sys.setenv(LANGUAGE = "en")
shhh <- suppressPackageStartupMessages # stops annoying warnings when loading libraries

library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS) #conflict with dplyr on select()
library(Matrix)
library(reshape2)
library(ape) # stats
library(vegan) # stats
library(RColorBrewer)
library(cocor)
library(DescTools)
library(reshape2)
library(grid)
library(ggplotify)

source('./functions.R')

# read the csv data files into a dataframe
files = list.files(pattern="*.csv")
data = sapply(files, read.csv, simplify=FALSE) %>% bind_rows(.id = "id")

colnames(data)

# Select variables we need for analysis 
trial_vars<- c( "participant","realFrameRate",
                "Colour_1", "Colour_2", "Colour1", "Colour2", 
                "similarity", "response_time", "catchnumber", "maskCon1", "mask_frames", "catchnumber", "catchresponse", "Catch", "screen_size_x","screen_size_y","viewerdistancecm", 'viewer_distance',"trialnumber")

data <- subset(data, select = trial_vars)

# Create data frame for trials 
dftrials <- subset(data, !is.na(Colour1))

# Label participant number from 1 - 15 
dftrials$ID <- NA
subjectlist <- unique(dftrials$participant)
k= 0
for (participant in subjectlist){
  k = k + 1
  dftrials$ID[dftrials$participant == participant] <- k
}


# Check average Response Time
rt_cutoff = 700 # mean reaction times must be above this

rt_avg_check(dftrials)

#check catch trials
subjectlist <- sort(unique(dftrials$participant))
print("catch trials")
for (participant in subjectlist){
  subjectdf <- dftrials[which(dftrials$participant == participant),] 
  ID = unique(subjectdf$ID)
  catch = subjectdf %>% filter(Catch == 1) %>% filter(!is.na(catchresponse))
  k = 0
  for (i in 1:length(catch$participant))
  {
    if(catch[i,'catchnumber.1'] == catch[i,'catchresponse'])
    {
      k = k + 1
    }
  }
  print(paste("Subject:",ID,"catch",k,'in 20'))
}

#correlation between 1st and 2nd pass
subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
correlation_list <- data.frame() # array to store the values in

for (ID in subjectlist){ # go through subject by subject
  subjectdf <-  dftrials[which(dftrials$ID == ID),]  # select the ID for subject of interest
  for (mask in unique(dftrials$maskCon1))
  {
    temp1 = subjectdf %>% filter(trialnumber <= 162) %>% filter(maskCon1 == mask)
    temp2 = subjectdf %>% filter(trialnumber >= 163) %>% filter(maskCon1 == mask)
    correlation_list <- rbind(correlation_list, c(ID, cor(temp1$similarity, temp2$similarity)))
  }
  correlation_list[2 * ID - 1, 3] = 'strong'
  correlation_list[2 * ID, 3] = 'weak'
  colnames(correlation_list) <- c('ID', 'correlation','mask')
}
plot <- ggplot(correlation_list, aes(x = ID, y = correlation, fill = mask, color = mask)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = 'correlation between 1st and 2nd pass - Pearson r', x = 'ID', y = 'correlation')
plot
correlation_list



# changing color values from RGB to hex for graphing purpose
dftrials$Colour1 <- as.character(dftrials$Colour1)
dftrials$Colour1 <- revalue(dftrials$Colour1, 
                            c(  "1" = '#FF0000',
                                "2" = '#FFAA00',
                                "3" = '#AAFF00',
                                "4" = '#00FF00',
                                "5" = '#00FFA9',
                                "6" = '#00A9FF',
                                "7" = '#0000FF',
                                "8" = '#AA00FF',
                                "9" = '#FF00AA'))
dftrials$Colour2 <- as.character(dftrials$Colour2)
dftrials$Colour2 <- revalue(dftrials$Colour2, 
                            c(  "1" = '#FF0000',
                                "2" = '#FFAA00',
                                "3" = '#AAFF00',
                                "4" = '#00FF00',
                                "5" = '#00FFA9',
                                "6" = '#00A9FF',
                                "7" = '#0000FF',
                                "8" = '#AA00FF',
                                "9" = '#FF00AA'))

# colors for the labels
# red, orange, yellow, green, cyan, cyan-blue, blue, purple, pink
colors <- c('#FF0000','#FFAA00','#AAFF00','#00FF00','#00FFA9','#00A9FF','#0000FF','#AA00FF','#FF00AA')
# can change the way the plot line up
# red, pink, orange, purple, yellow, blue, green, cyan-blue, cyan
#colors <- c('#FF0000','#FF00AA','#FFAA00','#AA00FF','#AAFF00','#0000FF','#00FF00','#00A9FF','#00FFA9')
abcolors <- sort(colors) # this was messing up the asymmetry plot, maybe useful for some other stuff

# set the maximum and minimum dissimilarity values for later analysis
min_val = 0
max_val = 6


trace_cutoff = 2 # mean dissimilarity for physically identical colours must be below this
antitrace_cutoff = 3.5 # mean dissimilarity accepted for maximally physically different colours must be above this
rt_cutoff = 700 # mean reaction times must be above this

exclude_noncompliant = FALSE

plotsubjects = FALSE
plot_within_between = FALSE
plotexpsummary = FALSE
across = FALSE
population = FALSE

# rainbowcloud theme for plotting, stolen from: 
# https://micahallen.org/2018/03/15/introducing-raincloud-plots/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com
raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# stealing ability to make flat violin plots
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

dftrials_strong = dftrials %>% filter(maskCon1 == 'strong')
dftrials_weak = dftrials %>% filter(maskCon1 == 'weak')

# Dissimplot for all data
dissimplot_temporal(dftrials_strong,colors)
dissimplot_temporal(dftrials_weak,colors)

#diagonal and t tests
diag_strong = dftrials_strong %>% filter(Colour1 == Colour2)
diag_weak = dftrials_weak %>% filter(Colour1 == Colour2)

t.test(x = diag_strong$similarity, mu = 0)
t.test(x = diag_weak$similarity, mu = 0)

t.test(x = diag_strong$similarity, y = diag_weak$similarity)

# Plot a dissimiliarity matrix for all subjects 
dissimplot_temporal_subject(dftrials_strong, colors)
dissimplot_temporal_subject(dftrials_weak, colors)

#plot individual dissimilarity
IDs <- unique(dftrials$ID)
maskcon = c('strong','weak')

for (ID in IDs){
  plot_list <- list()
  #Subset data for the subject
  subjectdf = dftrials[which(dftrials$ID == ID),]
  
  for (mask in maskcon)
  {
    if(mask == 'strong'){k=1}
    if(mask == 'weak'){k=2}
    mask_conditioned = subjectdf %>% filter(maskCon1 == mask)
    # refine data using function "dissimdata2 "
    datatemp <- dissimdata2(mask_conditioned, colors)
    datatemp <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean)
    plot <- ggplot(datatemp, aes(x = Group.1, y = Group.2)) +
      theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))+
      ggtitle(paste('pariticpant:',ID,"mask condition:", mask))
    # stuff that's standard across plot types
    plot <- plot + geom_raster(aes(fill = similarity)) +
      scale_fill_gradientn(colours = c("white","black"), limits = c(0, 7), n.breaks = 8) +
      guides(fill=guide_legend(title="Dissimilarity"))
    plot_list[[k]] <- plot
    
  }
  print(ID)
  plot_grob <- arrangeGrob(grobs=plot_list)
  grid.arrange(plot_grob)
  scan()
}

#MDS
library(smacof)
library(svglite)
library(dplyr)

for (k in IDs)
{
  strong = dftrials_strong %>% filter(ID == k)
  weak = dftrials_weak %>% filter(ID == k)
  MDS_plot_individual(strong,weak,colors,k)
  scan()
}

#distributions
temp = dftrials %>% select(Colour1,Colour2,similarity,maskCon1)
temp1 = temp %>% filter(maskCon1 == 'strong')
temp2 = temp %>% filter(maskCon1 == 'weak') %>% mutate(similarity = -1 * similarity)
temp = rbind(temp1,temp2)
svglite(file = 'distributions.svg',width = 18,9)
fig = ggplot(temp,aes(x = similarity,fill = maskCon1, color = maskCon1))
fig = fig + geom_histogram(binwidth = 1)
fig = fig + facet_wrap(Colour1 ~ Colour2,ncol = 9,nrow = 9)
print(fig)
dev.off()


#refresh rates
rate_vars<- c( "participant","realFrameRate1", "realFrameRate2","realFrameRate3","realFrameRate4","realFrameRate5","realFrameRate")
rate_data <- subset(data, select = rate_vars)

# Create data frame for trials 
rate_data <- subset(rate_data, !is.na(realFrameRate))

# Label participant number from 1 - 15 
rate_data$ID <- NA
subjectlist <- unique(rate_data$participant)
k= 0
for (participant in subjectlist){
  k = k + 1
  rate_data$ID[rate_data$participant == participant] <- k
}

rate = rate_data %>% group_by(ID) %>% summarise(Rate1 = mean(realFrameRate1),Rate2 = mean(realFrameRate2),
                                         Rate3 = mean(realFrameRate3),Rate4 = mean(realFrameRate4),
                                         Rate5 = mean(realFrameRate5),avg_Rate = mean(realFrameRate),) %>% ungroup()



# June 29 : Zhao on a new measure of intersubjective agreement 
# recording 
# https://drive.google.com/file/d/1quETgBsnJgZXIgT_HWG8Phzx6PIel92L/view?usp=sharing
# 
# slides
# https://docs.google.com/presentation/d/1Q_fE6F0TXzsnv72cd9I5GU__bpNxANZlKJ5nCEQcors/edit?usp=sharing

library(psych)
correlation_list = correlation_list %>% mutate(z = fisherz(correlation_list$correlation))
strong = correlation_list %>% filter(mask == 'strong')
weak = correlation_list %>% filter(mask == 'weak')
describe(strong$z)
describe(weak$z)

diag_strong %>% group_by(Colour1) %>% summarise(m = mean(similarity))
diag_weak %>% group_by(Colour1) %>% summarise(m = mean(similarity))

strong = diag_strong %>% group_by(ID) %>% summarise(m = mean(similarity))
weak = diag_weak %>% group_by(ID) %>% summarise(m = mean(similarity))
describe(strong$m)
describe(weak$m)