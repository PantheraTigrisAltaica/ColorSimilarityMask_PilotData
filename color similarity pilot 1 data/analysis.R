# Code from Ariel and Yusuke 

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
library(MASS)
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


# Select relevant trials for catch trials 

# Check catch scores 

# Screen parameters 

# Screen size function 

# View distance function 

# Calculate screen parameters for each participant 

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


# Similarity judgment histogram

# reaction time for each similarity


# correlation between reaction times and similarity judgements
# grouping at individual trial, individual participant, experiment or entire population level

rt_similarity_cor(dftrials,level='participant')

rt_similarity_plot(dftrials,xlabel='BLANK')

# reaction time raincloud plot

rsplot_raincloud(dftrials,xtype='linear')


# subject info

# get median reaction time

# function to aggregate everyone's data together


# factor the dataframes for the plot function

# return a list of the asymmetrical values for each subject

# Dissimplot for all data

# Plot within subject response variance 

# Calculate variance in similarity within subject
datavar <-  group_by(dftrials, Colour1, Colour2, participant) %>% 
  summarise(
    count = n(), 
    varsim = var(similarity, na.rm = TRUE),
  )

# Calculate the mean variance across subjects
datavar  <-  group_by(datavar, Colour1, Colour2) %>% 
  summarise(
    count = n(), 
    varmean = mean(varsim, na.rm = TRUE),
  )

# Plot the mean variance accross subjects
plot <- ggplot(datavar, aes(x = Colour1, y = Colour2)) +
  theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# stuff that's standard across plot types
plot <- plot + geom_raster(aes(fill = varmean)) +
  labs(title = 'Mean variance across subjects') +
  scale_fill_gradientn(colours = c("white",'orange')) +
  guides(fill=guide_legend(title="Variance"))
(plot)


# Variance plot for between subject variance 

# Calculate the mean similarity for each participant 

datavar1 <-  group_by(dftrials, Colour1, Colour2, participant) %>% 
  summarise(
    count = n(), 
    meansim = mean(similarity, na.rm = TRUE),
  )

# Calculate the mean variance across subjects
datavar1  <-  group_by(datavar1, Colour1, Colour2) %>% 
  summarise(
    count = n(), 
    varsim = var(meansim, na.rm = TRUE),
  ) 


plot <- ggplot(datavar1, aes(x = Colour1, y = Colour2)) +
  theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# stuff that's standard across plot types
plot <- plot + geom_raster(aes(fill = varsim)) +
  labs(title = 'Variance of mean similarity') +
  scale_fill_gradientn(colours = c("white",'orange')) +
  guides(fill=guide_legend(title="Variance"))
(plot)



# Dissimplot for all data

dissimplot_temporal(dftrials,colors)

# Plot a dissmiliarity matrix for all subjects 

dissimplot_temporal_subject(dftrials, colors)



library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)


# Asymmtery matrix temporal

# Create asymmetry dataframes for each subject 

IDs <- unique(dftrials$ID) # Create list of participants to loop through 

for (ID in IDs){
  #Subset data for the subject
  
  subjectdf = dftrials[which(dftrials$ID == ID),] 
  
  datatemp <- dissimdata2(subjectdf, colors)
  
  nmdsdata <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean)
  nmdsdata$Colour1 <- nmdsdata$Group.1
  nmdsdata$Colour2 <- nmdsdata$Group.2
  nmdsdata = subset(nmdsdata, select = c("Colour1","Colour2","similarity"))  # get rid of unnecessary columns
  nmdsmatrix <- spread(nmdsdata, Colour1, similarity) # convert the dataframe to a matrix
  nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number(just some label stuff) 
  nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
  matdf <- as.data.frame(nmdsmatrix - t(nmdsmatrix)) # calculate the asymmetry
  matdf$colorset <- c(colors) # adding additional column "colorset"
  num_colors <- length(colors)
  matdf <- matdf %>% gather(othercolor,asymmetry,1:num_colors)
  matdf$ID <- ID # convert the matrix back to the data frame which has the 
  # column "colortset", "othercolor", "asymmetry"
  assign(paste("matdf", ID, sep = ""), matdf) # Rename for the subject number 
}

#subjectdf = dftrials[which(dftrials$ID == ID & dftrials$Ecc.1 == 'PF'),] 
#subjectdf1st <- dftrials[which(dftrials$trialnumber<=81&dftrials$ID==7),]

# Create a data frame with all asymmetry data
matdfall <- rbind(matdf1, matdf2, matdf3, matdf4, matdf5, matdf6, matdf7, matdf8, matdf9, matdf11, matdf12, matdf13, matdf14, matdf15)


# Perform one paired t test for each asymmetry by

color1 <- unique(matdfall$colors) # Create list of colours to loop through

for (c in color1){
  
  #FF0000 
  
  test1 <- t.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#FF0000')], mu = 0)
  print(paste("Colour #FF0000 and ", c))
  print(test1)
  
  #FFAA00
  
  test2 <- t.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#FFAA00')], mu = 0)
  print(paste("Colour #FFAA00 and ", c))
  print(test2)
  
  
  #AAFF00 
  
  test3 <- t.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#AAFF00')], mu = 0)
  print(paste("Colour #AAFF00  and ", c))
  print(test3)
  
  
  #00FF00
  
  test4 <- t.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#00FF00')], mu = 0)
  print(paste("Colour #00FF00 and ", c))
  print(test4)
  
  
  #00FFA9 
  
  test5 <- t.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor == '#00FFA9')], mu = 0)
  print(paste("Colour #00FFA9  and ", c))
  print(test5)
  
  
  #00A9FF 
  
  test6 <- t.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#00A9FF')], mu = 0)
  print(paste("Colour #00A9FF and ", c))
  print(test6)
  
  
  
  #0000FF 
  
  test7 <- t.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#0000FF')], mu = 0)
  print(paste("Colour #0000FF and ", c))
  print(test7)
  
  
  
  #AA00FF 
  
  test8 <- t.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#AA00FF')], mu = 0)
  print(paste("Colour #AA00FF  and ", c))
  print(test8)
  
  #FF00AA
  
  test9 <- t.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#FF00AA')], mu = 0)
  print(paste("Colour #FF00AA and ", c))
  print(test9)
}

# Variance of asymmetry 

color1 <- unique(matdfall$colors) # Create list of colours to loop through

for (c in color1){
  
  #FF0000 
  
  var1 <- var.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#FF0000')], matdfall$asymmetry[which(matdfall$colorset != c | matdfall$othercolor !='#FF0000')], alternative = "greater")
  print(paste("Colour #FF0000 and ", c))
  print(var1)
  
  
  #FFAA00
  
  var2 <- var.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#FFAA00')],matdfall$asymmetry[which(matdfall$colorset != c | matdfall$othercolor !='#FFAA00')], alternative = "greater")
  print(paste("Colour #FFAA00 and ", c))
  print(var2)
  
  #AAFF00 
  
  var3 <- var.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#AAFF00')], matdfall$asymmetry[which(matdfall$colorset != c | matdfall$othercolor !='#AAFF00')], alternative = 'greater')
  print(paste("Colour #AAFF00 and ", c))
  print(var3)
  
  
  #00FF00
  
  var4 <- var.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#00FF00')], matdfall$asymmetry[which(matdfall$colorset != c | matdfall$othercolor !='#00FF00')], alternative = 'greater')
  print(paste("Colour #00FF00 and ", c))
  print(var4)
  
  #00FFA9 
  
  var5 <- var.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#00FFA9')], matdfall$asymmetry[which(matdfall$colorset != c | matdfall$othercolor !='#00FFA9')], alternative = 'greater')
  print(paste("Colour #00FFA9 and ", c))
  print(var5)
  
  #00A9FF 
  
  var6 <- var.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#00A9FF')], matdfall$asymmetry[which(matdfall$colorset != c | matdfall$othercolor !='#00A9FF')], alternative = 'greater')
  print(paste("Colour #00A9FF and ", c))
  print(var6)
  
  
  #0000FF 
  
  var7 <- var.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#0000FF')], matdfall$asymmetry[which(matdfall$colorset != c | matdfall$othercolor !='#0000FF')], alternative = 'greater')
  print(paste("Colour ##0000FF and ", c))
  print(var7)
  
  
  #AA00FF 
  
  var8 <- var.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#AA00FF')], matdfall$asymmetry[which(matdfall$colorset != c | matdfall$othercolor !='#AA00FF')], alternative = 'greater')
  print(paste("Colour #AA00FF  and ", c))
  print(var8)
  
  #FF00AA
  
  var9 <- var.test(matdfall$asymmetry[which(matdfall$colorset == c & matdfall$othercolor =='#FF00AA')], matdfall$asymmetry[which(matdfall$colorset != c | matdfall$othercolor !='#FF00AA')], alternative = 'greater')
  print(paste("Colour #FF00AA and ", c))
  print(var9)
}


# plot an asymmetry matrix for all data

# Plot variance of asymmetry between subject response variance 

asymvar <- aggregate(matdfall, by = list(matdfall$colors, matdfall$othercolor),FUN=var) # Calculate the variance of the asymmtery 

plot <- ggplot(asymvar, aes(x = Group.1, y = Group.2)) +
  theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# stuff that's standard across plot types
plot <- plot + geom_raster(aes(fill = asymmetry)) +
  labs(title = 'Variance of asymmetry between subject response variance') +
  scale_fill_gradientn(colours = c("white",'orange')) +
  guides(fill=guide_legend(title="Variance"))
(plot)


# Plot variance of asymmetry within subject response variance 

# Create 4 9x9 matrices for each subject and then add that data to have 4 asymmetry values

# Subset the 4 dataframes for the matrices 
for (ID in IDs){
  
  subjectdf <- dftrials[which(dftrials$ID==ID),]
  
  subjectdf1 <- subjectdf[which(subjectdf$trialnumber<=162 & subjectdf$maskCon1=='strong'),]
  subjectdf2 <- subjectdf[which(subjectdf$trialnumber<=162 & subjectdf$maskCon1=='weak'),]
  subjectdf3 <- subjectdf[which(subjectdf$trialnumber>=163 & subjectdf$maskCon1=='strong'),]
  subjectdf4 <- subjectdf[which(subjectdf$trialnumber>=163 & subjectdf$maskCon1=='weak'),]
  
  subjectdfnames <- list(subjectdf1, subjectdf2, subjectdf3, subjectdf4) # Create a a list of dataframes
  matlist <- list() # Create an empty list to store the asym mats
  
  # loop through asym mat 4 times for each df
  for (i in 1:4){
    
    datatemp <- dissimdata2(subjectdfnames[[i]], colors)
    
    
    nmdsdata <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean)
    nmdsdata$Colour1 <- nmdsdata$Group.1
    nmdsdata$Colour2 <- nmdsdata$Group.2
    nmdsdata = subset(nmdsdata, select = c("Colour1","Colour2","similarity"))  # get rid of unnecessary columns
    nmdsmatrix <- spread(nmdsdata, Colour1, similarity) # convert the dataframe to a matrix
    nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number(just some label stuff) 
    nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
    matdf <- as.data.frame(nmdsmatrix - t(nmdsmatrix)) # calculate the asymmetry
    matdf$colorset <- c(colors) # adding additional column "colorset"
    num_colors <- length(colors)
    matdf <- matdf %>% gather(othercolor,asymmetry,1:num_colors) # convert the matrix back to the data frame which has the column "colortset", "othercolor", "asymmetry"
    matdf$ID <- ID 
    matlist[[i]] <- matdf # Store the df in the list
    assign(paste("matlist", ID, sep = ""), matlist)# Rename for the subject number  
    
  }
}

# Bind the matrices for each subject together and calculate the variance of the asymmetry 

matbind1 = do.call(rbind, matlist1)
matasmy1 <- aggregate(matbind1, by = list(matbind1$colors, matbind1$othercolor),FUN=var)
matbind2 = do.call(rbind, matlist2)
matasmy2 <- aggregate(matbind2, by = list(matbind2$colors, matbind2$othercolor),FUN=var)
matbind3 = do.call(rbind, matlist3)
matasmy3 <- aggregate(matbind3, by = list(matbind3$colors, matbind3$othercolor),FUN=var)
matbind4 = do.call(rbind, matlist4)
matasmy4 <- aggregate(matbind4, by = list(matbind4$colors, matbind4$othercolor),FUN=var)
matbind5 = do.call(rbind, matlist5)
matasmy5 <- aggregate(matbind5, by = list(matbind5$colors, matbind5$othercolor),FUN=var)
matbind6 = do.call(rbind, matlist6)
matasmy6 <- aggregate(matbind6, by = list(matbind6$colors, matbind6$othercolor),FUN=var)
matbind7 = do.call(rbind, matlist7)
matasmy7 <- aggregate(matbind7, by = list(matbind7$colors, matbind7$othercolor),FUN=var)
matbind8 = do.call(rbind, matlist8)
matasmy8 <- aggregate(matbind8, by = list(matbind8$colors, matbind8$othercolor),FUN=var)
matbind9 = do.call(rbind, matlist9)
matasmy9 <- aggregate(matbind9, by = list(matbind9$colors, matbind9$othercolor),FUN=var)
matbind11 = do.call(rbind, matlist11)
matasmy11 <- aggregate(matbind11, by = list(matbind11$colors, matbind11$othercolor),FUN=var)
matbind12 = do.call(rbind, matlist12)
matasmy12 <- aggregate(matbind12, by = list(matbind12$colors, matbind12$othercolor),FUN=var)
matbind13 = do.call(rbind, matlist13)
matasmy13 <- aggregate(matbind13, by = list(matbind13$colors, matbind13$othercolor),FUN=var)
matbind14 = do.call(rbind, matlist14)
matasmy14 <- aggregate(matbind14, by = list(matbind14$colors, matbind14$othercolor),FUN=var)
matbind15 = do.call(rbind, matlist15)
matasmy15 <- aggregate(matbind15, by = list(matbind15$colors, matbind15$othercolor),FUN=var)

matasymmall <- rbind(matasmy1, matasmy2, matasmy3, matasmy4, matasmy5, matasmy6, matasmy7, matasmy8, matasmy9, matasmy11, matasmy12, matasmy13, matasmy14, matasmy15)


matasymmall  <-  group_by(matasymmall, Group.1, Group.2) %>% 
  summarise(
    count = n(), 
    meanvar = mean(asymmetry, na.rm = TRUE),
  )

plot <- ggplot(matasymmall, aes(x = Group.1, y = Group.2)) +
  theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# stuff that's standard across plot types
plot <- plot + geom_raster(aes(fill = meanvar)) +
  labs(title = 'Asymmetry for all data') +
  scale_fill_gradientn(colours = c("white",'orange')) +
  guides(fill=guide_legend(title="Variance"))
(plot)

# Plot similarity rating as a function of the trial number 

subjectdf1st <- dftrials[which(dftrials$trialnumber<=81&dftrials$ID==7),]
subjectdf2nd <- dftrials[which(dftrials$trialnumber>=163&dftrials$trialnumber<=243&dftrials$ID==7),]
ggplot()+
  geom_point(aes(x=trialnumber, y=similarity), data=subjectdf1st)+ geom_line(aes(x=trialnumber, y=similarity), data=subjectdf1st)+
  geom_point(aes(x=trialnumber-(161.9), y=similarity), data=subjectdf2nd, colour='red')+ geom_line(aes(x=trialnumber-(161.9), y=similarity), data=subjectdf2nd, colour='red')+
  ggtitle("Participant 7") +
  xlab("Trial Number") + ylab("Similarity")

subjectdf <- dftrials[which(dftrials$ID==1),]
subjectdf$dif <- NA
subjectdf$dif <- subjectdf$similarity[which(subjectdf$trialnumber<=162)] - subjectdf$similarity[which(subjectdf$trialnumber>=163)]


ggplot()+
  geom_point(aes(x=trialnumber, y=dif), data=subjectdf)+ geom_line(aes(x=trialnumber, y=dif), data=subjectdf)

subjectdf <- dftrials[which(dftrials$ID==15),]
matrixcor_pear(subjectdf)
matrixcor_spear(subjectdf)

# Plot an asymmetry matrix for all subjects 

asymmetry_plot_temporal_subject(dftrials, colors)
