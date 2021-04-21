##Functions

# Select relevant trials for catch trials 
get.catchtrial.info <- function(df.catchtrialorder){
  info <- (unique(df.catchtrialorder)[2])
  info <- as.character(info) # convert to string
  info <- str_sub(info, 2, -2) # remove the square brackets
  info <- str_split(info, pattern = fixed(',')) # get a vector of the catch trials in string format
  info <- info[[1]]
  #print(info) # testing
  info <- as.numeric(info) # convert to numeric
  return(info)
}

add.catchtrial.info <- function(df){
  IDs <- unique(df$participant)
  colnames <- colnames(df)
  output.df <- df[FALSE,]
  for(ID in IDs){
    tempdf <- subset(df, participant == ID)
    catch.trials <- get.catchtrial.info(tempdf$catchtrialorder)
    tempdf$catch.trial <- ifelse(is.element(tempdf$trialnumber,catch.trials),TRUE,FALSE)
    #print(colnames(tempdf)) #testing
    output.df <- rbind(output.df,tempdf)
  }  
  return(output.df)
  
  data$catch.trials <- NA # need to add this here to make stuff work nicely later
  test <- add.catchtrial.info(data)
  
}


# Check catch scores 
catch_trial_checker <- function(datadf){
  
  subjectlist <- sort(unique(test$participant))
  print("Catch scores")
  for (participant in subjectlist){
    subjectdf <- test[which(test$participant == participant),] 
    catch_trials <- subset(subjectdf, catch.trial == TRUE)
    catch_num = nrow(catch_trials)
    catch_correct = nrow(subset(catch_trials, catchnumber == catchresponse))
    
    print(paste("Subject",participant,":",catch_correct,"/",catch_num))
  }
}

# Screen parameters 
# Screen size function 
screen_size <- function(dftrials){
  
  dftrials<- subset(dftrials, !is.na(screen_size_x), !is.na(screen_size_y))
  
  width <- as.numeric(substr(as.character(dftrials$screen_size_x)[1],1,6))
  height <- as.numeric(substr(as.character(dftrials$screen_size_y)[1],1,6))
  
  # use pythagoras to just get the hypotenuse. Subjects have a fixed 16/9 aspect ratio so these are all comparable
  return(sqrt(width*width + height*height))
}


# View distance function 
view_distance <- function(datadf){
  return(as.numeric(substr(as.character(datadf$viewer_distance)[1],1,6)))
}


# Calculate screen parameters for each participant 
screen_parameters <- function(dftrials,individual=FALSE){
  
  subjectlist <- sort(unique(dftrials$participant))
  print("Screen Parameters")
  screen_fail = 0
  viewing_fail = 0
  for (participant in subjectlist){
    
    subjectdf <- dftrials[which(dftrials$participant == participant),] 
    
    
    screen_size <- round(screen_size(subjectdf)/10,1)
    viewing_distance <- round(view_distance(subjectdf)/10,1)
    
    if(screen_size < 20){screen_fail = screen_fail + 1}
    if(viewing_distance < 30){viewing_fail = viewing_fail + 1}
    
    if(individual){
      print(paste("Subject",participant,":"))
      print(paste("Screen size:",screen_size,"cm"))
      print(paste("Viewing distance:",viewing_distance,"cm"))
      print("")
    }
    
    
  }
  print("")
  print(paste("Screen size issues:",screen_fail,"/",length(subjectlist)))
  print(paste("Viewing distance issues:",viewing_fail,"/",length(subjectlist)))
}  


rt_avg <- function(data){
  return(median(data$response_time))
}


rt_avg_check <- function(dftrials){
  c = 1
  subjectlist <- sort(unique(dftrials$participant))
  print("RT avg")
  for (participant in subjectlist){
    subjectdf <- dftrials[which(dftrials$participant == participant),] 
    rt = rt_avg(subjectdf)
    print(paste("Subject:",c ,"mean rt",rt))
    c = c + 1
  }
}


# Similarity judgment histogram
simhistplot <- function(datadf){
  
  plot <- ggplot(dftrials, aes(x = similarity)) + geom_bar(aes(y = ..prop..)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
  return(plot)
}


simhistplot_summary <- function(datadf){
  
  datadf$subject <- as.character(datadf$subject) # necessary for visualisation
  
  plot <- ggplot(dftrials, aes(x = similarity)) + 
    geom_line(stat='count',aes(y = ..prop..,group = subject),color='#CC9933') +
    geom_line(stat='count',aes(y = ..prop..),size=1.5) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
  return(plot)
  
}


# reaction time for each similarity
rsplot <- function(datadf){
  
  plot <- ggplot(dftrials, aes(x= similarity, y=response_time)) + 
    stat_summary(fun.y = mean, geom = "bar") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", size =0.5, aes(width=0.5)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') + ylab('Reaction Time (s)') +
    theme(legend.position = "none") +
    ylim(0,4) # anyone taking more than 4 seconds has probably mindwandered
  
  return(plot)
}


rsplot_all <- function(data){
  subjectlist <- sort(unique(dftrials$ID))
  par(mfrow=c(3,5))
  for (ID in subjectlist){
    subjectdf <- dftrials[which(dftrials$ID==ID),]
    plot <- rsplot(subjectdf)
    return(plot)
  }
}


# correlation between reaction times and similarity judgements
# grouping at individual trial, individual participant, experiment or entire population level
rt_similarity_cor <- function(dftrials,level='participant'){
  
  if(level=="participant"){
    dftrials<- dftrials%>% 
      group_by(ID) %>% 
      mutate(rt_similarity_correlation = cor(similarity,response_time))
    dftrials <- aggregate(dftrials, by=list(dftrials$ID), FUN = mean)
    
    
  }
  return(dftrials)
  
}

rt_similarity_cor <- function(datadf,level='participant'){
  
  if(level=="participant"){
    datadf <- datadf %>% 
      group_by(ID) %>% 
      mutate(rt_similarity_correlation = cor(similarity,response_time))
    datadf <- aggregate(datadf, by=list(datadf$ID), FUN = mean)
    
  }
  return(datadf)
  
}

rt_similarity_plot <- function(dftrials,xlabel='BLANK'){
  
  datadf <- rt_similarity_cor(dftrials)
  
  datadf[xlabel] = xlabel
  
  plot <- ggplot(datadf,aes(x=xlabel,y=rt_similarity_correlation)) + 
    geom_boxplot() + 
    geom_dotplot(binaxis='y',stackdir='center',dotsize=0.75) +
    theme(text = element_text(size=15)) + xlab("")
  ggtitle(title)
  
  plot <- plot + ylab("Correlation (Spearman)") + ylim(-1,1)
  plot <- plot + geom_hline(yintercept=0, linetype="dashed", color = "blue")
  return(plot)
}


# reaction time raincloud plot
rsplot_raincloud <- function(datadf,xtype='linear'){
  
  datadf$ID <- as.character(datadf$ID) # necessary for visualisation  
  datadf$similarity <- as.character(datadf$similarity) # necessary for visualisation
  
  ylabtext = 'Reaction Time (ms)'
  
  plot <- ggplot(datadf, aes(y = response_time, x = similarity, fill = similarity)) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = response_time, color = similarity),
               position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
    expand_limits(x = 5.25) +
    guides(fill = FALSE) +
    guides(color = FALSE) +
    scale_color_brewer(palette = "Spectral") +
    scale_fill_brewer(palette = "Spectral") +
    xlab('Dissimilarity') + ylab("Reaction Time (ms)")
  # coord_flip() +
  theme_bw() +
    raincloud_theme
  
  if(xtype == 'log'){
    plot <- plot + scale_y_continuous(trans='log10')
  } else{
    plot <- plot + ylim(0,5000)
  }
  
  return(plot)
}


# correlation between reaction times and similarity judgements
# grouping at individual trial, individual participant, experiment or entire population level
rt_similarity_cor <- function(datadf,level='participant'){
  
  if(level=="participant"){
    datadf <- datadf %>% 
      group_by(ID) %>% 
      mutate(rt_similarity_correlation = cor(similarity,response_time))
    datadf <- aggregate(datadf, by=list(datadf$ID), FUN = mean)
    
    
  }
  return(datadf)
  
}


rt_similarity_plot <- function(datadf,xlabel='BLANK'){
  
  datadf <- rt_similarity_cor(datadf)
  
  datadf[xlabel] = xlabel
  
  plot <- ggplot(datadf,aes(x=xlabel,y=rt_similarity_correlation)) + 
    geom_boxplot() + 
    geom_dotplot(binaxis='y',stackdir='center',dotsize=0.75) +
    theme(text = element_text(size=15)) + xlab("")
  ggtitle(title)
  
  plot <- plot + ylab("Correlation (Spearman)") + ylim(-1,1)
  plot <- plot + geom_hline(yintercept=0, linetype="dashed", color = "blue")
  return(plot)
}


# subject info
sumplot <- function(datadf){
  
  # change ms to s, add the delay for each trial
  datadf$response_time <- ((datadf$response_time + 0.125*nrow(datadf)) / 1000)
  
  plot <- ggplot(datadf, aes(x=subject, y = response_time)) + 
    stat_summary(fun.y = sum, geom = "bar") + ylim(0,1000) +
    ylab('Response Time Total') + theme(axis.title.x=element_blank(), axis.text.x = element_text(size=20))
  
  return(plot)
}


# get median reaction time
rt_avg <- function(datadf){
  return(median(datadf$response_time))
}


# function to aggregate everyone's data together
aggregate_df <- function(datadf,dependent='color'){
  
  # aggregate everyone's data together for the matrices
  everyonedata <- aggregate(datadf, by=list(
    datadf$Color_1,
    datadf$Color_2,
    datadf$Circle_1,
    datadf$Circle_2,
    datadf$bin1,
    datadf$bin2
  ), FUN=mean, 
  )
  
  # correct the column names
  everyonedata$Color_1 <- everyonedata$Group.1
  everyonedata$Color_2 <- everyonedata$Group.2
  everyonedata$Circle_1 <- everyonedata$Group.3
  everyonedata$Circle_2 <- everyonedata$Group.4
  everyonedata$bin1 <- everyonedata$Group.5
  everyonedata$bin2 <- everyonedata$Group.6
  
  return(everyonedata)
}




# factor the dataframes for the plot function
dissimdata2 <- function(dftrials, colors){
  
  # refactor the levels so they can be plotted properly later if need be
  dftrials$Colour1 <- with(dftrials, factor(Colour1, levels = colors))
  dftrials$Colour2 <- with(dftrials, factor(Colour2, levels = colors))
  
  return(dftrials)
}

quantify_asymmetry <- function(dftrials){
  
  data <- dissimdata2(dftrials, colors)
  
  # aggregate over the remaining columns of interest
  nmdsdata <- aggregate(data, by = list(data$Colour1, data$Colour2),FUN=mean)
  nmdsdata$Colour1 <- nmdsdata$Group.1
  nmdsdata$Colour2 <- nmdsdata$Group.2
  
  nmdsdata = subset(nmdsdata, select = c("Colour1","Colour2","similarity"))  # get rid of unnecessary columns
  
  nmdsmatrix <- spread(nmdsdata, Colour1, similarity) # convert the dataframe to a matrix
  nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number (just some label stuff) 
  nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
  nmdsmatrix[is.na(nmdsmatrix)] <- 0  # change NA to 0 so sum can be calculated.
  
  matdf <- as.data.frame(as.vector(abs(nmdsmatrix - t(nmdsmatrix)))) # calculate the asymmetry
  asymmery_value <- sum(matdf)/2 # need to divide by 2 to get rid of the duplicates
  
  return(asymmery_value)
}


# return a list of the asymmetrical values for each subject
asymValues_list2 <- function(datadf){
  
  subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
  
  asymValues_list <- vector() # array to store the values in
  
  for (ID in subjectlist){ # go through subject by subject
    subjectdf <-  dftrials[which(dftrials$ID == ID),] 
    # select the ID for subject of interest
    asymValues_list <- c(asymValues_list, quantify_asymmetry(subjectdf))
  }
  return(asymValues_list)
}


df2mat.full <- function(dftrials){
  
  
  # aggregate over the remaining columns of interest
  datadf <- aggregate(dftrials, by = list(dftrials$Colour1, dftrials$Colour2),FUN=mean)
  datadf$Colour1 <- datadf$Group.1
  datadf$Colour2 <- datadf$Group.2
  
  datadf = subset(datadf, select = c("Colour1","Colour2","similarity"))  # get rid of unnecessary columns
  datadf <- spread(datadf, Colour1, similarity)
  
  # convert the dataframe to a matrix
  datamatrix <- data.matrix(datadf)
  datamatrix <- datamatrix[,-1] # get rid of the labels in the first column, it messes up the code
  rownames(datamatrix) <- colnames(datamatrix)
  return(datamatrix)
  
}


# Dissimplot for all data

# Variance plot for between subject variance 

# Calculate the mean similarity for each participant 
dissimplot_temporal <- function(dftrials,colors,dependent='color'){
  
  # refine data using function "dissimdata2 "
  datatemp <- dissimdata2(dftrials, colors)
  datatemp <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean)
  
  plot <- ggplot(datatemp, aes(x = Group.1, y = Group.2)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = similarity)) +
    labs(title = 'Dissimilarity plot for all data') +
    scale_fill_gradientn(colours = c("white","black"), limits = c(0, 7), n.breaks = 8) +
    guides(fill=guide_legend(title="Dissimilarity"))
  return(plot)
}


# CORRELATION BETWEEN PASSES 

pass_compare_list_Fisher <- function(dftrials){
  
  subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
  
  correlation_list <- vector() # array to store the values in
  
  for (ID in subjectlist){ # go through subject by subject
    subjectdf <-  dftrials[which(dftrials$ID == ID),]  # select the ID for subject of interest
    correlation_list <- c(correlation_list, FisherZ((matrixcor_pass(subjectdf))))
    
    plot <- plot(correlation_list, main = '1st and 2nd pass correlation - z',
                 xlab='Participant',ylab='z',xlim=c(1,14))
    axis <- axis(1,seq(1,14,1))
  }
  return(correlation_list)
  return(plot)
  return(axis)
}

matrixcor_pear <- function(dftrials){
  
  matrix1 <- df2mat.full(dftrials[which(dftrials$trialnumber<=162),])
  matrix2 <- df2mat.full(dftrials[which(dftrials$trialnumber>=163),])
  return(cor(c(matrix1), c(matrix2), method = "pearson"))
}

matrixcor_spear <- function(dftrials){
  
  matrix1 <- df2mat.full(dftrials[which(dftrials$trialnumber<=162),])
  matrix2 <- df2mat.full(dftrials[which(dftrials$trialnumber>=163),])
  return(cor(c(matrix1), c(matrix2), method = "spearman"))
}


pass_compare_list_plot <- function(dftrials){
  
  subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
  
  correlation_list <- vector() # array to store the values in
  
  for (ID in subjectlist){ # go through subject by subject
    subjectdf <-  dftrials[which(dftrials$ID == ID),]  # select the ID for subject of interest
    correlation_list <- c(correlation_list, (matrixcor_pear(subjectdf)))
    
    plot <- plot(correlation_list, main = '1st and 2nd pass Pearson correlation - r',
                 xlab='Participant',ylab='r',xlim=c(1,14),pch = 21, col="black")
    axis <- axis(1,seq(1,14,1))
  }
  return(correlation_list)
  return(plot)
  return(axis)
}



# Plot a dissmiliarity matrix for all subjects 
dissimplot_temporal_subject <- function(dftrials, colors){
  
  IDs <- unique(dftrials$ID)
  plot_list <- list()
  
  for (ID in IDs){
    #Subset data for the subject
    
    subjectdf = dftrials[which(dftrials$ID == ID),] 
    
    # refine data using function "dissimdata2 "
    datatemp <- dissimdata2(subjectdf, colors)
    datatemp <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean)
    
    
    plot <- ggplot(datatemp, aes(x = Group.1, y = Group.2)) +
      theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))+
      ggtitle(paste("Subject ID:", ID))
    
    
    # stuff that's standard across plot types
    plot <- plot + geom_raster(aes(fill = similarity)) +
      scale_fill_gradientn(colours = c("white","black"), limits = c(0, 7), n.breaks = 8) +
      guides(fill=guide_legend(title="Dissimilarity"))
    
    plot_list[[ID]] <- plot
    
  }
  plot_grob <- arrangeGrob(grobs=plot_list)
  return(grid.arrange(plot_grob))
}


# Asymmtery matrix temporal

df2mat_asymmetry_temporal <- function(subjectdf){
  
  datatemp <- dissimdata2(subjectdf, colors)
  
  # aggregate over the remaining columns of interest
  nmdsdata <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean)
  nmdsdata$Colour1 <- nmdsdata$Group.1
  nmdsdata$Colour2 <- nmdsdata$Group.2
  
  nmdsdata = subset(nmdsdata, select = c("Colour1","Colour2","similarity",'participant'))  # get rid of unnecessary columns
  nmdsmatrix <- spread(nmdsdata, Colour1, similarity) # convert the dataframe to a matrix
  nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number(just some label stuff) 
  nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
  nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
  
  matdf<-  as.data.frame(nmdsmatrix - t(nmdsmatrix)) # calculate the asymmetry
  matdf$colorset <- c(colors) # adding additional column "colorset"
  num_colors <- length(colors)
  matdf <- matdf %>% gather(othercolor,asymmetry ,1:num_colors) # convert the matrix back to the data frame which has the 
  # column "colortset", "othercolor", "asymmetry"
  return(matdf)
}



# Create asymmetry dataframes for each subject 


# plot an asymmetry matrix for all data
asymmetry_plot_temporal <- function(subjectdf, colors){
  
  datatemp <- df2mat_asymmetry_temporal(subjectdf)
  
  # refactor the levels so they can be plotted properly later if need be
  datatemp$colorset <- with(datatemp, factor(colorset, levels = colors))
  datatemp$othercolor <- with(datatemp, factor(othercolor, levels = colors))
  
  plot <- ggplot(datatemp, aes(x = colorset, y = othercolor)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          #axis.title.x = element_text("left"), axis.title.y = element_text("right"),
          plot.title = element_text(hjust = 0.5))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = asymmetry)) +
    labs(title = 'Presented - Response Screen') +
    scale_fill_gradientn(colours = c("blue","white","red"), limits = c(-4,4)) +
    guides(fill=guide_legend(title="Dissimilarity\nAsymmetry"))
  return(plot)
}


# Plot an asymmetry matrix for all subjects 
asymmetry_plot_temporal_subject <- function(dftrials, colors){
  
  IDs <- unique(dftrials$ID)
  plot_list <- list()
  
  for (ID in IDs){
    #Subset data for the subject
    
    subjectdf = dftrials[which(dftrials$ID == ID),] 
    datatemp <- df2mat_asymmetry_temporal(subjectdf)
    
    # refactor the levels so they can be plotted properly later if need be
    datatemp$colorset <- with(datatemp, factor(colorset, levels = colors))
    datatemp$othercolor <- with(datatemp, factor(othercolor, levels = colors))
    
    plot <- ggplot(datatemp, aes(x = colorset, y = othercolor)) +
      theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))+
      ggtitle(paste("Subject ID:", ID))
    
    # stuff that's standard across plot types
    plot <- plot + geom_raster(aes(fill = asymmetry)) +
      scale_fill_gradientn(colours = c("blue","white","red"), limits = c(-4,4)) +
      guides(fill=guide_legend(title="Dissimilarity\nAsymmetry"))
    
    plot_list[[ID]] <- plot
  }
  plot_grob <- arrangeGrob(grobs=plot_list)
  return(grid.arrange(plot_grob))
}



MDS_plot_individual <- function(dftrials_strong, dftrials_weak,colors,ID)
{
  avg_strong = dftrials_strong %>% group_by(Colour1,Colour2) %>% summarise(mean_sim = mean(similarity))
  avg_weak = dftrials_weak %>% group_by(Colour1,Colour2) %>% summarise(mean_sim = mean(similarity))
  
  plot_list <- list()
  
  matrix_strong = data.frame(Colour2 = colors)
  for (i in colors)
  {
    temp = avg_strong %>% filter(Colour1 == i) %>% ungroup() %>% select(-Colour1)
    matrix_strong = left_join(matrix_strong,temp,by = 'Colour2')
  }
  rownames(matrix_strong) = matrix_strong$Colour2
  matrix_strong = matrix_strong %>% select(-Colour2)
  colnames(matrix_strong) = colors
  
  strong_mds = mds(delta = as.matrix(matrix_strong), ndim = 2, type='interval',init='torgerson')
  print(strong_mds$stress)
  strong_temp = strong_mds$conf %>% as_tibble()
  rownames(strong_temp) = colors
  strong_fig = ggplot(strong_temp,aes(x = D1, y = D2)) + geom_point(size= 3) + geom_text(aes(label = rownames(strong_temp)),nudge_y = -0.1)
  strong_fig = strong_fig + ggtitle(paste("participant:", ID,"strong mask"))
  plot_list[[1]] <- strong_fig
  
  matrix_weak = data.frame(Colour2 = colors)
  for (i in colors)
  {
    temp = avg_weak %>% filter(Colour1 == i) %>% ungroup() %>% select(-Colour1)
    matrix_weak = left_join(matrix_weak,temp,by = 'Colour2')
  }
  rownames(matrix_weak) = matrix_weak$Colour2
  matrix_weak = matrix_weak %>% select(-Colour2)
  colnames(matrix_weak) = colors
  
  weak_mds = mds(delta = as.matrix(matrix_weak), ndim = 2, type='interval',init='torgerson')
  print(weak_mds$stress)
  weak_temp = weak_mds$conf %>% as_tibble()
  rownames(weak_temp) = colors
  weak_fig = ggplot(weak_temp,aes(x = D1, y = D2)) + geom_point(size= 3) + geom_text(aes(label = rownames(weak_temp)),nudge_y = -0.1)
  weak_fig = weak_fig + ggtitle(paste("participant:", ID,"weak mask"))
  plot_list[[2]] <- weak_fig
  
  
  plot_grob <- arrangeGrob(grobs=plot_list)
  return(grid.arrange(plot_grob))
}