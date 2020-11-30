#function to plot VS neuron
plotVS <- function(vs,id){
  img_list <- list()
  tuk <- c()
  for (i in 1:nrow(vs)) {
    show(i)
    cell <- vs[i,2]
    phase <- vs[i,3]
    if (phase == 'learn'){
      n_learn <- gsub("X",as.character(cell),"CellX_learnEvents_P")
      n_learn <- gsub("P",id,n_learn)
      n_time <- gsub("X",as.character(cell),"CellX_times_P")
      n_time <- gsub("P",id,n_time)
      
      cell_learn <- eval(as.symbol(n_learn))
      cell_time <- eval(as.symbol(n_time))
      
      #resize the timescale
      cell_time$Time <- cell_time$Time / 1e6
      cell_learn[,1:4] <- cell_learn[,1:4] / 1e6
      cell_trial <- cell_learn
    }else{
      n_recall <- gsub("X",as.character(cell),"CellX_RecallEvents_P")
      n_recall <- gsub("P",id,n_recall)
      n_time <- gsub("X",as.character(cell),"CellX_times_P")
      n_time <- gsub("P",id,n_time)
      
      cell_recall <- eval(as.symbol(n_recall))
      cell_time <- eval(as.symbol(n_time))
      
      #resize the timescale
      cell_time$Time <- cell_time$Time / 1e6
      cell_recall[,1:4] <- cell_recall[,1:4] / 1e6
      cell_trial <- cell_recall
    }
    cell_trial <- rbind(cell_trial[which(cell_trial$Category==unique(cell_trial$Category)[1]),],
                        cell_trial[which(cell_trial$Category==unique(cell_trial$Category)[2]),],
                        cell_trial[which(cell_trial$Category==unique(cell_trial$Category)[3]),],
                        cell_trial[which(cell_trial$Category==unique(cell_trial$Category)[4]),],
                        cell_trial[which(cell_trial$Category==unique(cell_trial$Category)[5]),])
                        
    #plot
    df <- data.frame()
    df3 <- data.frame()
    for (i in 1:nrow(cell_trial)){
      trial <- cell_time$Time[cell_time$Time > (cell_trial$On[i] - 1) & cell_time$Time < (cell_trial$On[i] + 2)]
      trial <- trial - cell_trial$On[i]
      cat <- cell_trial$Category[i]
      df1 <- data.frame(trial,Category=rep(cat,length(trial)),number=rep(i,length(trial)))
      df <- rbind(df,df1)
      
      trial2 <- cell_time$Time[cell_time$Time > (cell_trial$On[i] + 0.2) & cell_time$Time < (cell_trial$On[i] + 1.7)]
      trial2 <- trial2 - cell_trial$On[i]
      cat2 <- cell_trial$Category[i]
      df2 <- data.frame(trial2,Category=rep(cat2,length(trial2)),number=rep(i,length(trial2)))
      df3 <- rbind(df3,df2)
    }
    model <- aov(trial2~Category,data=df3)
    tu <- TukeyHSD(model)
    tuk <- c(tuk,tu)
    
    df <- df[order(df$Category),]
    row.names(df) <- NULL
    raster <- ggplot(df,aes(y=number,x=trial,col=Category))+annotate('rect',xmin=0,xmax = 1,ymin=-Inf,ymax = Inf,fill='gray',alpha=0.5) + geom_point(size=0.1) + scale_color_npg() + ylab('Trial number (resorted)') + xlab('') +xlim(-1,2)+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))
    psth <- ggplot(df,aes(x=trial,col=Category)) +annotate('rect',xmin=0,xmax = 1,ymin=-Inf,ymax = Inf,fill='gray',alpha=0.5)+ stat_bin(geom = 'line',bins = 12,position = 'identity') + scale_color_npg() + ylab('Firing rate (spikes/s)') + xlab('Time(s)')+xlim(-1,2)+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"))
    gA <- ggplotGrob(raster)
    gB <- ggplotGrob(psth)
    maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
    gA$widths[2:5] <- as.list(maxWidth)
    gB$widths[2:5] <- as.list(maxWidth)
    img <- grid.arrange(gA, gB, ncol=1)
    show(img)
  }
  return(tuk)
}

plotVS(vs15,'P15')
plotVS(vs38,'P38')
plotVS(vs51,'P51')
#vs38 1(-cars) 4 7 8  9(animals)
#vs51 2 6 7