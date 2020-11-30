identifyVS <- function(patient,id){
  
  res <- data.frame()
  res_r <- data.frame()
  p_list <- list()
  for (j in 1:(length(patient)/4)){
    n_learn <- gsub("X",as.character(j),"CellX_learnEvents_P")
    n_learn <- gsub("P",id,n_learn)
    n_recall <- gsub("X",as.character(j),"CellX_RecallEvents_P")
    n_recall <- gsub("P",id,n_recall)
    n_time <- gsub("X",as.character(j),"CellX_times_P")
    n_time <- gsub("P",id,n_time)
    
    cell_learn <- eval(as.symbol(n_learn))
    cell_recall <- eval(as.symbol(n_recall))
    cell_time <- eval(as.symbol(n_time))
    
    cell_time$Time <- cell_time$Time / 1e6
    cell_learn[,1:4] <- cell_learn[,1:4] / 1e6
    cell_recall[,1:4] <- cell_recall[,1:4] / 1e6
    
    #identify VS neurons from learning phase  
    df_l <- data.frame()
    for (i in 1:nrow(cell_learn)){
      trial <- cell_time$Time[cell_time$Time > (cell_learn$On[i] - 0) & cell_time$Time < (cell_learn$On[i] + 1.4)]
      trial <- trial - cell_learn$On[i]
      cat <- cell_learn$Category[i]
      df1 <- data.frame(trial,Category=rep(cat,length(trial)),Cell=rep(j,length(trial)))
      df_l <- rbind(df_l,df1)
    }
    #names(df)[2] <- 'Category'
    #show(length(df$Category))
    if (length(df_l$Category)>2){
      model <- aov(trial~Category,data=df_l)
      su <- summary(model)
      tu <- TukeyHSD(model)
      ano <- data.frame(p_value=su[[1]][["Pr(>F)"]][[1]],Cell = j)
      res <- rbind(res,ano)
    }
    
    #identify VS neurons from recall phase  
    df_r <- data.frame()
    for (i in 1:nrow(cell_recall)){
      trial <- cell_time$Time[cell_time$Time > (cell_recall$On[i] - 0) & cell_time$Time < (cell_recall$On[i] + 1.4)]
      trial <- trial - cell_recall$On[i]
      cat <- cell_recall$Category[i]
      df <- data.frame(trial,Category=rep(cat,length(trial)),Cell=rep(j,length(trial)))
      df_r <- rbind(df_r,df)
    }
    if (length(df_r$Category)>2){
      model_r <- aov(trial~Category,data=df_r)
      su_r <- summary(model_r)
      tu_r <- TukeyHSD(model_r)
      ano_r <- data.frame(p_value=su_r[[1]][["Pr(>F)"]][[1]],Cell = j)
      res_r <- rbind(res_r,ano_r)
    }
  }
  vs <- rbind(res[which(res$p_value<0.05),],res_r[which(res_r$p_value<0.05),])
  
  return(vs)
}

vs15 <- identifyVS(temp_p15,'P15')
vs38 <- identifyVS(temp_p15,'P38')
vs51 <- identifyVS(temp_p15,'P51')
