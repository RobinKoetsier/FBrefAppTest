filterData <- function(df,Xx,Yy,minNinety,compi){
  df <- df %>% filter(`90s` >= minNinety) %>% filter(comp %in% compi)
  df <- df %>% select(Player,`90s`,Xx,Yy)
 # idvec <- grep(Xx, colnames(df), value = TRUE)
#  df<-df[c("Player",idvec, Yy,"90s")]
  df$xAxis <- colnames(df)[3]
  df$yAxis <- colnames(df)[4]
  colnames(df)[3]<-"X"
  colnames(df)[4]<-"Y"
  subtitle <- ""
  for(i in 1:length(unique(df$comp))){
    subtitle <- paste(subtitle,unique(df$comp)[i])
  }
  df$subtitle <- subtitle
  return(df)
}

scatterMaken <- function(df,percX,percY){
  ggplot(df,aes(x=as.integer(X),y=as.integer(Y)))+geom_point(colour='#026937') +
    geom_label_repel(data=df%>% filter(X > quantile(X, percX/100)|
                                         Y > quantile(Y, percY/100)),aes(label = Player),fill="white",color="black")+
    labs(x=df$xAxis,
         y=df$yAxis,
         title = paste0(df$xAxis," and " ,df$yAxis, "   19/20"),
         caption = "Data from FBref.com\n@RobinWilhelmus") +
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = 15))
  
}


scatterMaken90X <- function(df,percX,percY){
  ggplot(df,aes(x=X/`90s`,y=Y))+geom_point(colour='#026937') +
    geom_label_repel(data=df%>% filter(X/`90s` > quantile(X/`90s`, percX/100)|
                                         Y > quantile(Y, percY/100)),aes(label = Player),fill="white",color="black")+
    labs(x=glue::glue("{df$xAxis} P90"),
         y=df$yAxis,
         
         title = paste0(df$xAxis," and " ,df$yAxis, "   19/20"),
         caption = "Data from FBref.com\n@RobinWilhelmus") +
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = 15))
  
}


scatterMaken90 <- function(df,percX,percY){
  ggplot(df,aes(x=X/`90s`,y=Y/`90s`))+geom_point(colour='#026937') +
    geom_label_repel(data=df%>% filter(X/`90s` > quantile(X/`90s`, percX/100)|
                                         Y/`90s` > quantile(Y/`90s`, percY/100)),aes(label = Player),fill="white",color="black")+
    labs(x=glue::glue("{df$xAxis} P90"),
         y=glue::glue("{df$yAxis} P90"),
         title = paste0(df$xAxis," and " ,df$yAxis, "   19/20"),
         caption = "Data from FBref.com\n@RobinWilhelmus") +
    theme_bw()+
    theme(plot.title = element_text(hjust=0.5, size = 15))
  
}

scatterMaken90Y <- function(df,percX,percY){
  ggplot(df,aes(x=X,y=Y/`90s`))+geom_point(colour='#026937') +
    geom_label_repel(data=df%>% filter(X > quantile(X, percX/100)|
                                         Y/`90s` > quantile(Y/`90s`, percY/100)),aes(label = Player),fill="white",color="black")+
    labs(x=df$xAxis,
         y=glue::glue("{df$yAxis} P90"),
         title = paste0(df$xAxis," and " ,df$yAxis, "   19/20"),
         caption = "Data from FBref.com\n@RobinWilhelmus")+
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5, size = 15))
  
}

scatterMakenDark <- function(df,percX,percY){
  ggplot(df,aes(x=X,y=Y))+geom_point(colour='red', alpha=0.5) +
    geom_label_repel(data=df%>% filter(X > quantile(X, percX/100)|
                                         Y > quantile(Y, percY/100)),aes(label = Player))+
    labs(x=df$xAxis,
         y=df$yAxis,
         title = paste0(df$xAxis," and " ,df$yAxis, "   19/20"),
         caption = "Data from FBref.com\n@RobinWilhelmus") +
    dark_theme_gray()+
    theme(plot.title = element_text(hjust=0.5, size = 15))
  
}


scatterMaken90XDark <- function(df,percX,percY){
  ggplot(df,aes(x=X/`90s`,y=Y))+geom_point(colour='red', alpha=0.5) +
    geom_label_repel(data=df%>% filter(X/`90s` > quantile(X/`90s`, percX/100)|
                                         Y > quantile(Y, percY/100)),aes(label = Player))+
    labs(x=glue::glue("{df$xAxis} P90"),
         y=df$yAxis,
         
         title = paste0(df$xAxis," and " ,df$yAxis, "   19/20"),
         caption = "Data from FBref.com\n@RobinWilhelmus") +
    dark_theme_gray()+
    theme(plot.title = element_text(hjust=0.5, size = 15))
  
}


scatterMaken90Dark <- function(df,percX,percY){
  ggplot(df,aes(x=X/`90s`,y=Y/`90s`))+geom_point(colour='red', alpha=0.5) +
    geom_label_repel(data=df%>% filter(X/`90s` > quantile(X/`90s`, percX/100)|
                                         Y/`90s` > quantile(Y/`90s`, percY/100)),aes(label = Player))+
    labs(x=glue::glue("{df$xAxis} P90"),
         y=glue::glue("{df$yAxis} P90"),
         title = paste0(df$xAxis," and " ,df$yAxis, "   19/20"),
         caption = "Data from FBref.com\n@RobinWilhelmus") +
    dark_theme_gray()+
    theme(plot.title = element_text(hjust=0.5, size = 15))
  
}

scatterMaken90YDark <- function(df,percX,percY){
  ggplot(df,aes(x=X,y=Y/`90s`))+geom_point(colour='red', alpha=0.5) +
    geom_label_repel(data=df%>% filter(X > quantile(X, percX/100)|
                                         (Y/`90s`) > quantile(Y/`90s`, percY/100)),aes(label = Player))+
    labs(x=df$xAxis,
         y=glue::glue("{df$yAxis} P90"),
         title = paste0(df$xAxis," and " ,df$yAxis, "   19/20"),
         caption = "Data from FBref.com\n@RobinWilhelmus")+
    dark_theme_gray() +
    theme(plot.title = element_text(hjust=0.5, size = 15))
  
}



