setwd("/media/data/data_project/citybike")

library(plyr)
library(dplyr)
library(geosphere)
library(data.table)
library(lubridate)
library(reshape2)
library(gbm)
library(caret)

#######################################################################################################
## load data and create new variables
#########################################################################################################
dir.create(file.path('./data'), showWarnings = FALSE)
db <- list.files('raws')
for(i in 1:length(db)){
  n <- db[i]
  df <- fread(n)
  n <- strsplit(n, '[.]')[[1]][1]
  
  setnames(df, c('duration', 'starttime', 'stoptime', 'startid', 'startname', 'startlat', 'startlon', 'endid', 'endname', 'endlat', 'endlon', 'bikeid', 'usertype', 'birth', 'gender'))
  
  #get a matrix of the distance between stations
  if(i == 1){
    station <- df %>% distinct(startid) %>% dplyr::select(as.numeric(startlon), as.numeric(startlat))
    station <- as.matrix(station)
    station <- apply(station, 2, as.numeric)
    dist <- distm(station,  station)
    rownames(dist) <- colnames(dist) <- unique(df$startid)
    dist <- as.data.frame(dist)
    dist$startid <- rownames(dist)
    mdist <- melt(dist, id = 'startid', variable.name = 'endid', value.name = 'dist')
    mdist$endid <- as.character(mdist$endid)
  }
  #get the distance from the station matrix and match it to the travels
  df <- as.data.frame(df)
  df <- left_join(df, mdist)
  
  df$speed<- df$dist/as.numeric(df$duration)  
  
  #extract year month day hour min sec from date    
  dates <- as.data.frame(do.call(rbind,strsplit(as.character(df$starttime), ' ')))
  names(dates) <- c('dates', 'times')
  d <- do.call(rbind,strsplit(as.character(dates$dates), '-|/'))
  
  if(grepl('/',dates$dates[1])){
    #the last data have a different format for the date so need to transform them
    d <- d[,c(3,1,2)]
    df$starttime <-  mdy_hms(df$starttime)
    df$stoptime <-  mdy_hms(df$stoptime)
  }else{
    df$starttime <- ymd_hms(df$starttime)
    df$stoptime <- ymd_hms(df$stoptime)    
  }
  
  ti <- do.call(rbind,strsplit(as.character(dates$times), ':'))
  cut_date <- data.frame(d,ti)
  cut_date <- sapply(cut_date, function(x) as.numeric(as.character(x)))
  
  colnames(cut_date) <- c('year', 'month', 'day', 'hour', 'min', 'sec')
  
  df$dayofweek <- format(as.Date(as.character(dates[,1])), '%a')
  bike <- data.frame(df, cut_date)
  rm(df); rm(dates); rm(d); rm(ti); rm(cut_date) 
  
  df <- data.frame(bikeid = bike$bikeid, stoptime = bike$stoptime, starttime = bike$starttime)
  b <- df %>% group_by(bikeid) %>% dplyr::mutate(stay = difftime(stoptime,lead(starttime)))
  b$bikeid <- as.character(b$bikeid)
  bike <- left_join(bike, b, by = c('bikeid', 'stoptime'))
  bike$idx1 <- substring(n, 1,7)
  bike$idx2 <- 1:dim(bike)[1]
  bike$toofast <- ifelse(bike$speed > 6, TRUE, FALSE)
  bike$weekend <- ifelse(bike$dayofweek %in% c('Sat', 'Sun'), TRUE, FALSE)
  bike$rush <- ifelse(bike$hour %in% c(7,8,9, 17, 18, 19) & bike$weekend == FALSE, TRUE, FALSE)
  bike$night <- ifelse(bike$hour %in% c(21:23,0:6), TRUE, FALSE)
  
  save(bike, file = paste('data/',n, sep =''))
}

########################################################################################################
#create train and test dataset
##########################################################################################################
dir.create(file.path('train'), showWarnings = FALSE)
dir.create(file.path('test'), showWarnings = FALSE)

df <- list.files('data')
create_train.f <- function(x){
  load(paste0('data/',x))
  idxtrain <- createDataPartition(bike$endid, list = FALSE)
  train <- bike[idxtrain,]
  test <- bike[-idxtrain,]
  save(train, file = paste('train/',x, sep =''))
  save(test, file = paste('test/',x, sep =''))
}
lapply(df, create_train.f)

create_df.f <- function(nm){
  if(nm == 'train'){
    train <- lapply(list.files(nm), function(x) {load(paste0(nm, '/',x)); return(train)})
    train <- do.call(rbind, train)
    save(train, file = paste0(nm, '/', nm))
  }else{
    test <- lapply(list.files(nm), function(x) {load(paste0(nm, '/',x)); return(test)})
    test <- do.call(rbind, test)
    save(test, file = paste0(nm, '/', nm))
  }
}
create_df.f('train')
create_df.f('test')

################################################################################################################
## Transform bike data in station data 
################################################################################################################
library(doParallel)
change_days.f <- function(t){
  days = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
  nr <- t$stay %/% 1440
  extra <- ifelse(t$stay%%1440 != 0, 1, 0)
  y <- as.data.frame(matrix(t, nrow = nr+extra, ncol = 10, byrow = TRUE))
  names(y) = names(t)
  y$stay[1:nr] <- 1440
  if(extra == 1){y$stay[(nr+extra)] <- t$stay%%60}
  y$day <- unlist(y$day) + 0:(dim(y)[1]-1)
  nday <- which(days == t$dayofweek)+0:(dim(y)[1]-1)
  while(any(nday > 7)){
    nday[which(nday > 7)] <- nday[which(nday > 7)] - 7
  }
  y$dayofweek <- days[nday]
  y$month[which(y$day > days_in_month(t$month))] <- unlist(y$month[which(y$day > days_in_month(t$month))]) +1
  return(y)
}

change_hour.f <- function(x){
  days = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
  x$stay <- as.numeric(x$stay)
  nr <- x$stay%/%60
  extra <- ifelse(x$stay%%60 != 0, 1, 0)
  y <- as.data.frame(matrix(x, nrow = nr+extra, ncol = 10, byrow = TRUE))
  names(y) = names(x)
  y$stay[1:nr] <- 60
  if(extra == 1){y$stay[(nr+extra)] <- x$stay%%60}
  y$hour <- unlist(y$hour) + 0:(dim(y)[1]-1)
  y$day[which(y$hour > 23)] <- unlist(y$day[which(y$hour > 23)]) + 1
  y$dayofweek[which(y$hour > 23)] <- days[which(days == x$dayofweek)+1]
  y$hour[which(y$hour > 23)] <- y$hour[which(y$hour > 23)] - 24
  return(y)
}  

is_in.f <- function(instat, tm, n){
  s <- seq(0,60-tm,tm)
  names <- paste(s,'+', tm, sep = '')  
  instat$stay <- round(instat$stay)
  #change if time bef/ginning in the station +stay > 60 min (so need to change min and add a line....)
  toolong <- instat[which((instat$stay + instat$min )> 60),]
  instat_cut <- instat[-which((instat$stay + instat$min )> 60),]
  toolong2 <- toolong
  toolong$stay <- with(toolong,  60 - toolong$min)
  toolong2$stay <- toolong2$stay - toolong$stay
  toolong2$min <- 0
  instat_cut <- rbind(instat_cut, toolong, toolong2)
  
  #more than a day
  toolong <- instat_cut[which(instat_cut$stay > 1440),]
  instat_cut <- instat_cut[-which(instat_cut$stay > 1440),]
  registerDoParallel(cores=6)
  tl <- foreach(i = 1:nrow(toolong), .combine = rbind) %dopar%{
    change_days.f(toolong[i,])
  }
  instat_cut$stoptime <- as.character(instat_cut$stoptime)
  instat_cut <- rbind(instat_cut, tl)
  
  toolong <- instat_cut[which(instat_cut$stay > 60),]
  instat_cut <- instat_cut[-which(instat_cut$stay > 60),]
  #that's taking forever need to parallelize or something....
  registerDoParallel(cores=4)
  system.time(
    tl <- foreach(i = 1:nrow(toolong), .combine = rbind) %dopar%{
      change_hour.f(toolong[i,])
    })
  #tl <- alply(toolong,1, change_hour.f)     
  #tl <- do.call(rbind, tl)
  instat_cut <- rbind(instat_cut, tl)  
  
  is_in <- matrix(0, dim(instat_cut)[1], length(names))
  instat_cut$stay <- as.numeric(instat_cut$stay)
  instat_cut$min <- as.numeric(instat_cut$min)
  instat_cut$min[which(is.na(instat_cut$min))] <- 0
  nyes <- round(instat_cut$stay/tm)  
  begin <- round(instat_cut$min/tm)
  begin[which(begin == 6)] = 5
  e <- begin + nyes
  f <- begin+1
  change <- which(f>e)
  e[change] <- 0
  f[change] <- 0
  for(i in 1:dim(is_in)[1]){        
    is_in[i, (f[i]):(e[i])] = 1
  }
  #is_in <- mapvalues(is_in, NA, 'no')
  colnames(is_in) <- names
  instat_cut <- data.frame(instat_cut, is_in)
  
  return(instat_cut)  
}

to_station.f <- function(train, n){
  train$stay <- as.numeric(train$stay)
  train  <- train[-which(train$stay < 0),]
  instat <- train %>% select(endid, stoptime, bikeid, dayofweek, stay)
  instat <- instat %>% mutate(year = year(stoptime), month = month(stoptime), day = day(stoptime), hour = hour(stoptime), min = minute(stoptime), dayofweek = format(train$stoptime, '%a'))
  instat <- instat[!is.na(instat$stay),]
  
  station <- is_in.f(instat, 10, 3)
  
  station$weekend = ifelse(station$dayofweek %in% c('Sat', 'Sun'), TRUE, FALSE)
  station$rush = ifelse(station$hour %in% c(7,8,9, 17, 18, 19) & station$weekend == FALSE, TRUE, FALSE)
  station$night = ifelse(station$hour %in% c(21:23,0:6), TRUE, FALSE)
  station$dayofweek <- as.character(station$dayofweek)
  st <- apply(station[,c(1,3, 7,8,9)], 2, as.numeric)
  station <- data.frame(station[,c(4,5,10:19)], st)
  mst <- melt(station, id = c(1:3, 10:17), variable.name = 'min_block')  
  
  #gmst <- mst %>% group_by(endid, month,hour, dayofweek, rush, night, weekend, min_block) %>% dplyr::summarise(count = n(), nbike = sum(value)) 
  
  
  dmst <- mst %>% group_by(endid, month,day, dayofweek, hour, rush, night, weekend, min_block) %>% dplyr::summarise(count = n(), nbike = sum(value)) 
  gmst <- dmst %>% group_by(endid, month,hour,  rush, night, weekend, min_block, dayofweek) %>% dplyr::summarise(nday = n(), avgbike = round(mean(nbike)))
  
  #this is good but missing hours.... that will take forever so do it without that first....
  # t <- gmst %>% filter(month == 1 & endid == 72 & dayofweek == 'Sun')
  #     get_missing_hours.f <- function(t){
  #       hours <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
  #       missing <- hours[!(hours %in% t$hour)]
  #       nr <- length(missing)*6
  #       if(dim(t)[1] >= nr){
  #         newh <- t[1:nr,]
  #       }else{
  #         nrm <- dim(t)[1] - nr 
  #         newh <- rbind(t, t[1:nrm,])
  #       }
  #       newh$hour <- rep(missing, each = 6)
  #       out <- rbind(t, newh)
  #       return(out)
  #     }
  
  gmst$endid <- as.factor(gmst$endid)
  gmst$dayofweek <- as.factor(gmst$dayofweek)
  t <- apply(gmst[,4:6], 2, as.numeric)
  gmst[,4:6] <- t
  
  return(gmst)
}

load('train/train')
gmst <- to_station.f(train, 6)
save(gmst, file = 'train/train_station')

load('test/test')
gmst_test <- to_station.f(test, 6)
save(gmst_test, file = 'test/test_station')
#############################################################################################################
## transform station data by binning the number of bike per station per time
##############################################################################################################
library(Hmisc)
create_binary_y.f <- function(df){
  gmstbin <- df %>% mutate(value = ifelse(avgbike >=1, 1,0))
  gmstbin <- gmstbin[,-9]
  return(gmstbin)
} 

create_block.f <- function(df, v1=1, v2=2, v3=5){#1,3,10
  gmstblock <- df %>% mutate(value = ifelse(avgbike < v1, 0, ifelse(avgbike < v2, 1, ifelse(avgbike < v3, 2,3))))
  gmstblock <- gmstblock[,-c(9,10)]
  return(gmstblock)
}

load('train/train_station')
gmstbin <- create_binary_y.f(gmst)
save(gmstbin, file = 'train/train_bin')

table(cut2(gmst$avgbike, g=4))
gmstblock <- create_block.f(gmst)
save(gmstblock, file = 'train/train_block')
