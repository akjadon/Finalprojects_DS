
##################################################################################################
###----------------------------------------Ecom Capstone Solution------------------------------###
##################################################################################################

library(dplyr)
library(plyr)
library(lubridate)
library(MASS)
library(car)
library(DataCombine)
library(zoo)
library(glmnet)
library(DAAG)
library(ggplot2)

# Load Dataset in working directory 
ecom <- read.csv("mmm_data.csv")

# Let's modify the name 
colnames(ecom) <- c("FSN_ID","Order_date","Year","Month","Order_id","Order_item_id","gmv","Units","deliverybdays","deliverycdays", "payment_mode","SLA","cust_id","pincode","P_super_category","P_analytic_category","P_sub_category","P_analytic_vertical","MRP","Procurement_SLA")

# structure of dataset 
str(ecom)
summary(ecom)

# data sanity Check : Data should be from july-2015 to June -2016,Let's remove other data from ecom dataset
ecom <- subset(ecom, !(Month ==5 & Year==2015|Month ==6 & Year==2015|Month ==7 & Year==2016))

## Convert the dataset into weekly levels. 

# Lubridate package 

library(lubridate)

ecom$Order_date <- date(ecom$Order_date)

# create new column week of year 

ecom$week_year <- week(ecom$Order_date)

# Data quality issues 
# Jan 2016 should be week 54, not week 1 etc. 
ecom$week_year<- ifelse(ecom$week_year<=26 & ecom$Year==2016,ecom$week_year+53,ecom$week_year)

# create new column of date 

ecom$date<-as.Date(ecom$Order_date)

summary(ecom$MRP)

# not considering free products
ecom <- subset(ecom,MRP!=0)

sum(is.na(ecom))

# removing rows with NA values
ecom <- na.omit(ecom)

ecom$gmv[which(ecom$gmv==0)] <- 1

# gmv should not be more than MRP*units since we can offer discounts but 
# not charge higher
ecom <- subset(ecom, (MRP*Units)>=gmv)

## Let's divide the dataset into 3 dataframes based on category.


GamingAccessory <-ecom[ecom$P_sub_category=="GamingAccessory" ,] 

CameraAccessory<-ecom[ecom$P_sub_category=="CameraAccessory",]

HomeAudio<-ecom[ecom$P_sub_category=="HomeAudio",]


##############################################################
## KPI FUNCTION FOR ALL THE 3 CATEGORIES ##
##############################################################

Product_data <- function(dataset){
  
  #1. KPI - List price for all the products
  dataset$list_price <- dataset$gmv/dataset$Units
  
  #2. KP - Promotional Offer for all the products
  dataset$promotional_offer <- (dataset$MRP-dataset$list_price)/dataset$MRP
  
  
  #3. Clustering
  ## Creating a new KPI (though not used in the final model)
  ## It divides the products into three categories based on MRP and num units sold - 
  ## mass market, medium market and premium product
  
  dataset$P_analytic_vertical <- factor(dataset$P_analytic_vertical)
  
  cluster<- aggregate(cbind(Units,list_price,MRP)~P_analytic_vertical,dataset,mean)
  
  
  if(nrow(cluster)<=3){
    
    cluster$p_tag <-NA
    
    
    # Assuming premium product:- 
    
    cluster$P_tag[which(cluster$MRP>=mean(cluster$MRP))] <- "Middle_p"
    cluster$P_tag[-which(cluster$MRP>=mean(cluster$MRP))] <- "Mass_p"
    
    cluster <- cluster[,-c(2:4)]
    
    dataset <-merge(dataset,cluster,by="P_analytic_vertical")
    
  } else {
    
    #str(cluster) 
    
    cluster$list_price_1 <- scale(cluster$list_price)
    cluster$MRP_1<- scale(cluster$MRP)
    cluster$Units_1 <- scale(cluster$Units)
    
    #str(cluster)
    
    k1 <- cluster[,-c(2:4)]
    
    
    clust <- kmeans(k1[,-1], centers = 3,iter.max = 50,nstart = 50)
    
    cluster$P_tag <- as.factor(clust$cluster)
    
    cluster <- cluster[,c(1,8)]
    
    # Add extra column in dataset with 
    
    dataset <-merge(dataset,cluster,by=c("P_analytic_vertical"),all.x=TRUE)
    
    
    library("plyr")
    library("dplyr")
    
    k2 <- table(dataset$P_tag)
    
    levels(dataset$P_tag)[which(k2==max(table(dataset$P_tag)))] <- "Mass_p"
    levels(dataset$P_tag)[which(k2==min(table(dataset$P_tag)))] <- "Premium_p"
    levels(dataset$P_tag)[which(k2!=max(table(dataset$P_tag))& k2!=min(table(dataset$P_tag)))] <- "Middle_p"
    
    
  }
  
  #3. Payment model indicator
  dataset$order_pay_ind<-ifelse(dataset$payment_mode=="Prepaid",1,0)
  
  #4.Add (Online_order/Total Order) KPI
  
  # Total Order placed
  total_order <-aggregate(order_pay_ind ~ week_year,data=dataset,FUN=NROW)
  
  # Total online order
  Online_order<-aggregate(order_pay_ind ~ week_year,data=dataset,FUN=sum)
  
  # Merge the both the file 
  merged <-merge(total_order,Online_order,by=c("week_year"),all.x=TRUE)
  
  # Create new column of percentage online
  merged$per_order <- merged$order_pay_ind.y/merged$order_pay_ind.x
  
  # Remove other variables from 
  # Merge with dataset file. 
  
  merged <- merged[,-c(2,3)]
  
  # add "per_order" column in dataset 
  
  dataset<- merge(dataset,merged,by=c("week_year"),all.x=TRUE)
  
  dataset$P_sub_category <- NULL
  
  dataset$P_super_category <- NULL
  
  dataset$P_analytic_category <- NULL
  
  
  #6.NPS score
  nps<-read.csv("nps.csv",h=T)
  nps$Month<-as.character(nps$Month)
  
  
  dataset<-merge(dataset,nps,by=c("Month","Year"),all.x=TRUE)
  
  #5.Holiday_column
  
  holiday_list<-c("2015-07-18","2015-07-19","2015-08-15",
                  "2015-08-16","2015-08-17","2015-08-28",
                  "2015-08-29","2015-08-30","2015-10-15",
                  "2015-10-16","2015-10-17","2015-11-07","2015-11-08","2015-11-09","2015-11-10",
                  "2015-10-11","2015-10-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26",
                  "2015-12-27","2015-12-28","2015-12-29","2015-12-30","2016-01-01","2016-01-02",
                  "2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02",
                  "2016-02-20","2016-02-21","2016-02-14","2016-02-15","2016-03-07","2016-03-08",
                  "2016-03-09","2016-05-25","2016-05-26","2016-05-27")
  
  
  holiday_list <- as.Date(holiday_list)
  
  week_year <- week(holiday_list)
  
  year <- year(holiday_list)
  
  holiday <- data.frame(week_year,year)
  
  holiday$week_year<- ifelse(holiday$week_year<=26 & holiday$year==2016,holiday$week_year+53,holiday$week_year)
  
  holiday$year <-NULL
  holiday$holiday_freq <- 1
  
  holiday <- aggregate( holiday_freq ~ week_year,holiday,sum)
  
  
  products <- as.data.frame.matrix(t(table(dataset$P_tag,dataset$week_year)))
  
  
  products$week_year <- row.names(products)
  
  dataset_1 <- aggregate(gmv~week_year,dataset,sum)
  
  dataset<- aggregate(cbind(list_price,MRP,Units,SLA,promotional_offer,Procurement_SLA,per_order,NPS)~ week_year,data=dataset,FUN = mean)
  
  
  dataset <- merge(dataset,products,by="week_year",all.x=TRUE)
  
  dataset <- merge(dataset,holiday,by="week_year",all.x=TRUE)
  dataset$holiday_freq[is.na(dataset$holiday_freq)] <-0
  
  dataset <- merge(dataset,dataset_1,by="week_year",all.x=TRUE)
  
  ##################################################################################################
  
  return(dataset) 
  
} 
# Datasets

GamingAccessory<- Product_data(GamingAccessory)

HomeAudio <-Product_data(HomeAudio)

CameraAccessory <- Product_data(CameraAccessory)


###############################################################################################
# Create Adstock manually for each of the variables and import it
###############################################################################################

Adstock <- read.csv("Final_adstock.csv")



#################################################################################################

## Combine the Weekly KPI in the dataset 

## Final PRocess for GamingAccessory

GamingAccessory <- merge(GamingAccessory , Adstock,by.x = "week_year")

##################################################################################################

## Final PRocess for HomeAudio

HomeAudio <- merge(HomeAudio,Adstock,by.x = "week_year")

##################################################################################################

## Final PRocess for CameraAccessory

CameraAccessory <- merge(CameraAccessory,Adstock,by.x = "week_year")



## ------------------------Creating moving average variables------------------------------------

advanced_kpi <- function(dataset)
{
  library(dplyr)
  library(zoo)
  
  myfun1 = function(x) rollmean(x, k = 2, fill = NA, align = "right")
  myfun2 = function(x) rollmean(x, k = 3, fill = NA, align = "right")
  myfun3 = function(x) rollmean(x, k = 4, fill = NA, align = "right")
  
  #dataset1<-arrange(dataset1,P_analytic_vertical,Year,week_year)
  
  x=dataset[,c("week_year","list_price","promotional_offer")]
  
  
  x1<-x %>% mutate_each(funs(myfun1),list_price,promotional_offer) %>% data.frame()
  
  x2<-x %>% mutate_each(funs(myfun2),list_price,promotional_offer) %>% data.frame()
  
  x3<-x %>% mutate_each(funs(myfun3),list_price,promotional_offer) %>% data.frame()
  
  
  x1$LP_MA1<-(x1$list_price)
  x1$PO_MA1<-(x1$promotional_offer)
  
  x2$LP_MA2<-(x2$list_price)
  x2$PO_MA2<-(x2$promotional_offer)
  
  x3$LP_MA3<-(x3$list_price)
  x3$PO_MA3<-(x3$promotional_offer)
  
  x4=cbind(x1[,-c(2:3)],x2[,-c(1:3)],x3[,-c(1:3)])
  
  
  
  dataset<-merge(dataset,x4,by="week_year")
  
  
  
  dataset$inc_LP_MA1<-(dataset$list_price - dataset$LP_MA1)/dataset$LP_MA1
  dataset$inc_LP_MA2<-(dataset$list_price - dataset$LP_MA2)/dataset$LP_MA2
  dataset$inc_LP_MA3<-(dataset$list_price - dataset$LP_MA3)/dataset$LP_MA3
  
  dataset$inc_PO_MA1<-(dataset$promotional_offer - dataset$PO_MA1)/dataset$PO_MA1
  dataset$inc_PO_MA2<-(dataset$promotional_offer - dataset$PO_MA2)/dataset$PO_MA2
  dataset$inc_PO_MA3<-(dataset$promotional_offer - dataset$PO_MA3)/dataset$PO_MA3
  
  #Deleting some columns
  
  dataset$LP_MA1<-NULL
  dataset$LP_MA2<-NULL
  dataset$LP_MA3<-NULL
  
  dataset$PO_MA1<-NULL
  dataset$PO_MA2<-NULL
  dataset$PO_MA3<-NULL
  
  names(dataset)[22:27]<-c("inc_LP_MA1","inc_LP_MA2","inc_LP_MA3","inc_PO_MA1","inc_PO_MA2",
                           "inc_PO_MA3")
  
  
  
  #------1) Lag of List price by 1 week,2 week, 3 week
  #------2) Lag of discount(promo_off) by 1 week,2 week, 3 week
  #------3) Incremental Lag of List price & promotions/discounts by 1 week,2 week, 3 week
  
  #-----------------Lag the data after aggregating by week----#
  
  #8.	Lag List price (different period lags)
  
  library(DataCombine)
  
  #List of list price by 1,2,3 dates (Date values are ordered)
  #Previous List price
  data_dum <- slide(dataset, Var = "list_price",slideBy = -1)
  
  data_dum <- slide(data_dum, Var = "list_price",slideBy = -2)
  
  data_dum <- slide(data_dum, Var = "list_price", slideBy = -3)
  
  #9.lag the promotion variables
  
  data_dum <- slide(data_dum, Var = "promotional_offer", slideBy = -1)
  
  data_dum <- slide(data_dum, Var = "promotional_offer", slideBy = -2)
  
  data_dum <- slide(data_dum, Var = "promotional_offer", slideBy = -3)
  
  data_dum <- slide(data_dum, Var = "NPS", slideBy = -1)
  
  data_dum <- slide(data_dum, Var = "NPS", slideBy = -2)
  
  data_dum <- slide(data_dum, Var = "NPS", slideBy = -3)
  
  
  data_dum <- slide(data_dum, Var = "holiday_freq", slideBy = -1)
  
  data_dum <- slide(data_dum, Var = "holiday_freq", slideBy = -2)
  
  data_dum <- slide(data_dum, Var = "holiday_freq", slideBy = -3)
  
  
  dataset <- na.omit(data_dum) 
  
  
  return(dataset)
}

Game_final <- advanced_kpi(GamingAccessory)
Home_final <- advanced_kpi(HomeAudio)
Camera_final <- advanced_kpi(CameraAccessory)


######################## EDA ###########################

EDA <- function(dataset,name){

  
  # Ad_Stock
  plot1 <- ggplot(dataset,aes(TV,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "TV AdStock", y = "GMV")
  plot1
  
  
  plot2 <- ggplot(dataset,aes(Digital,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Digital AdStock", y = "GMV")
  plot2
  
  plot3 <- ggplot(dataset,aes(Sponsorship,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Sponsorship AdStock", y = "GMV")
  plot3
  
  plot4 <- ggplot(dataset,aes(Content.Marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Content Marketing AdStock", y = "GMV")
  plot4
  
  plot5 <- ggplot(dataset,aes(Online_marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Online Marketing AdStock", y = "GMV")
  plot5
  
  plot6 <- ggplot(dataset,aes(Affiliates,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Affiliates AdStock", y = "GMV")
  plot6
  
  plot7 <- ggplot(dataset,aes(SEM,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "SEM AdStock", y = "GMV")
  plot7

  plot8 <- ggplot(dataset,aes(dataset$week_year,dataset$gmv, fill = as.factor(ifelse(dataset$holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Holiday_flag", x = "Week", y = "GMV") + ggtitle(name)
  plot8
  
  #return(list(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8))
  
}

# Game_final

EDA(Game_final,"Gaming Accessory")

#Camera_final

EDA(Camera_final,"Camera Accessory")


# Home_final
EDA(Home_final,"Home Audio")


