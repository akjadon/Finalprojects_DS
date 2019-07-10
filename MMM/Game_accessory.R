#####################################################################################################
############################# Model on Game Accessory Dataset#######################################
#####################################################################################################

Cross_game<- rep(0,5)

# Building the Baisc Linear regression Model

Linear_model <- Game_final

Linear_model <- Linear_model[,-c(1:4,10:12,28:39)]

Linear_model <- scale(Linear_model)

Linear_model <-data.frame(Linear_model)

model_1 <- lm(gmv~.,Linear_model)


################################################################################################

# Summary of Linear Model 
summary(model_1)


library(car)
library(MASS)

model_2 <- stepAIC(model_1,direction = "both")

summary(model_2) 

vif(model_2)


model_3 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                Sponsorship + Content.Marketing + SEM + inc_LP_MA1 + inc_LP_MA2 + 
                inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, data = Linear_model)


summary(model_3) 

vif(model_3)

model_4 <-  lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                 Sponsorship + SEM + inc_LP_MA1 + inc_LP_MA2 + 
                 inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, data = Linear_model)


summary(model_4) 

vif(model_4)


model_5 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                Sponsorship + SEM + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA2 + inc_PO_MA3, data = Linear_model)



summary(model_5) 

vif(model_5)


model_6 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                Sponsorship + SEM + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA2, data = Linear_model)


summary(model_6) 

vif(model_6)


model_7 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                Sponsorship + SEM + inc_LP_MA1 +  inc_PO_MA2, data = Linear_model)



summary(model_7)

vif(model_7)
model_8 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + TV + 
                SEM + inc_LP_MA1 +  inc_PO_MA2, data = Linear_model)


summary(model_8)

vif(model_8)

model_9 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS +  
                SEM + inc_LP_MA1 +  inc_PO_MA2, data = Linear_model)

summary(model_9)


model_10 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS +  
                 SEM +   inc_PO_MA2, data = Linear_model)


summary(model_10)


model_11 <- lm(formula = gmv ~  per_order + NPS +  
                 SEM +   inc_PO_MA2, data = Linear_model)


summary(model_11)


model_12 <- lm(formula = gmv ~   NPS +  
                 SEM +   inc_PO_MA2, data = Linear_model)


summary(model_12)


##Final Model 
Linear_Final_model_game <- model_12

# Adj R square  = 0.3606  with 3 variables

temp_crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ NPS + SEM + inc_PO_MA2),m = 10)
Cross_game[1] <- attr(temp_crossval, "ms")

##############################################################################################################
# Elasticity Analysis

train <- Linear_model


grlm <- Linear_Final_model_game


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Linear Model") +xlab("Variables")

#####################################################################################################################
#####################################################################################################################
########################################## Multiplicative Model #####################################################

Game_multi <- Game_final
Game_multi <- Game_multi[,-c(1:4,10:12,13,22:39)]



# Treatment

Game_multi$Content.Marketing[which(Game_multi$Content.Marketing==0)] <- 0.01

Game_multi$per_order[which(Game_multi$per_order==0)] <- 0.01

Game_multi <- log(Game_multi)

################################################
#################################################

## Build the First model ##

multi_model <- lm(gmv~.,Game_multi)

summary(multi_model)

library(car)
library(MASS)

multi_model_2 <- stepAIC(multi_model,direction = "both")

summary(multi_model_2)

vif(multi_model_2)


model_3 <- lm(formula = gmv ~ SLA + per_order + NPS + TV + Sponsorship + 
                Content.Marketing + Affiliates, data = Game_multi)



summary(model_3) 

vif(model_3)



model_4 <- lm(formula = gmv ~ SLA + per_order + NPS + Sponsorship + 
                Content.Marketing + Affiliates, data = Game_multi)
summary(model_4)

vif(model_4)


model_5 <- lm(formula = gmv ~ SLA + per_order + Sponsorship + 
                Content.Marketing + Affiliates, data = Game_multi)

summary(model_5)

vif(model_5)


model_6 <- lm(formula = gmv ~ SLA + per_order + Sponsorship + 
                Affiliates, data = Game_multi)

summary(model_6)

vif(model_6)


model_7 <- lm(formula = gmv ~ per_order + Sponsorship + 
                Affiliates, data = Game_multi)

summary(model_7)

model_8 <- lm(formula = gmv ~ per_order + 
                Affiliates, data = Game_multi)


summary(model_8)


### 
# Multiplicative : Final Model Model-9

multi_final_model_game <- model_8

#Adj R2 = 0.5844  with 2 significant variables 

temp_crossval <- cv.lm(data = Game_multi, form.lm = formula(gmv ~ per_order + Affiliates),m = 10)
Cross_game[2] <- attr(temp_crossval, "ms")

#############################################################################

# Elasticity Analysis# 

train <- Game_multi


grlm <- multi_final_model_game


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
View(elasticity.outputs)


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multiplicative Model") +xlab("Variables")


############################################## Koyck models #################################################

Game_koyck<- Game_final[,-c(1:4,10:12,28:39)]


# gmv 1 lag
Game_koyck <- slide(Game_koyck, Var = "gmv",slideBy = -1)


Game_koyck <- na.omit(Game_koyck)

Game_koyck <- scale(Game_koyck)

Game_koyck <- data.frame(Game_koyck)

# Build first model
Koy_model <- lm(gmv~.,Game_koyck)

summary(Koy_model)

model_2 <- stepAIC(Koy_model,direction = "both")

summary(model_2)  
vif(model_2)

model_3 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                Digital + Sponsorship + Content.Marketing + inc_LP_MA2 + 
                inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + gmv.1, 
              data = Game_koyck)


summary(model_3)

vif(model_3)


model_4 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                Digital + Sponsorship + Content.Marketing + inc_LP_MA2 + 
                inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + gmv.1, 
              data = Game_koyck)


summary(model_4)

vif(model_4)


model_5 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                Digital + Sponsorship + inc_LP_MA2 + 
                inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + gmv.1, 
              data = Game_koyck)

summary(model_5)

vif(model_5)


model_6 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                Digital + Sponsorship + 
                inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + gmv.1, 
              data = Game_koyck)


summary(model_6)

vif(model_6)


model_7 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                Digital + Sponsorship + 
                inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
              data = Game_koyck)

summary(model_7)

vif(model_7)


model_8 <- lm(formula = gmv ~ SLA + Procurement_SLA + per_order + NPS + 
                Digital + 
                inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
              data = Game_koyck)

summary(model_8)

vif(model_8)


model_9 <- lm(formula = gmv ~ SLA + per_order + NPS + 
                Digital + 
                inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
              data = Game_koyck)

summary(model_9)

vif(model_9)


model_10 <- lm(formula = gmv ~ SLA + NPS + 
                 Digital + 
                 inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
               data = Game_koyck)

summary(model_10)


model_11 <- lm(formula = gmv ~ SLA + NPS + 
                 inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
               data = Game_koyck)

summary(model_11)

model_12 <- lm(formula = gmv ~ NPS + 
                 inc_LP_MA3 + inc_PO_MA1 + gmv.1, 
               data = Game_koyck)

summary(model_12)

##Final Model 
koyck_Final_Model_game <- model_12 

# Adj R square  = 0.4061 with 4 significant variable 

temp_crossval <- cv.lm(data = Game_koyck, form.lm = formula(gmv ~ NPS + inc_LP_MA3 + inc_PO_MA1 + gmv.1),m = 10)
Cross_game[3] <- attr(temp_crossval, "ms")

###############################################################################################

# Elasticity Analysis

train <- Game_koyck


grlm <- koyck_Final_Model_game


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Koyck Model") +xlab("Variables")



############################################## Distributed lag models #################################################

Dis_Model <- Game_final[,-c(1:4,10:12, 28:30)]

Dis_model_1 <- slide(Dis_Model, Var = "gmv",slideBy = -1)

Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -2)

Dis_model_1 <- slide(Dis_model_1, Var = "gmv",slideBy = -3)

Dis_model <- na.omit(Dis_model_1)

Dis_model <- scale(Dis_model)
Dis_model <- data.frame(Dis_model)

# Lag distribution model: 


dist_model <- lm(gmv~.,Dis_model)

summary(dist_model)


model_2 <- stepAIC(dist_model,direction = "both")

summary(model_2)

vif(model_2)



model_3 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                Sponsorship + Content.Marketing + Online_marketing + Affiliates + 
                SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                inc_PO_MA3 + promotional_offer.1 + promotional_offer.2 + 
                promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                gmv.2 + gmv.3, data = Dis_model)

summary(model_3)


vif(model_3)



model_4 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                Sponsorship + Content.Marketing + Online_marketing + Affiliates + 
                SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                promotional_offer.1 + promotional_offer.2 + 
                promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                gmv.2 + gmv.3, data = Dis_model)


summary(model_4)


vif(model_4)


model_5 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                Sponsorship + Content.Marketing + Online_marketing + Affiliates + 
                SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                promotional_offer.2 + 
                promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                gmv.2 + gmv.3, data = Dis_model)

summary(model_5)


vif(model_5)


model_6 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                Sponsorship + Content.Marketing + Affiliates + 
                SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                promotional_offer.2 + 
                promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                gmv.2 + gmv.3, data = Dis_model)


summary(model_6)


vif(model_6)


model_7 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                Sponsorship + Content.Marketing + Affiliates + 
                SEM + inc_LP_MA1 + inc_LP_MA3 + inc_PO_MA1 + 
                promotional_offer.2 + 
                promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                gmv.2 + gmv.3, data = Dis_model)

summary(model_7)


vif(model_7)


model_8 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                Sponsorship + Affiliates + 
                SEM + inc_LP_MA1 + inc_LP_MA3 + inc_PO_MA1 + 
                promotional_offer.2 + 
                promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.2 + gmv.1 + 
                gmv.2 + gmv.3, data = Dis_model)
summary(model_8)


vif(model_8)

model_9 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                Sponsorship + Affiliates + 
                SEM + inc_LP_MA1 + inc_LP_MA3 + inc_PO_MA1 + 
                promotional_offer.2 + 
                promotional_offer.3 + NPS.3 + holiday_freq.2 + gmv.1 + 
                gmv.2 + gmv.3, data = Dis_model)

summary(model_9)

vif(model_9)

model_10 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                 Sponsorship + Affiliates + 
                 SEM + inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 promotional_offer.3 + NPS.3 + holiday_freq.2 + gmv.1 + 
                 gmv.2 + gmv.3, data = Dis_model)


summary(model_10)
vif(model_10)


model_11 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                 Affiliates + 
                 SEM + inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 promotional_offer.3 + NPS.3 + holiday_freq.2 + gmv.1 + 
                 gmv.2 + gmv.3, data = Dis_model)

summary(model_11)

vif(model_11)

model_12 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                 Affiliates + 
                 SEM + inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 NPS.3 + holiday_freq.2 + gmv.1 + 
                 gmv.2 + gmv.3, data = Dis_model)

summary(model_12)

vif(model_12)

model_13 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                 Affiliates + 
                 SEM + inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 NPS.3 + holiday_freq.2 + gmv.1 + 
                 gmv.3, data = Dis_model)

summary(model_13)

vif(model_13)


model_14 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + holiday_freq + 
                 Affiliates + 
                 SEM + inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 NPS.3 + holiday_freq.2 + gmv.1
               , data = Dis_model)

summary(model_14)

model_15 <- lm(formula = gmv ~ promotional_offer + holiday_freq + 
                 Affiliates + 
                 SEM + inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 NPS.3 + holiday_freq.2 + gmv.1
               , data = Dis_model)

summary(model_15)

model_16 <- lm(formula = gmv ~ promotional_offer + holiday_freq + 
                 SEM + inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 NPS.3 + holiday_freq.2 + gmv.1
               , data = Dis_model)
summary(model_16)


model_17 <- lm(formula = gmv ~ promotional_offer + holiday_freq + 
                 inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 NPS.3 + holiday_freq.2 + gmv.1
               , data = Dis_model)
summary(model_17)


model_18 <- lm(formula = gmv ~ promotional_offer + holiday_freq + 
                 inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 NPS.3 + gmv.1
               , data = Dis_model)
summary(model_18)

model_19 <- lm(formula = gmv ~ holiday_freq + 
                 inc_LP_MA1 + inc_PO_MA1 + 
                 promotional_offer.2 + 
                 NPS.3 + gmv.1
               , data = Dis_model)
summary(model_19)

##Final Model 

Dist_Final_Model_game <- model_19

# Adj R square  = 0.5335 with 6 significant variables

temp_crossval <- cv.lm(data = Dis_model, form.lm = formula(gmv ~ holiday_freq + inc_LP_MA1 + inc_PO_MA1 + promotional_offer.2 + NPS.3 + gmv.1),m = 10)
Cross_game[4] <- attr(temp_crossval, "ms")

########################################################################################

# Elasticity Analysis
train <- Dis_model

grlm <- Dist_Final_Model_game


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Distributed Lag Model") +xlab("Variables")


################################################################################


# Multiplicative + distributed model: 

Multi_dist <- Game_final[-c(1:4,10:13,22:30,37:39)] # Not considered moving averages.

# gmv

Multi_Dis_model_1 <- slide(Multi_dist, Var = "gmv",slideBy = -1)

Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -2)

Multi_Dis_model_1 <- slide(Multi_Dis_model_1, Var = "gmv",slideBy = -3)

Multi_dist <- na.omit(Multi_Dis_model_1)



colnames(Multi_dist)[14:22]<- c("promotional_offer_1","promotional_offer_2","promotional_offer_3","NPS_1",
                                "NPS_2","NPS_3", "gmv_1", "gmv_2", "gmv_3")
Multi_dist$Content.Marketing[which(Multi_dist$Content.Marketing==0)] <-1
Multi_dist$per_order[which(Multi_dist$per_order==0)] <-0.01


########################################################

Multi_dist <- log(Multi_dist)

model_1 <- lm(gmv~., Multi_dist)

summary(model_1)

model_2 <- stepAIC(model_1,direction = "both")

summary(model_2) 

vif(model_2)


model_3 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                TV + Digital + Sponsorship + Content.Marketing + Online_marketing + 
                SEM + promotional_offer_1 + promotional_offer_2 + 
                NPS_2 + gmv_1 + gmv_2 + gmv_3, data = Multi_dist)

summary(model_3)

vif(model_3)


model_4 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                TV + Digital + Sponsorship + Content.Marketing + 
                SEM + promotional_offer_1 + promotional_offer_2 + 
                NPS_2 + gmv_1 + gmv_2 + gmv_3, data = Multi_dist)

summary(model_4)

vif(model_4)


model_5 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                TV + Digital + Sponsorship + Content.Marketing + 
                promotional_offer_1 + promotional_offer_2 + 
                NPS_2 + gmv_1 + gmv_2 + gmv_3, data = Multi_dist)

summary(model_5)

vif(model_5)



model_6 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                Digital + Sponsorship + Content.Marketing + 
                promotional_offer_1 + promotional_offer_2 + 
                NPS_2 + gmv_1 + gmv_2 + gmv_3, data = Multi_dist)

summary(model_6)

vif(model_6)


model_7 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                Digital + Sponsorship + Content.Marketing + 
                promotional_offer_1 + promotional_offer_2 + 
                NPS_2 + gmv_1 + gmv_3, data = Multi_dist)

summary(model_7)

vif(model_7)


model_8 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                Digital + Sponsorship + Content.Marketing + 
                promotional_offer_1 + promotional_offer_2 + 
                NPS_2 + gmv_1, data = Multi_dist)

summary(model_8)

vif(model_8)


model_9 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + 
                Digital + Sponsorship + Content.Marketing + 
                promotional_offer_1 + promotional_offer_2 + 
                NPS_2 + gmv_1, data = Multi_dist)

summary(model_9)

vif(model_9)

model_10 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA + 
                 Digital + Sponsorship + Content.Marketing + 
                 promotional_offer_2 + 
                 NPS_2 + gmv_1, data = Multi_dist)
summary(model_10)


model_11 <- lm(formula = gmv ~ Procurement_SLA + 
                 Digital + Sponsorship + Content.Marketing + 
                 promotional_offer_2 + 
                 NPS_2 + gmv_1, data = Multi_dist)
summary(model_11)


model_12 <- lm(formula = gmv ~ Procurement_SLA + 
                 Digital + Sponsorship + 
                 promotional_offer_2 + 
                 NPS_2 + gmv_1, data = Multi_dist)

summary(model_12)

model_13 <- lm(formula = gmv ~ Digital + Sponsorship + 
                 promotional_offer_2 + 
                 NPS_2 + gmv_1, data = Multi_dist)

summary(model_13)

# Adjusted R-squared:  0.6379  with 5 significant variables

Multi_Dist_Final_Model_game <- model_13

temp_crossval <- cv.lm(data = Multi_dist, form.lm = formula(gmv ~ Digital + Sponsorship + promotional_offer_2 + NPS_2 + gmv_1),m = 10)
                                                              
Cross_game[5] <- attr(temp_crossval, "ms")

######################################################################################
# Elasticity Analysis

train <- Multi_dist

grlm <- Multi_Dist_Final_Model_game


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")



ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multi. & DL model") +xlab("Variables")


#############################################################################################################
#############################################################################################################
