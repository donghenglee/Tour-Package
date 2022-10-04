library(dplyr)
library(ggplot2)
library(corrplot)

setwd("C:/Users/DHLee/OneDrive/Desktop/Y2S2/PS0002 Intro To Data Science & Artificial Intelligence/Projects")
tour_raw <-read.csv("tour_package.csv", header = TRUE, sep=",")
str(tour_raw)

tour = tour_raw %>% select(-ï..CustomerID) %>% na.omit() 
tour[tour=="Fe Male"] ="Female"

#change categorical variables to numeric values
tour$ProdTaken = as.factor(tour$ProdTaken)
tour$CityTier = as.factor(tour$CityTier)
tour$TypeofContact = as.factor(tour$TypeofContact)
tour$Occupation = as.factor(tour$Occupation)
tour$Gender = as.factor(tour$Gender)
tour$ProductPitched = as.factor(tour$ProductPitched)
tour$MaritalStatus = as.factor(tour$MaritalStatus)
tour$Passport = as.factor(tour$Passport)
tour$OwnCar = as.factor(tour$OwnCar)
tour$Designation = as.factor(tour$Designation)

dim(tour)# 4128 19 
summary(tour)

attach(tour)

data = tour %>% group_by(ProductPitched,ProdTaken) %>% summarize(n=n()) %>% mutate(Proportion=n/sum(n)) 
ggplot(data, aes(x=ProductPitched, fill = ProdTaken, group = ProdTaken)) + geom_bar(aes(y=Proportion), stat="identity", position = "dodge")

tour_num = tour %>% select(Age,DurationOfPitch,NumberOfPersonVisiting,NumberOfFollowups, PreferredPropertyStar,NumberOfTrips,PitchSatisfactionScore,NumberOfChildrenVisiting,MonthlyIncome)
corrplot(cor(tour_num), type="upper", method="color",addCoef.col = "black",number.cex = 0.6)

Listing 1. Data Preparation and Preliminary Analysis

library(dplyr)
library(ggplot2)
library(ggforce)

tour_data = summarise(tour,mean(Age),sd(Age),mean(MonthlyIncome),sd(MonthlyIncome))
tour_scal <- tour %>% mutate(Age_scal = scale(Age),
                             MonthlyIncome_scal = scale(MonthlyIncome)) %>% select(-c(Age,MonthlyIncome))
str(tour_scal)

#Basic
tour_basic = tour %>% filter(ProductPitched == "Basic") %>%
  select(-ProductPitched)
tour_scal_basic = tour_scal %>% filter(ProductPitched == "Basic") %>%
  select(-ProductPitched)

set.seed(100)
wcss_basic <- function(k) {
  kmeans(tour_scal_basic, k, nstart = 10 )$tot.withinss
}
k_basic.values <- 1:30
wcss_k_basic<-sapply(k_basic.values, wcss_basic)
plot(k_basic.values, wcss_k_basic,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
set.seed(100)
k_basic <- kmeans(tour_scal_basic, 6, nstart = 25)
k_basic
plot(tour_scal_basic[,1:2], col=k_basic$cluster)
points(k_basic$centers[,1:2], col=1:6, pch=8, cex=2)

k_basic_df = cbind(data.frame(k_basic$centers),data.frame(k_basic$size),
                   data.frame(k_basic$withinss)) %>% mutate("k_basic.SD"=
                                                              sqrt(k_basic.withinss/(k_basic.size-1))) %>% mutate(
                                                                "Actual_Age_Mean" = Age_scal*tour_data[1,2]+tour_data[1,1],
                                                                "Actual_Age_SD" = k_basic.SD*tour_data[1,2],
                                                                "Actual_Income_Mean" = MonthlyIncome_scal*tour_data[1,4]+tour_data[1,3],
                                                                "Actual_Income_SD" = k_basic.SD*tour_data[1,4]) %>%
  select(-c(Age_scal,MonthlyIncome_scal,k_basic.size,k_basic.withinss,k_basic.SD))

ggplot()+
  geom_point(data=tour_basic,aes(Age,MonthlyIncome,
                                 color=as.character(k_basic$cluster))) +
  geom_point(data=k_basic_df[,c(1,3)],
             aes(x=Actual_Age_Mean,y=Actual_Income_Mean)) +
  geom_ellipse(aes(x0 = k_basic_df[1,1], y0 = k_basic_df[1,3],
                   a = k_basic_df[1,2], b = k_basic_df[1,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_basic_df[2,1], y0 = k_basic_df[2,3],
                   a = k_basic_df[2,2], b = k_basic_df[2,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_basic_df[3,1], y0 = k_basic_df[3,3],
                   a = k_basic_df[3,2], b = k_basic_df[3,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_basic_df[4,1], y0 = k_basic_df[4,3],
                   a = k_basic_df[4,2], b = k_basic_df[4,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_basic_df[5,1], y0 = k_basic_df[5,3],
                   a = k_basic_df[5,2], b = k_basic_df[5,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_basic_df[6,1], y0 = k_basic_df[6,3],
                   a = k_basic_df[6,2], b = k_basic_df[6,4], angle = 0)) +
  geom_rect(aes(xmin=35, xmax=50, ymin=35000, ymax=37500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=30, xmax=55, ymin=30000, ymax=35000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=35, xmax=50, ymin=27500, ymax=30000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=15, xmax=25, ymin=17500, ymax=20000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=25, xmax=35, ymin=20000, ymax=22500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=35, xmax=45, ymin=17500, ymax=22500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=50, xmax=60, ymin=17500, ymax=22500),
            linetype=0, fill="cyan", alpha=0.2) +
  scale_x_continuous(limits = c(15, 65)) + 
  scale_y_continuous(limits = c(15000, 40000))

#Standard
tour_standard = tour %>% filter(ProductPitched == "Standard") %>%
  select(-ProductPitched)
tour_scal_standard = tour_scal %>% filter(ProductPitched == "Standard") %>%
  select(-ProductPitched)

set.seed(100)
wcss_standard <- function(k) {
  kmeans(tour_scal_standard, k, nstart = 10 )$tot.withinss
}
k_standard.values <- 1:30
wcss_k_standard<-sapply(k_standard.values, wcss_standard)
plot(k_standard.values, wcss_k_standard,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
set.seed(100)
k_standard <- kmeans(tour_scal_standard, 7, nstart = 25)
k_standard
plot(tour_scal_standard[,1:2], col=k_standard$cluster)
points(k_standard$centers[,1:2], col=1:7, pch=8, cex=2)

k_standard_df = cbind(data.frame(k_standard$centers),data.frame(k_standard$size),
                      data.frame(k_standard$withinss)) %>% mutate("k_standard.SD"=
                                                                    sqrt(k_standard.withinss/(k_standard.size-1))) %>% mutate(
                                                                      "Actual_Age_Mean" = Age_scal*tour_data[1,2]+tour_data[1,1],
                                                                      "Actual_Age_SD" = k_standard.SD*tour_data[1,2],
                                                                      "Actual_Income_Mean" = MonthlyIncome_scal*tour_data[1,4]+tour_data[1,3],
                                                                      "Actual_Income_SD" = k_standard.SD*tour_data[1,4]) %>%
  select(-c(Age_scal,MonthlyIncome_scal,k_standard.size,k_standard.withinss,k_standard.SD))

ggplot()+
  geom_point(data=tour_standard,aes(Age,MonthlyIncome,
                                    color=as.character(k_standard$cluster))) +
  geom_point(data=k_standard_df[,c(1,3)],
             aes(x=Actual_Age_Mean,y=Actual_Income_Mean)) +
  geom_ellipse(aes(x0 = k_standard_df[1,1], y0 = k_standard_df[1,3],
                   a = k_standard_df[1,2], b = k_standard_df[1,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_standard_df[2,1], y0 = k_standard_df[2,3],
                   a = k_standard_df[2,2], b = k_standard_df[2,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_standard_df[3,1], y0 = k_standard_df[3,3],
                   a = k_standard_df[3,2], b = k_standard_df[3,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_standard_df[4,1], y0 = k_standard_df[4,3],
                   a = k_standard_df[4,2], b = k_standard_df[4,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_standard_df[5,1], y0 = k_standard_df[5,3],
                   a = k_standard_df[5,2], b = k_standard_df[5,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_standard_df[6,1], y0 = k_standard_df[6,3],
                   a = k_standard_df[6,2], b = k_standard_df[6,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_standard_df[7,1], y0 = k_standard_df[7,3],
                   a = k_standard_df[7,2], b = k_standard_df[7,4], angle = 0)) +
  geom_rect(aes(xmin=45, xmax=60, ymin=32500, ymax=37500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=45, xmax=60, ymin=25000, ymax=30000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=45, xmax=55, ymin=20000, ymax=25000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=40, xmax=45, ymin=20000, ymax=22500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=45, xmax=50, ymin=17500, ymax=20000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=35, xmax=40, ymin=27500, ymax=30000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=30, xmax=35, ymin=20000, ymax=30000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=25, xmax=30, ymin=20000, ymax=25000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=20, xmax=25, ymin=20000, ymax=22500),
            linetype=0, fill="cyan", alpha=0.2) +
  scale_x_continuous(limits = c(15, 65)) + 
  scale_y_continuous(limits = c(15000, 40000))

#Deluxe
tour_deluxe = tour %>% filter(ProductPitched == "Deluxe") %>%
  select(-ProductPitched)
tour_scal_deluxe = tour_scal %>% filter(ProductPitched == "Deluxe") %>%
  select(-ProductPitched)

set.seed(100)
wcss_deluxe <- function(k) {
  kmeans(tour_scal_deluxe, k, nstart = 10 )$tot.withinss
}
k_deluxe.values <- 1:30
wcss_k_deluxe<-sapply(k_deluxe.values, wcss_deluxe)
plot(k_deluxe.values, wcss_k_deluxe,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
set.seed(100)
k_deluxe <- kmeans(tour_scal_deluxe, 6, nstart = 25)
k_deluxe
plot(tour_scal_deluxe[,1:2], col=k_deluxe$cluster)
points(k_deluxe$centers[,1:2], col=1:6, pch=8, cex=2)

k_deluxe_df = cbind(data.frame(k_deluxe$centers),data.frame(k_deluxe$size),
                    data.frame(k_deluxe$withinss)) %>% mutate("k_deluxe.SD"=
                                                                sqrt(k_deluxe.withinss/(k_deluxe.size-1))) %>% mutate(
                                                                  "Actual_Age_Mean" = Age_scal*tour_data[1,2]+tour_data[1,1],
                                                                  "Actual_Age_SD" = k_deluxe.SD*tour_data[1,2],
                                                                  "Actual_Income_Mean" = MonthlyIncome_scal*tour_data[1,4]+tour_data[1,3],
                                                                  "Actual_Income_SD" = k_deluxe.SD*tour_data[1,4]) %>%
  select(-c(Age_scal,MonthlyIncome_scal,k_deluxe.size,k_deluxe.withinss,k_deluxe.SD))

ggplot()+
  geom_point(data=tour_deluxe,aes(Age,MonthlyIncome,
                                  color=as.character(k_deluxe$cluster))) +
  geom_point(data=k_deluxe_df[,c(1,3)],
             aes(x=Actual_Age_Mean,y=Actual_Income_Mean)) +
  geom_ellipse(aes(x0 = k_deluxe_df[1,1], y0 = k_deluxe_df[1,3],
                   a = k_deluxe_df[1,2], b = k_deluxe_df[1,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_deluxe_df[2,1], y0 = k_deluxe_df[2,3],
                   a = k_deluxe_df[2,2], b = k_deluxe_df[2,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_deluxe_df[3,1], y0 = k_deluxe_df[3,3],
                   a = k_deluxe_df[3,2], b = k_deluxe_df[3,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_deluxe_df[4,1], y0 = k_deluxe_df[4,3],
                   a = k_deluxe_df[4,2], b = k_deluxe_df[4,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_deluxe_df[5,1], y0 = k_deluxe_df[5,3],
                   a = k_deluxe_df[5,2], b = k_deluxe_df[5,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_deluxe_df[6,1], y0 = k_deluxe_df[6,3],
                   a = k_deluxe_df[6,2], b = k_deluxe_df[6,4], angle = 0)) +
  geom_rect(aes(xmin=45, xmax=55, ymin=32500, ymax=37500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=45, xmax=55, ymin=20000, ymax=27500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=20, xmax=40, ymin=20000, ymax=22500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=25, xmax=40, ymin=22500, ymax=25000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=35, xmax=40, ymin=25000, ymax=27500),
            linetype=0, fill="cyan", alpha=0.2) +
  scale_x_continuous(limits = c(15, 65)) + 
  scale_y_continuous(limits = c(15000, 40000))

#Super Deluxe
tour_sdeluxe = tour %>% filter(ProductPitched == "Super Deluxe") %>%
  select(-ProductPitched)
tour_scal_sdeluxe = tour_scal %>% filter(ProductPitched == "Super Deluxe") %>%
  select(-ProductPitched)

set.seed(100)
wcss_sdeluxe <- function(k) {
  kmeans(tour_scal_sdeluxe, k, nstart = 10 )$tot.withinss
}
k_sdeluxe.values <- 1:14
wcss_k_sdeluxe<-sapply(k_sdeluxe.values, wcss_sdeluxe)
plot(k_sdeluxe.values, wcss_k_sdeluxe,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
set.seed(100)
k_sdeluxe <- kmeans(tour_scal_sdeluxe, 4, nstart = 25)
k_sdeluxe
plot(tour_scal_sdeluxe[,1:2], col=k_sdeluxe$cluster)
points(k_sdeluxe$centers[,1:2], col=1:4, pch=8, cex=2)

k_sdeluxe_df = cbind(data.frame(k_sdeluxe$centers),data.frame(k_sdeluxe$size),
                     data.frame(k_sdeluxe$withinss)) %>% mutate("k_sdeluxe.SD"=
                                                                  sqrt(k_sdeluxe.withinss/(k_sdeluxe.size-1))) %>% mutate(
                                                                    "Actual_Age_Mean" = Age_scal*tour_data[1,2]+tour_data[1,1],
                                                                    "Actual_Age_SD" = k_sdeluxe.SD*tour_data[1,2],
                                                                    "Actual_Income_Mean" = MonthlyIncome_scal*tour_data[1,4]+tour_data[1,3],
                                                                    "Actual_Income_SD" = k_sdeluxe.SD*tour_data[1,4]) %>%
  select(-c(Age_scal,MonthlyIncome_scal,k_sdeluxe.size,k_sdeluxe.withinss,k_sdeluxe.SD))

ggplot()+
  geom_point(data=tour_sdeluxe,aes(Age,MonthlyIncome,
                                   color=as.character(k_sdeluxe$cluster))) +
  geom_point(data=k_sdeluxe_df[,c(1,3)],
             aes(x=Actual_Age_Mean,y=Actual_Income_Mean)) +
  geom_ellipse(aes(x0 = k_sdeluxe_df[1,1], y0 = k_sdeluxe_df[1,3],
                   a = k_sdeluxe_df[1,2], b = k_sdeluxe_df[1,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_sdeluxe_df[2,1], y0 = k_sdeluxe_df[2,3],
                   a = k_sdeluxe_df[2,2], b = k_sdeluxe_df[2,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_sdeluxe_df[3,1], y0 = k_sdeluxe_df[3,3],
                   a = k_sdeluxe_df[3,2], b = k_sdeluxe_df[3,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_sdeluxe_df[4,1], y0 = k_sdeluxe_df[4,3],
                   a = k_sdeluxe_df[4,2], b = k_sdeluxe_df[4,4], angle = 0)) +
  geom_rect(aes(xmin=40, xmax=45, ymin=27500, ymax=35000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=50, xmax=60, ymin=27500, ymax=32500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=35, xmax=45, ymin=22500, ymax=25000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=40, xmax=45, ymin=20000, ymax=22500),
            linetype=0, fill="cyan", alpha=0.2) +
  scale_x_continuous(limits = c(15, 65)) + 
  scale_y_continuous(limits = c(15000, 40000))

#King
tour_king = tour %>% filter(ProductPitched == "King") %>%
  select(-ProductPitched)
tour_scal_king = tour_scal %>% filter(ProductPitched == "King") %>%
  select(-ProductPitched)

set.seed(100)
wcss_king <- function(k) {
  kmeans(tour_scal_king, k, nstart = 10 )$tot.withinss
}
k_king.values <- 1:12
wcss_k_king<-sapply(k_king.values, wcss_king)
plot(k_king.values, wcss_k_king,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
set.seed(100)
k_king <- kmeans(tour_scal_king, 3, nstart = 25)
k_king
plot(tour_scal_king[,1:2], col=k_king$cluster)
points(k_king$centers[,1:2], col=1:3, pch=8, cex=2)

k_king_df = cbind(data.frame(k_king$centers),data.frame(k_king$size),
                  data.frame(k_king$withinss)) %>% mutate("k_king.SD"=
                                                            sqrt(k_king.withinss/(k_king.size-1))) %>% mutate(
                                                              "Actual_Age_Mean" = Age_scal*tour_data[1,2]+tour_data[1,1],
                                                              "Actual_Age_SD" = k_king.SD*tour_data[1,2],
                                                              "Actual_Income_Mean" = MonthlyIncome_scal*tour_data[1,4]+tour_data[1,3],
                                                              "Actual_Income_SD" = k_king.SD*tour_data[1,4]) %>%
  select(-c(Age_scal,MonthlyIncome_scal,k_king.size,k_king.withinss,k_king.SD))

ggplot()+
  geom_point(data=tour_king,aes(Age,MonthlyIncome,
                                color=as.character(k_king$cluster))) +
  geom_point(data=k_king_df[,c(1,3)],
             aes(x=Actual_Age_Mean,y=Actual_Income_Mean)) +
  geom_ellipse(aes(x0 = k_king_df[1,1], y0 = k_king_df[1,3],
                   a = k_king_df[1,2], b = k_king_df[1,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_king_df[2,1], y0 = k_king_df[2,3],
                   a = k_king_df[2,2], b = k_king_df[2,4], angle = 0)) +
  geom_ellipse(aes(x0 = k_king_df[3,1], y0 = k_king_df[3,3],
                   a = k_king_df[3,2], b = k_king_df[3,4], angle = 0)) +
  geom_rect(aes(xmin=40, xmax=45, ymin=35000, ymax=37500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=50, xmax=60, ymin=35000, ymax=37500),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=20, xmax=35, ymin=17500, ymax=20000),
            linetype=0, fill="cyan", alpha=0.2) +
  geom_rect(aes(xmin=25, xmax=30, ymin=20000, ymax=22500),
            linetype=0, fill="cyan", alpha=0.2) +
  scale_x_continuous(limits = c(15, 65)) + 
  scale_y_continuous(limits = c(15000, 40000))

Listing 2. ML for Clustering

library(dplyr)
library(ggplot2)
library(class)
library(e1071)

tour[, c("CityTier","TypeofContact","Occupation","Gender","ProductPitched","MaritalStatus","Passport","OwnCar","Designation")] = sapply(tour[, c("CityTier","TypeofContact","Occupation","Gender","ProductPitched","MaritalStatus","Passport","OwnCar","Designation")], unclass)

##Logistic Regression Method

#Split data
set.seed(100)
training.idx = sample(1: nrow(tour), size=nrow(tour)*0.8)
train.data =tour[training.idx, ]
test.data = tour[-training.idx, ]

#Logistic regression
mlogit = glm(ProdTaken ~., data = train.data, family = "binomial")
summary(mlogit)

Pred.p =predict(mlogit, newdata =test.data, type = "response")

ProdTaken_pred_num =ifelse(Pred.p > 0.5, 1, 0)
ProdTaken_pred =factor(ProdTaken_pred_num, levels=c(0, 1))

#Accuracy
mean(ProdTaken_pred ==test.data$ProdTaken)


#Confusion matrix
tab=table(ProdTaken_pred,test.data$ProdTaken)
tab

##kNN Method

tour_kNN=tour

nor=function(x) { (x -min(x))/(max(x)-min(x)) }
tour_kNN[,2:19]=sapply(tour[,2:19], nor)

#Split data
set.seed(100)
training.idx = sample(1: nrow(tour_kNN), size=nrow(tour_kNN)*0.8)
train.data = tour_kNN[training.idx, ]
test.data = tour_kNN[-training.idx, ]

#Try different k to find the best classifier
ac=rep(0, 30)
for(i in 1:30){
  set.seed(101)
  knn.i=knn(train.data[,2:19], test.data[,2:19], cl=train.data$ProdTaken, k=i)
  ac[i]=mean(knn.i ==test.data$ProdTaken)
  cat("k=", i, " accuracy=", ac[i], "\n")
}

#Accuracy plot
plot(ac, type="b", xlab="K",ylab="Accuracy")
set.seed(101)
knn1=knn(train.data[,2:19], test.data[,2:19], cl=train.data$ProdTaken, k=1)
mean(knn1 ==test.data$ProdTaken)
table(knn1,test.data$ProdTaken)
##SVM Method

#Split data
set.seed(100)
training.idx = sample(1: nrow(tour), size=nrow(tour)*0.8)
train.data = tour[training.idx, ]
test.data = tour[-training.idx, ]

#SVM classification
m.svm=svm(ProdTaken~., data = train.data, kernel="linear")

summary(m.svm)

#Predict newdata in test set
pred.svm = predict(m.svm, newdata=test.data[,2:19])

#Evaluate classification performance and check accuracy
table(pred.svm, test.data$ProdTaken)
mean(pred.svm ==test.data$ProdTaken)

#Set a seed for reproducing results, improve model by using radial kernel
set.seed(123)
m.svm.tune=tune.svm(ProdTaken~., data=train.data, kernel="radial",cost=10^(-1:2), gamma=c(.1,.5,1,2))
summary(m.svm.tune)

#Visualize results of parameter tuning
plot(m.svm.tune)

#Confusion matrix and accuracy
best.svm = m.svm.tune$best.model
pred.svm.tune = predict(best.svm, newdata=test.data[,2:19])
table(pred.svm.tune, test.data$ProdTaken)

mean(pred.svm.tune ==test.data$ProdTaken)

