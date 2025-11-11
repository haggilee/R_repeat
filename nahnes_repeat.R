#-----------------------------data extraction and migration----------------------
#Extract the 2013-2014 data
setwd("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2013-2014")
#BiocManager::install("Rtools")
#install.packages("Rtools")
#install.packages("devtools")
library(haven)
library(tidyverse)
library(dplyr)
library(usethis)
library(devtools)
#install_github("jamesjiadazhan/dietaryindex")
library(readr)
library(dietaryindex)
pre_file <- list.files(pattern  = "*.xpt")
pre_xpt_all <- read_xpt((pre_file[1]))
for (i in 2 : length(pre_file)){
  pre_xpt_alone <- read_xpt(pre_file[i])
  pre_xpt_all <- merge(pre_xpt_all, pre_xpt_alone, by = "SEQN", all = T )
}
# Extract the 2015-2016 data
setwd("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2015-2016")
lat_1516_file <- list.files(pattern = "*.xpt")
lat_1516_xpt_all <- read_xpt(lat_1516_file[1])
for (i in 2 :length(lat_1516_file)){
  lat_1516_xpt_alone <- read_xpt(lat_1516_file[i])
  lat_1516_xpt_all <- merge(lat_1516_xpt_all, lat_1516_xpt_alone, by = "SEQN", all = T )
}
nhanes_variable_xpt <- read.csv("C:/Users/haggi/Desktop/Deck/Project/R_repeat/nhanes_variable&xpt.csv")
var <- nhanes_variable_xpt
var_col <- var %>% select(variable)
var_col <- rbind(var_col,"SDMVPSU", "SDMVSTRA")
#list_row_df <- split(df, seq(nrow(df)))
#var_col <- split(var_col, seq(nrow(var_col)))
var_col <- as.data.frame(var_col)
var_col[var_col == "NA" | var_col == ""] <- NA
var_col_clean <- na.omit((var_col))
var_col_clean <- as.vector(var_col_clean)

#var_col_clean <- var_col[, !apply(var_col, 2, anyNA)]
pre_xpt_data <- pre_xpt_all$SEQN
#pre_xpt_data <- pre_xpt_all[, colnames(pre_xpt_all) %in% var_col_clean, drop =T]
#var_col_clean <- c(var_col_clean)
for (i in var_col_clean){
  pre_xpt_data <- cbind(pre_xpt_data, pre_xpt_all[,i])
}
names(pre_xpt_data)[names(pre_xpt_data) == 'pre_xpt_data'] <- "SEQN"
lat_1516_xpt_data <- lat_1516_xpt_all$SEQN
for (i in var_col_clean){
  lat_1516_xpt_data <- cbind(lat_1516_xpt_data, lat_1516_xpt_all[, i])
}
names(lat_1516_xpt_data)[names(lat_1516_xpt_data)== 'lat_1516_xpt_data'] <- "SEQN"
#Change the first variable's name into SEQN
setdiff(colnames(pre_xpt_data), colnames(lat_1516_xpt_data))
#Check the difference of the dataset's variable
xpt_data <- rbind(pre_xpt_data, lat_1516_xpt_data)



#install_github("jamesjiadazhan/dietaryindex")
#Extract the nutrition data, and get the score_2013-2014
nutrition_demo <- read_xpt("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2013-2014/DEMO_H.xpt")
first_diet <- read_xpt("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2013-2014/DR1TOT_H.xpt")
sec_diet <- read_xpt("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2013-2014/DR2TOT_H.xpt")
first_diet1 <- read_sas("~/BaiduSyncdisk/nhanes_repeat/2013-2014/fped_dr1tot_1314.sas7bdat")
sec_diet2 <- read_sas("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2013-2014/fped_dr2tot_1314.sas7bdat")
AHEI1314 <- HEI2020_NHANES_FPED(FPED_PATH = first_diet1, NUTRIENT_PATH = first_diet,
                                DEMO_PATH = nutrition_demo,
                                FPED_PATH2 = sec_diet2, NUTRIENT_PATH2 = sec_diet)
#Extract the nutrition data, and get the score_2015-2016
nutrition_demo_2 <- read_xpt("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2015-2016/DEMO_I.xpt")
first_diet_2 <- read_xpt("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2015-2016/DR1TOT_I.xpt")
sec_diet_2 <- read_xpt("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2015-2016/DR2TOT_I.xpt")
first_diet_21 <-read_sas("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2015-2016/fped_dr1tot_1516.sas7bdat")
sec_diet_21 <- read_sas("C:/Users/haggi/Desktop/Deck/Project/R_repeat/2015-2016/fped_dr2tot_1516.sas7bdat")
AHEI1516 <- HEI2020_NHANES_FPED(FPED_PATH = first_diet_21, NUTRIENT_PATH = first_diet_2,
                                DEMO_PATH = nutrition_demo_2,
                                FPED_PATH2 = sec_diet_21, NUTRIENT_PATH2 = sec_diet_2)
AHEI_all <- rbind(AHEI1314, AHEI1516)

#-----------data clean--------------
xpt_yong1 <- xpt_data
xpt_yong1 <- merge(xpt_yong1, AHEI_all[, c(1, 3)], by = "SEQN", all = T ) 
#Rename the variable
colnames(xpt_yong1) <- c("SEQN", "Hcg", "Pregnancy", "Ovarian", "Breastbreeding", "Homone", "Pb", "Cd", "Hg",
                         "Se", "Mn", "Pb_DX", "Cd_DX", "Hg_DX", "Se_DX", "Mn_DX",
                         "TST", "SEX", "Age", "Month", "Gender", "Gender2", "BMI", "Drink", "Smoke", 
                         "ALP", "WBC", "Lym", "Neu", "Mon", "Plt", "Weight", "SDMVPSU", "SDMVSTRA", "HEI2020")
#Delete the line's which have NA
xpt_yong2 <- xpt_yong1[!(is.na(xpt_yong1$Pb)|is.na(xpt_yong1$Cd)|is.na(xpt_yong1$Hg)|is.na(xpt_yong1$Se)|is.na(xpt_yong1$Mn)|is.na(xpt_yong1$TST)|is.na(xpt_yong1$SEX)|
                           is.na(xpt_yong1$Age)|is.na(xpt_yong1$BMI)),]

#the second condition: age <18; pregnancy;
sum(xpt_yong2$Age < 18)
sum(xpt_yong2$Hcg %in% 1)
sum(xpt_yong2$Homone %in% 1 | xpt_yong2$Ovarian %in%1)
#Hcg'results is more correctly than questionnaire
xpt_yong3 <- xpt_yong2[!((xpt_yong2$Hcg %in%1)|(xpt_yong2$Homone %in% 1)|(xpt_yong2$Ovarian %in% 1)|
                         (xpt_yong2$Age <18)),]
colSums(is.na(xpt_yong3))/4824
#transformer the variables as factor, for the next analysis.将分类变量转变为因子。
xpt_yong3$Drink <- as.character(xpt_yong3$Drink)
xpt_yong3$Smoke <- as.character(xpt_yong3$Smoke)
xpt_yong3$Gender2 <- as.factor(xpt_yong3$Gender2)
xpt_yong3$Pb_DX <- as.factor(xpt_yong3$Pb_DX)
xpt_yong3$Cd_DX <- as.factor(xpt_yong3$Cd_DX)
xpt_yong3$Hg_DX <- as.factor(xpt_yong3$Hg_DX)
xpt_yong3$Se_DX <- as.factor(xpt_yong3$Se_DX)
xpt_yong3$Mn_DX <- as.factor(xpt_yong3$Mn_DX)
#metal which higher than normal统计有毒金属及微量元素高于上限的比例。
sum(xpt_yong3$Pb_DX == "0")/4824
sum(xpt_yong3$Cd_DX == "0")/4824
sum(xpt_yong3$Hg_DX == "0")/4824
sum(xpt_yong3$Se_DX == "0")/4824
sum(xpt_yong3$Mn_DX == "0")/4824
#transform the data of metal and toxic chemicals into normal distribution
xpt_yong3$Pb <- log10(xpt_yong3$Pb)
xpt_yong3$Cd <- log10(xpt_yong3$Cd)
xpt_yong3$Hg <- log10(xpt_yong3$Hg)
xpt_yong3$Se <- log10(xpt_yong3$Se)
xpt_yong3$Mn <- log10(xpt_yong3$Mn)
#盖帽法处理异常值
#首先找有没有异常值
boxplot(xpt_yong3$Pb)
boxplot(xpt_yong3$Cd)
boxplot(xpt_yong3$Hg)
boxplot(xpt_yong3$Se)
boxplot(xpt_yong3$Mn)
#总通过盖帽发进行异常值的处理
seq1 <- quantile(xpt_yong3$Se, 0.001)#计算0.1%位数的值作为下限
seq99 <- quantile(xpt_yong3$Se, 0.999)#计算99.9%位数的值作为上限
xpt_yong3[xpt_yong3$Se<seq1,]$Se<- seq1#将低于0.1%位数的值替换为下限
xpt_yong3[xpt_yong3$Se>seq99,]$Se<- seq99#高于99.9%位数 的值替换为上限
boxplot(xpt_yong3$Se)#验证是否替换成功
#删除不会总用到的变量,如序列号、是否怀孕、是否双重卵巢切除，是否用过激素
xpt_fenxi <- xpt_yong3[,-c(1, 3, 4, 6)]
#将持续变量赋值为分类变量
#诊断标准：HA 女性50岁及以上＞32,50岁以下则＞46|TD 男性TST＜300
xpt_fenxi$disease [xpt_fenxi$SEX ==2 & xpt_fenxi$Age <50 &xpt_fenxi$TST >46] <-"1"
xpt_fenxi$disease [xpt_fenxi$SEX ==2 & xpt_fenxi$Age <50 &(xpt_fenxi$TST <46 | xpt_fenxi$TST ==46)] <- "0"
xpt_fenxi$disease [xpt_fenxi$sex ==2 & (xpt_fenxi$Age >50 | xpt_fenxi$Age == 50) &xpt_fenxi$TST >32] <- "1"
xpt_fenxi$disease [xpt_fenxi$SEX ==2 & (xpt_fenxi$Age <50 | xpt_fenxi$Age == 50) &(xpt_fenxi$TST <32 | xpt_fenxi$TST ==32)] <- "0"
xpt_fenxi$disease [xpt_fenxi$SEX ==1 & (xpt_fenxi$TST > 300| xpt_fenxi$TST == 300)] <- "0"
xpt_fenxi$disease [xpt_fenxi$SEX ==1 & xpt_fenxi$TST < 300] <- "1"
xpt_fenxi$disease <- as.factor((xpt_fenxi$disease))#将变量因子化
#将BMI进行分组
#分为4组作为协变量
xpt_fenxi$BMI1<- ifelse(xpt_fenxi$BMI< 18.5, "<18.5",
                        ifelse((xpt_fenxi$BMI>18.5|xpt_fenxi$BMI==18.5)&xpt_fenxi$BMI<25, "18.5 to 25",
                               ifelse((xpt_fenxi$BMI>25|xpt_fenxi$BMI==25)&xpt_fenxi$BMI<30, "25 to 30", ">30")))
#分3组作为亚组分析
xpt_fenxi$BMI2<-ifelse(xpt_fenxi$BMI<25, "<25",
                       ifelse((xpt_fenxi$BMI>25|xpt_fenxi$BMI==25)&xpt_fenxi$BMI<30, "25 to 30", ">30"))
#年龄分为5组作为协变量
xpt_fenxi$Age1<- ifelse(xpt_fenxi$Age<18, "<25",
                        ifelse((xpt_fenxi$Age>18|xpt_fenxi$Age==18)&xpt_fenxi$Age<25,"18 to 25",
                               ifelse((xpt_fenxi$Age>25|xpt_fenxi$Age==25)&xpt_fenxi$Age<35, "25 to 35",
                                      ifelse((xpt_fenxi$Age>35|xpt_fenxi$Age==35)&xpt_fenxi$Age<45, "35 to 45",
                                             ifelse((xpt_fenxi$Age>45|xpt_fenxi$Age==45)&xpt_fenxi$Age<60, "45 to 60",">60")))))
#年龄另外再分5组作为亚组分析
xpt_fenxi$Age2<- ifelse(xpt_fenxi$Age<18, "<25",
                        ifelse((xpt_fenxi$Age>18|xpt_fenxi$Age==18)&xpt_fenxi$Age<35,"18 to 35",
                               ifelse((xpt_fenxi$Age>35|xpt_fenxi$Age==35)&xpt_fenxi$Age<45, "35 to 45",
                                      ifelse((xpt_fenxi$Age>45|xpt_fenxi$Age==45)&xpt_fenxi$Age<50, "45 to 50",
                                             ifelse((xpt_fenxi$Age>50|xpt_fenxi$Age==50)&xpt_fenxi$Age<60, "50 to 60",">60")))))
#种族分为4组mexican American\other Hispanic\non-hispanic white\non-hispanic black\others
xpt_fenxi$Gender1<-ifelse(xpt_fenxi$Gender==1,"Mexican American",
                          ifelse(xpt_fenxi$Gender==2, "Other Hispanic",
                                 ifelse(xpt_fenxi$Gender==3, "non-Hispanic White",
                                        ifelse(xpt_fenxi$Gender==4, "non-Hispanic Black","Others"))))
#AHEI分为4组<31\31-38\>38\missing/refused/unknown
xpt_fenxi$AHEI1[xpt_fenxi$HEI2020<31]<- "<31"
xpt_fenxi$AHEI1[(xpt_fenxi$HEI2020>31|xpt_fenxi$HEI2020==31)&xpt_fenxi$HEI2020<38]<-"≥31 to <38"
xpt_fenxi$AHEI1[xpt_fenxi$HEI2020>38|xpt_fenxi$HEI2020==38]<-"≥38"
xpt_fenxi$AHEI1[is.na(xpt_fenxi$HEI2020)]<-"Missing/refused/unknown"

#多重插补处理缺失值
library(mice)
xpt_fenxi<-mice(xpt_fenxi, method = "rf", m=5,printFlag = F, seed = 123)
xpt_fenxi<- complete(xpt_fenxi)

#计算无法直接获取的炎性因子
xpt_fenxi$SiRI<- (xpt_fenxi$Neu*xpt_fenxi$Mon)/xpt_fenxi$Lym
xpt_fenxi$SII<- (xpt_fenxi$Plt*xpt_fenxi$Neu)/xpt_fenxi$Lym
xpt_fenxi$NLR<- xpt_fenxi$Neu/xpt_fenxi$Lym
xpt_fenxi$PLR<- xpt_fenxi$Plt/xpt_fenxi$Lym
xpt_fenxi<-xpt_fenxi[,-c(16)]
xpt_fenxi_repeat <- xpt_fenxi
xpt_fenxi<- xpt_fenxi_repeat
xpt_fenxi$Drink<-as.character(xpt_fenxi$Drink)
xpt_fenxi$Smoke<- as.character(xpt_fenxi$Smoke)
#将分类变量进行赋值
xpt_fenxi$Smoke[xpt_fenxi$Smoke==1] <- "Yes"
xpt_fenxi$Smoke[xpt_fenxi$Smoke==2] <- "Yes"
xpt_fenxi$Smoke[xpt_fenxi$Smoke==3] <- "No"
xpt_fenxi$Smoke[xpt_fenxi$Smoke==7 &xpt_fenxi$Smoke ==9] <- "Missing/Refused/Unknown"
xpt_fenxi$Drink[xpt_fenxi$Drink ==1] <- "Yes"
xpt_fenxi$Drink[xpt_fenxi$Drink ==2] <- "No"
xpt_fenxi$Drink[xpt_fenxi$Drink ==7] <- "Missing/Refused/Unknown"
xpt_fenxi$Drink[xpt_fenxi$Drink ==9] <- "Missing/Refused/Unknown"
