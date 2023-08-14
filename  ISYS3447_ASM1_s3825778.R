library(car)
library(dplyr)
library(fpp3)
library(tidyr)
library(pastecs)
library(VIM)
library(FactoMineR)
library(missMDA)
library(naniar)
library(strucchange)
library(corrplot)
library(GGally)
library(readxl)
library(janitor)
library(RColorBrewer)
library(ltm)
library(egg)
library(scales)

#Tran Van Phuoc s3825778

#Import data
data <- read.csv("/Users/phuoctran/Desktop/Intro to BA/ASM1/ISYS3446_A1_IntelliAuto-1.csv")

#Question 1
# The normal working week is 40 hours, I would like to get an overall summary of the number of 
# hours worked at the organisation. Also, I am interested to know how many of the employees are working more than 60 hours?  

## overall summary of total hours worked
work_hrs <- format(stat.desc(data$WorkHrs), scientific = FALSE)
print(work_hrs)
write.csv(work_hrs,"/Users/phuoctran/Desktop/Intro to BA/ASM1/Work Hours Summary.csv")

###Get Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

print(getmode(data$WorkHrs))

#Histogram and boxplot
par(mfrow=c(2,1))
hist(data$WorkHrs,main = "Histogram of employee worked hours",xlab = "Worked Hours",ylab = "Number of observation")
boxplot(data$WorkHrs, horizontal = TRUE, main = "Boxplot of employee worked hours",xlab = "Worked Hours")

data %>% ggplot(aes)


plt1 <- data %>% ggplot(aes(x=WorkHrs)) +
  geom_histogram(fill="skyblue2", color="black", alpha=1,bins = 10) + ylim(0,600)+
  xlab("Number of Worked Hours") + ylab("Number of observations") + ggtitle("Histogram and boxplot of Employees' Worked Hours") +
  theme_classic()

plt2 <- data %>%
  ggplot(aes(x = WorkHrs)) + 
  geom_boxplot(fill = "skyblue2",alpha = 0.5) +
  stat_boxplot(geom = 'errorbar') +
  xlab("Number of Worked Hours") + theme_classic()

ggarrange(plt1, plt2, heights = 2:1)

#Correlation Matrix
data_numeric <- Filter(is.numeric, data[,-1])
cor(data_numeric)
ggcorr(data_numeric,label = TRUE,nbreaks = 6)

## Number of employees worked more than 60 hours/week
sum(data$WorkHrs > 60)

data["worked_hours_group"] = cut(data$WorkHrs, c(25, 30, 40, 50, 60, Inf), 
                        c("< 30", "31-40", "41-50", "51-60","> 60"), include.lowest=TRUE)

work_hrs_count <- aggregate(data$WorkHrs, by=list(data$worked_hours_group), FUN=length)
write.csv(work_hrs_count,"/Users/phuoctran/Desktop/Intro to BA/ASM1/work_hours_count.csv")

#Barplot for work_hrs_count

ggplot(work_hrs_count,aes(x=Group.1,y=x,fill=Group.1)) + 
  geom_bar(stat='identity',alpha = 0.8,color = "Black",width = 0.6) + scale_fill_brewer(palette = "Reds")+
  geom_text(aes(label = x), vjust = -0.5) +
  xlab("Worked Hours") + ylab("Number of employees")+ ylim(0,500)+
  ggtitle("Barplot illustrates number of employees per worked hour group") +
  theme_classic(base_size = 15) + theme(legend.position="none")


data["less_over_40"] = cut(data$WorkHrs, c(25,40,Inf), 
                           c("=< 40",">40"), include.lowest=TRUE)
less_over_40_count <- aggregate(data$WorkHrs, by=list(data$less_over_40), FUN=length)

ggplot(less_over_40_count,aes(x=Group.1,y=x,fill=Group.1)) + 
  geom_bar(stat='identity',alpha = 0.8,color = "Black",width = 0.4) + scale_fill_brewer(palette = "Reds")+
  geom_text(aes(label = x), hjust = -0.5) +
  xlab("Worked Hours") + ylab("Number of employees")+ ylim(0,600)+
  ggtitle("Barplot illustrates number of employees per worked hour group") +
  theme_classic(base_size = 15) + theme(legend.position="none") + coord_flip()


#Question 2
#Are there any differences in hours worked based on the following?
#On the basis of the comparisons, can you provide me with a brief summary of which employee groups are working the longest hours? 
# 1.Gender 2. Age 3.Years in workforce. 

#1.Gender
work_hrs_gender <- aggregate(data$WorkHrs, by=list(data$Sex), FUN= median)
print(work_hrs_gender)
#Barplot
ggplot(work_hrs_gender,aes(x=Group.1,y=x)) + 
  geom_bar(stat='identity',fill=c("pink1","skyblue2"),alpha = 0.9,width=0.4,color = "black")+
  geom_text(aes(label = x), hjust = -0.5) + ylim(0,60)+
  xlab("Gender") + ylab("Median Worked Hours") + 
  ggtitle("Barplot illustrates the difference between male and female employees in median worked hours") +
  theme_classic(base_size = 15) + coord_flip() + geom_hline(yintercept = 40,linetype = "dashed",col = "red",size = 0.4,show.legend = TRUE) +
  theme(axis.text = element_text(size = 12))
## Male workers have a higher total of work hours compared to female workers


#Median function for boxplot
fun_median <- function(x){
  return(data.frame(y=median(x),label=median(x,na.rm=T)))}
#Boxplot
ggplot(data,aes(x=WorkHrs,y=Sex)) +
  geom_boxplot(fill = c("pink1","skyblue2")) + stat_boxplot(geom = 'errorbar')+
  stat_summary(fun = median, geom="line",colour="darkred", size=0.2) +
  stat_summary(fun.data = fun_median, geom="text", hjust=-0.7, vjust = -0.5,color = "black",fontface = "bold") + 
  theme_classic() + xlab("Number of Worked Hours") + ylab("Gender")+
  theme(axis.text = element_text(size = 12))


#Correlation
biserial.cor(data$WorkHrs,data$Sex)

#2. Age

data["age_group"] = cut(data$Age, c(18, 29, 39, 49, 59, Inf), 
                        c("18-29", "30-39", "40-49", "50-59",">=60"), include.lowest=TRUE)


work_hrs_age_median <- aggregate(data$WorkHrs, by=list(data$age_group), FUN=median)
print(work_hrs_age_median)

cor(data$WorkHrs,data$Age)
#Barplot

## Median
library(RColorBrewer)

#Barplot
ggplot(work_hrs_age_median,aes(x=Group.1,y=x,fill=Group.1)) + 
  geom_bar(stat='identity',width=0.6,color = "black") + ylim(0,60)+ theme_classic(base_size = 15)+
  scale_fill_brewer(palette = "Blues")+ theme(legend.position="none")+
  geom_text(aes(label = x), vjust = -0.5) +
  xlab("Age groups") + ylab("Median Worked Hours") + 
  ggtitle("Barplot illustrates the difference between age groups in median worked hours") +
  geom_hline(yintercept = 40,linetype = "dashed",col = "red",size = 0.4,show.legend = TRUE) +
  theme(axis.text = element_text(size = 12))

#Boxplot
ggplot(data,aes(x=age_group,y=WorkHrs,fill = age_group)) +
  geom_boxplot() + stat_boxplot(geom = 'errorbar')+
  scale_fill_brewer(palette = "PuBu") +
  stat_summary(fun = median, geom="line",colour="darkred", size=0.2) +
  stat_summary(fun.data = fun_median, geom="text", hjust=-0.7, vjust = -0.5,color = "black",fontface = "bold") + 
  theme_classic() + xlab("Age groups") + ylab("Number of worked hours")+
  theme(axis.text = element_text(size = 12),legend.position = "none") + coord_flip()



###Correlation
cor(data$WorkHrs,data$Age)

#Number of employee per age group
num_emp_by_age_group <- aggregate(data$IdNum,by=list(data$age_group), FUN=length)
num_emp_by_age_group

ggplot(num_emp_by_age_group,aes(x=Group.1,y=x)) + 
  geom_bar(stat='identity') + theme_minimal()+
  theme(legend.position="none")+
  geom_text(aes(label = x), hjust = -0.2) +
  xlab("Age groups") + ylab("Number of employees") + 
  ggtitle("Number of employees per age group") +coord_flip()

#3. Work year
format(stat.desc(data$WrkYears),scientific = FALSE)

data["WorkYears_group"] = cut(data$WrkYears, c(1, 9, 19, 29, 39, 49,Inf), 
                        c("1-9", "10-19","20-29" ,"30-39", "40-49",">=50"), include.lowest=TRUE)

work_hrs_workyear_median <- aggregate(data$WorkHrs, by=list(data$WorkYears_group), FUN=median)
print(work_hrs_workyear_median)

#Barplot
ggplot(work_hrs_workyear_median,aes(x=Group.1,y=x,fill=Group.1)) + 
  geom_bar(stat='identity',width=0.6,color="black") + theme_classic()+ ylim(0,75)+
  scale_fill_brewer(palette = "Blues")+ theme(legend.position="none")+
  geom_text(aes(label = x), hjust = -0.5) +
  xlab("Work year group") + ylab("Median Worked Hours") + 
  geom_hline(yintercept = 40,linetype = "dashed",col = "red",size = 0.4,show.legend = TRUE) +
  ggtitle("Barplot illustrates the difference between work year groups in median worked hours") + coord_flip() +
  theme(axis.text = element_text(size = 12))

#Boxplot
ggplot(data,aes(x=WorkYears_group,y=WorkHrs,fill = WorkYears_group)) +
  geom_boxplot() + stat_boxplot(geom = 'errorbar')+
  scale_fill_brewer(palette = "PuBu") +
  stat_summary(fun = median, geom="line",colour="darkred", size=0.2) +
  stat_summary(fun.data = fun_median, geom="text", hjust=-0.7, vjust = -0.5,color = "black",fontface = "bold") + 
  theme_classic() + xlab("Work year groups") + ylab("Number of worked hours")+
  theme(axis.text = element_text(size = 12),legend.position = "none") + coord_flip()
 

cor(data$WrkYears,data$WorkHrs)

#Question 3
#What is the proportion of female employees? 
#Can you compare and summarise the following for female employees only? 
#Note:  proportion of female employ= (number of female employees) / (total number of employees) 
#Age 
#Occupation 
#Promotional opportunities 

#By Age
proportion_by_age <- prop.table(with(data, table(age_group, Sex)), 1)
write.csv(proportion_by_age,"/Users/phuoctran/Desktop/Intro to BA/ASM1/Female_proportion_by_age.csv")

proportion_by_age_df <- as.data.frame(proportion_by_age)
ggplot(proportion_by_age_df,aes(x = age_group,y= Freq ,fill = Sex)) + 
  geom_col(position = "stack",width = 0.7)+
  geom_text(aes(label = percent(round(Freq,digits = 4))),position = position_stack(vjust = 0.5),color = "black",size =5) +
  scale_fill_manual(values = c("pink1","skyblue2")) + theme_classic() + scale_y_continuous(labels = scales::percent) + 
  ylab("Percentage") + xlab("Age group") + ggtitle("Percentage stacked bar chart between genders in different age groups") +
  theme(axis.text = element_text(size = 12))

#By Occupation
proportion_by_occupation <- prop.table(with(data, table(Occup.n, Sex)), 1)
write.csv(proportion_by_occupation,"/Users/phuoctran/Desktop/Intro to BA/ASM1/Female_proportion_by_occupation.csv")


proportion_by_occupation_df <- as.data.frame(proportion_by_occupation)
ggplot(proportion_by_occupation_df,aes(x = Occup.n,y= Freq ,fill = Sex)) + 
  geom_col(position = "stack",width = 0.8)+
  geom_text(aes(label = percent(round(Freq,digits = 4))),position = position_stack(vjust = 0.5),color = "black",size =5) +
  scale_fill_manual(values = c("pink1","skyblue2")) + theme_classic() + scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete(labels=c("Managerial", "Professional","Technical/Sales","Admin Support","Service","Production","Laborer")) + 
  ylab("Percentage") + xlab("Occupation") + ggtitle("Percentage stacked bar chart between genders in different occupation") +
  theme(axis.text = element_text(size = 12))

#By Promotional Opportunity
proportion_by_promo_opp <- prop.table(with(data, table(SexPromo, Sex)), 1)

proportion_by_promo_opp_df <- as.data.frame(proportion_by_promo_opp)


ggplot(proportion_by_promo_opp_df,aes(x = SexPromo,y= Freq ,fill = Sex)) + 
  geom_col(position = "stack",width = 0.8)+
  geom_text(aes(label = percent(round(Freq,digits = 4))),position = position_stack(vjust = 0.5),color = "black",size =5) +
  scale_fill_manual(values = c("pink1","skyblue2")) + theme_classic() + scale_y_continuous(labels = scales::percent) +
  ylab("Percentage") + xlab("Survey answer") + ggtitle("Percentage stacked bar chart between genders in employee answers regarding the
  effect of their genders on their promotional opportunity")+
  theme(axis.text = element_text(size = 12))

past_promo_number <- prop.table(with(data,table(NumPromo,Sex)),1)
past_promo_number_df <- as.data.frame(past_promo_number)

ggplot(past_promo_number_df,aes(x = NumPromo,y= Freq ,fill = Sex)) + 
  geom_col(position = "stack",width = 0.8)+
  geom_text(aes(label = percent(round(Freq,digits = 4))),position = position_stack(vjust = 0.5),color = "black",size =5) +
  scale_fill_manual(values = c("pink1","skyblue2")) + theme_classic(base_size = 15) + scale_y_continuous(labels = scales::percent) +
  ylab("Percentage") + xlab("Number of past promotion") + ggtitle("Percentage stacked bar chart between genders in different number of past promotion")+
  theme(axis.text = element_text(size = 12))

biserial.cor(data$NumPromo,data$Sex)

#Question 4: Industry 4.0 awareness

#By age group

Industry_aware_age <- prop.table(with(data, table(AwareI4.0, age_group)), 2)
Industry_aware_age_df <- as.data.frame(Industry_aware_age)

age_group_plot <- ggplot(Industry_aware_age_df,aes(x = age_group,y= Freq ,fill = AwareI4.0)) + 
  geom_col(position = "stack",width = 0.7)+
  geom_text(aes(label = percent(round(Freq,digits = 4))),position = position_stack(vjust = 0.5),color = "black",size =5) +
  scale_fill_manual(values = c("indianred2","palegreen1")) + theme_classic() + scale_y_continuous(labels = scales::percent) + 
  ylab("Percentage") + xlab("Age group") + ggtitle("Percentage stacked bar chart between Yes/No of 
  Industry 4.0 awareness in different age groups")+
  theme(axis.text = element_text(size = 12))


#By union and non-union members
Industry_aware_union <- prop.table(with(data, table(AwareI4.0, MemUnion)), 2)
Industry_aware_union_df <- as.data.frame(Industry_aware_union)

ggplot(Industry_aware_union_df,aes(x = MemUnion,y= Freq ,fill = AwareI4.0)) + 
  geom_col(position = "stack",width = 0.9)+
  geom_text(aes(label = percent(round(Freq,digits = 4))),position = position_stack(vjust = 0.5),color = "black",size =5) +
  scale_fill_manual(values = c("indianred2","palegreen1")) + theme_classic() + scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete(labels=c("Non-union members", "Union members"))+
  ylab("Percentage") + xlab("Type of employee") + ggtitle("Percentage stacked bar chart between Yes/No of 
  Industry 4.0 awareness of Union and non-union members")+
  theme(axis.text = element_text(size = 12))




dummy_aware <- ifelse(data$AwareI4.0 == "Yes",1,0)
data <- data %>% mutate("dummy_aware" = dummy_aware)

dummy_union <- ifelse(data$MemUnion == "Yes Union",1,0)
data <- data %>% mutate("dummy_union" = dummy_union)

biserial.cor(data$Age,data$AwareI4.0)
biserial.cor(data$dummy_aware,data$dummy_union)

