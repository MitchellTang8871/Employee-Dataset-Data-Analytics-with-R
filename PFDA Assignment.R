####Mitchell Tang Kai Yang
###TP060964

##Packages Used
library(ggplot2)
library(dplyr)
library(plotrix)

##Data Import
data = read.csv("C:\\Users\\User\\Desktop\\Work\\PFDA Assignment\\employee_attrition.csv",header = TRUE)
names(data)=c("Employee_ID", "Record_Date", "Birth_Date", "Hired_Date", "Termination_Date", 
              "Age", "Service_Length", "City_Name", "Department_Name", "Job_Title", "Store_Name", 
              "Gender_Short", "Gender_Full", "Termination_Reason", "Termination_Type", "Status_Year", 
              "Status", "Business_Unit")

##Data Cleaning
#Drop duplicated data from dataset
data = distinct(data)
#Change the date columns datatype to datetime type
data$Record_Date = as.Date(data$Record_Date, format="%m/%d/%Y")
data$Birth_Date = as.Date(data$Birth_Date, format="%m/%d/%Y")
data$Hired_Date = as.Date(data$Hired_Date, format="%m/%d/%Y")
data$Termination_Date = as.Date(data$Termination_Date, format="%m/%d/%Y")
data$Store_Name = as.character(data$Store_Name)
#Drop data that has missing value in it 
data = data[complete.cases(data),]

##Data Transformation
#Drop one of the gender column as the dataset have 2(short form and full)
data = subset(data, select = -Gender_Full)
#Rename the remain gender column's name
names(data)[names(data)=="Gender_Short"] = "Gender"
#Correct the spelling error of Resignaton to Resignation(adding "i")
data = mutate(data, Termination_Reason = replace(
  Termination_Reason, Termination_Reason=="Resignaton", "Resignation"))
#Correct the spelling error of New Westminister to New Westminster(removing "i")
data = mutate(data, City_Name = replace(
  City_Name, City_Name=="New Westminister", "New Westminster"))

##Data Exploration
#Summary of dataset
summary(data)
#List of name of dataset
names(data)
#List of structure of dataset
str(data)
#Number of row/column of dataset
nrow(data)
ncol(data)
#List of city with alphabetical order & level number of city
sort(unique(factor(data$City_Name)))
#List of department with alphabetical order & level number of department
sort(unique(factor(data$Department_Name)))
# List of job title in dataset with alphabetical order & level number of job title
sort(unique(factor(data$Job_Title)))
#Number of store
nlevels(factor(data$Store_Name))
#List of termination reason with alphabetical order
sort(unique(factor(data$Termination_Reason)))
#level number of termination reason
nlevels(factor(data$Termination_Reason))
#List of termination type with alphabetical order
sort(unique(factor(data$Termination_Type)))
#level number of termination type
nlevels(factor(data$Termination_Type))
#List of working status with alphabetical order
sort(unique(factor(data$Status)))
#List of business unit with alphabetical order
sort(unique(factor(data$Business_Unit)))
#Number of employees in dataset
nlevels(unique(factor(data$Employee_ID)))

################################################################################

##Question 1: Why staff terminated from the company
#Analysis 1-1: Find the number of staff who leave the company
latest = data %>% group_by(Employee_ID) %>% filter(row_number() == n())
leave = sum(latest$Status=="TERMINATED")
leave
wk = sum(latest$Status=="ACTIVE")
wk
num=c(wk, leave)
per=paste0(round(100*num/sum(num), 2), "%")
per
pie3D(num, labels=per, main="Percentage of staff with different working status",col=c("blue","yellow"), explode=0.1)
legend("topright",c("Active","Terminated"), fill=c("blue", "yellow"))

#Analysis 1-2: Find the number of staff who leave the company in terms of termination reason
rt = sum(latest$Termination_Reason=="Retirement")
rs = sum(latest$Termination_Reason=="Resignation")
lo = sum(latest$Termination_Reason=="Layoff")
num=c(rt, rs, lo)
num
per=paste0(round(100*num/sum(num), 2), "%")
per
pie3D(num, labels=per, main="Percentage of staff leaving with termination reason",col=c("blue","red","yellow"),explode=0.1)
legend("topright",c("Retirement","Resignation","Layoff"), fill=c("blue","red","yellow"))

#Analysis 1-3: Relationship between terminated staff and gender
terminated = latest[latest$Status=="TERMINATED",]
ggplot(terminated, aes(x=Gender))+
  geom_bar(fill=c("pink","cyan"))+
  labs(title="Staff Gender as Terminated")+
  geom_text(aes(label = ..count..), stat = "count", vjust = -.5)
f=sum(terminated$Gender=="F")
m=sum(terminated$Gender=="M")
num=c(f,m)
per=paste0(round(100*num/sum(num), 2), "%")
per
pie3D(num, labels=per, main="Percentage of Staff Gender as Terminated",col=c("pink","cyan"),explode=0.1)
legend("topright",c("Female","Male"), fill=c("pink","cyan"))

#Analysis 1-4: Relationship between termination year and terminated staff
ggplot(terminated, aes(x=Status_Year, fill=Termination_Reason))+
  geom_bar(position="stack",col="black")+
  labs(title="Termination Year of Terminated Staff in term of Termination Reason")+
  geom_text(aes(label= ..count..), stat = "count", vjust = -.5, position=position_stack(1))

#Analysis 1-5: Relationship between terminated staff and city
ggplot(terminated, aes(fill=Termination_Reason, x=City_Name)) + 
  labs(title="Proportion of Terminated Staff in term of City",y="Proportion")+
  geom_bar(position="fill",col="black")+coord_flip()
susCity=terminated %>% group_by(Employee_ID) %>% filter(City_Name=="Williams Lake"|City_Name=="White Rock"|City_Name=="Princeton"|
                                                      City_Name=="Pitt Meadows"|City_Name=="Ocean Falls"|City_Name=="North Vancouver"|
                                                      City_Name=="Bella Bella"|City_Name=="Haney"|City_Name=="Grand Forks"|
                                                      City_Name=="Fort Nelson"|City_Name=="Dawson Creek"|City_Name=="Dease Lake"|
                                                      City_Name=="Cortes Island"|City_Name=="Blue River")
ggplot(susCity, aes(x=City_Name, fill=Termination_Reason)) + 
  geom_bar(col="black") +
  labs(title="Terminated Staff in term of City") + coord_flip()+
  geom_text(aes(label= ..count..), stat = "count", hjust=-.15, position=position_stack(1))

#Analysis 1-6: Relationship between terminated staff and store
ggplot(terminated, aes(fill=Termination_Reason, x=Store_Name)) + 
  labs(title="Proportion of Terminated Staff in term of Store",y="Proportion")+
  geom_bar(position="fill",col="black")+coord_flip()
susStore=terminated %>% group_by(Employee_ID) %>% filter(Store_Name==3|Store_Name==4|Store_Name==6|
                                                      Store_Name==7|Store_Name==9|Store_Name==10|
                                                      Store_Name==11|Store_Name==13|Store_Name==14|
                                                      Store_Name==20|Store_Name==22|Store_Name==23|
                                                      Store_Name==24|Store_Name==27|Store_Name==34|
                                                      Store_Name==39|Store_Name==40|Store_Name==42|
                                                      Store_Name==43|Store_Name==44|Store_Name==45|
                                                      Store_Name==46)
ggplot(susStore, aes(x=Store_Name, fill=Termination_Reason)) + 
  geom_bar(col="black") +
  labs(title="Terminated Staff in term of Store") + coord_flip()+
  geom_text(aes(label= ..count..), stat = "count", hjust=-.15, position=position_stack(1))

#Analysis 1-7: Relationship between terminated staff and department
ggplot(terminated, aes(fill=Termination_Reason, x=Department_Name)) + 
  labs(title="Proportion of Terminated Staff in term of Department",y="Proportion")+
  geom_bar(position="fill",col="black")+coord_flip()

#Analysis 1-8: Relationship between terminated staff and job title
ggplot(terminated, aes(fill=Termination_Reason, x=Job_Title)) + 
  labs(title="Proportion of Terminated Staff in term of Job Title",y="Proportion")+
  geom_bar(position="fill",col="black")+coord_flip()

#Analysis 1-9: Relationship between terminated staff and business unit
ggplot(terminated, aes(fill=Termination_Reason, x=Business_Unit)) + 
  labs(title="Proportion of Terminated Staff in term of Business Unit",y="Proportion")+
  geom_bar(position="fill",col="black")+coord_flip()

#Analysis 1-10: Relationship between staff, gender, years
ggplot(data, aes(x=Status_Year, fill=Gender))+
  geom_bar(position='dodge',col="black")+
  labs(title="Gender of staff over the years")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))


################################################################################

##Question 2: Why staff resign from the company
#Analysis 2-1: Relationship between age and resigned staff
resign = latest[latest$Termination_Reason=="Resignation",]
ggplot(resign, aes(x=Age))+geom_histogram(binwidth=0.5, aes(fill=..count..))+
  labs(title="Staff Age as Resigning")+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)

#Analysis 2-2: Relationship between gender and resigned staff
ggplot(resign, aes(x=Gender))+
  geom_bar(fill=c("pink","cyan"))+
  labs(title="Staff Gender as Resigning")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)
f=sum(resign$Gender=="F")
m=sum(resign$Gender=="M")
num=c(f,m)
per=paste0(round(100*num/sum(num), 2), "%")
per
pie3D(num, labels=per, main="Percentage of Staff Gender as Resigning",col=c("pink","cyan"),explode=0.1)
legend("topright",c("Female","Male"), fill=c("pink","cyan"))

#Analysis 2-3: Relationship between length of service and resigned staff
ggplot(resign, aes(x=Service_Length))+
  geom_bar(aes(fill=..count..))+
  labs(title="Staff Length of Service as Resigning")+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)

#Analysis 2-4: Relationship between resignation year and resigned staff
ggplot(resign, aes(x=Status_Year))+
  geom_bar(aes(fill=..count..))+
  labs(title="Resign Year of Resigned Staff")+ 
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)

#Analysis 2-5: Relationship between resign year, gender, resigned staff
ggplot(resign, aes(x=Status_Year, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Resign Year, Gender, Resigned Staff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 2-6: Relationship between department and resigned staff
ggplot(resign, aes(x=Department_Name))+
  geom_bar(aes(fill=..count..))+
  labs(title="Department of Resigned Staff")+ 
  scale_fill_gradient("Count",low="green",high="red")+coord_flip()+
  geom_text(aes(label= ..count..), stat = "count", hjust=-.01)

#Analysis 2-7: Relationship between department, gender, resigned staff
ggplot(resign, aes(x=Department_Name, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Department, Gender, Resign")+coord_flip()+
  geom_text(aes(label= ..count..), stat = "count", hjust=-.15, position=position_dodge(.9))

#Analysis 2-8: Relationship between resign year(2011-2014), department, resigned staff
resignYear1114=resign[resign$Status_Year>=2011&resign$Status_Year<=2014,]
ggplot(resignYear1114, aes(x=Status_Year, fill=Department_Name))+
  geom_bar(position='dodge',col="black")+
  labs(title="Resign Year(2011-2014), Department, Resigned Staff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 2-9: Relationship between business unit(Head Office), gender, resigned staff
resignHO=resign[resign$Business_Unit=="HEADOFFICE",]
ggplot(resignHO, aes(x=Business_Unit, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Business Unit(Head Office), Gender, Resign")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 2-10: Relationship between business unit(Store), gender, resigned staff
resignST=resign[resign$Business_Unit=="STORES",]
ggplot(resignST, aes(x=Business_Unit, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Business Unit(Stores), Gender, Resign")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 2-11: Relationship between store(sus), gender, resigned staff
susStoreResign=susStore %>% group_by(Employee_ID) %>% filter(Store_Name==6|Store_Name==46|Store_Name==44|Store_Name==43|
                                                               Store_Name==42|Store_Name==40|Store_Name==22)
ggplot(susStoreResign, aes(x=Store_Name, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Store, Gender, Resign")+coord_flip()+
  geom_text(aes(label= ..count..), stat = "count", hjust=-.15, position=position_dodge(.9))

################################################################################

##Question 3: Why staff getting layoff by the company
#Analysis 3-1: Relationship between age and laid off staff
layoff = terminated[terminated$Termination_Reason=="Layoff",]
ggplot(layoff, aes(x=Age))+geom_histogram(binwidth=0.5, aes(fill=..count..))+
  labs(title="Staff Age as Layoff")+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)

#Analysis 3-2: Relationship between gender and laid off staff
ggplot(layoff, aes(x=Gender))+
  geom_bar(fill=c("pink","cyan"))+
  labs(title="Staff Gender as Layoff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)
f=sum(layoff$Gender=="F")
m=sum(layoff$Gender=="M")
num=c(f,m)
per=paste0(round(100*num/sum(num), 2), "%")
per
pie3D(num, labels=per, main="Percentage of Staff Gender as Layoff",col=c("pink","cyan"),explode=0.1)
legend("topright",c("Female","Male"), fill=c("pink","cyan"))

#Analysis 3-3: Relationship between length of service and laid off staff
ggplot(layoff, aes(x=Service_Length))+
  geom_bar(aes(fill=..count..))+
  labs(title="Staff Length of Service as Layoff")+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)

#Analysis 3-4: Relationship between layoff year and laid off staff
ggplot(layoff, aes(x=Status_Year))+
  geom_bar(position="dodge")+
  labs(title="Layoff Year of Layoff Staff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)

#Analysis 3-5: Relationship between layoff year, gender, laid off staff
ggplot(layoff, aes(x=Status_Year, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Layoff Year, Gender, Layoff Staff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 3-6: Relationship between department and laid off staff
ggplot(layoff, aes(x=Department_Name))+
  geom_bar(aes(fill=..count..))+
  labs(title="Department of Layoff Staff")+coord_flip()+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label= ..count..), stat = "count", hjust=-.15)

#Analysis 3-7: Relationship between department, gender, laid off staff
ggplot(layoff, aes(x=Department_Name, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Department, Gender, Layoff")+coord_flip()+
  geom_text(aes(label= ..count..), stat = "count", hjust=-.15, position=position_dodge(.9))

#Analysis 3-8: Relationship between business unit(Head Office), gender, laid off staff
layoffHO=layoff[layoff$Business_Unit=="HEADOFFICE",]
ggplot(layoffHO, aes(x=Business_Unit, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Business Unit(Head Office), Gender, Layoff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)
nrow(layoffHO)

#Analysis 3-9: Relationship between business unit(Store), gender, laid off staff
layoffST=layoff[layoff$Business_Unit=="STORES",]
ggplot(layoffST, aes(x=Business_Unit, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Business Unit(Stores), Gender, Layoff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 3-10: Relationship between city(sus), gender, laid off staff
susCityLayoff=susCity %>% group_by(Employee_ID) %>% filter(City_Name=="White Rock"|City_Name=="Princeton"|City_Name=="New Westminister"|
                                                         City_Name=="Haney"|City_Name=="Grand Forks"|City_Name=="Fort Nelson"|
                                                         City_Name=="Dawson Creek")
ggplot(susCityLayoff, aes(x=City_Name, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="City, Gender, Layoff")+coord_flip()+
  geom_text(aes(label = ..count..), stat = "count", hjust=-.15, position=position_dodge(.9))

#Analysis 3-11: Relationship between store(sus), gender, laid off staff
susStoreLayoff=susStore %>% group_by(Employee_ID) %>% filter(Store_Name==9|Store_Name==39|Store_Name==27|
                                                                Store_Name==20|Store_Name==14|Store_Name==13|
                                                                Store_Name==11)
ggplot(susStoreLayoff, aes(x=Store_Name, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Store, Gender, Layoff")+coord_flip()+
  geom_text(aes(label = ..count..), stat = "count", hjust=-.15, position=position_dodge(.9))

#Analysis 3-12: Find the length of service for laid off staff(Age64)
layoffAge64 = layoff[layoff$Age==64,]
ggplot(layoffAge64, aes(x=Service_Length))+geom_bar(aes(fill=..count..))+
  labs(title="Staff Age as Layoff")+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)

#Analysis 3-13: Relation between laid off staff(Age64) and Store
ggplot(layoffAge64,aes(x=Store_Name))+geom_bar(aes(fill=..count..))+
  labs(title="Staff Age as Layoff")+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)

################################################################################

##Question 4: Is any attributes affect staff retirement
#Analysis 4-1: Relationship between age and retired staff
retire = terminated[terminated$Termination_Reason=="Retirement",]
ggplot(retire, aes(x=Age))+geom_histogram(binwidth=0.5, aes(fill=..count..))+
  labs(title="Staff Age as Retire")+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5)

#Analysis 4-2: Relationship between gender and retired staff
ggplot(retire, aes(x=Gender))+
  geom_bar(fill=c("pink","cyan"))+
  labs(title="Staff Gender as Retire")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5)
f=sum(retire$Gender=="F")
m=sum(retire$Gender=="M")
num=c(f,m)
per=paste0(round(100*num/sum(num), 2), "%")
per
pie3D(num, labels=per, main="Percentage of Staff Gender as Retire",col=c("pink","cyan"),explode=0.1)
legend("topright",c("Female","Male"), fill=c("pink","cyan"))

#Analysis 4-3: Relationship between length of service and retired staff
ggplot(retire, aes(x=Service_Length))+
  geom_bar(aes(fill=..count..))+
  labs(title="Staff Length of Service as retire")+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5)
sort(unique(retire$Service_Length))

#Analysis 4-4: Relationship between retirement year and retired staff
ggplot(retire, aes(x=Status_Year))+
  geom_bar(position="dodge", aes(fill=..count..))+
  labs(title="Retire Year of Retire Staff")+
  scale_fill_gradient("Count",low="green",high="red")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 4-5: Relationship between retirement year(2006-2010), department, retired staff
retireYear0610=retire[retire$Status_Year>=2006&retire$Status_Year<=2010,]
ggplot(retireYear0610, aes(x=Status_Year, fill=Department_Name))+
  geom_bar(position='dodge', col="black")+
  labs(title="Retire Year, Department, Retire Staff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 4-6: Relationship between retirement year, gender, retired staff
ggplot(retire, aes(x=Status_Year, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="retire Year, Gender, retire Staff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 4-7: Relationship between department and retired staff
ggplot(retire, aes(x=Department_Name))+
  geom_bar(aes(fill=..count..))+
  labs(title="Department of retire Staff")+ 
  scale_fill_gradient("Count",low="green",high="red")+coord_flip()+
  geom_text(aes(label= ..count..), stat = "count", hjust=.1)

#Analysis 4-8: Relationship between business unit(Head Office), gender, retired staff
retireHO=retire[retire$Business_Unit=="HEADOFFICE",]
ggplot(retireHO, aes(x=Business_Unit, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Business Unit(Head Office), Gender, Retire")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 4-9: Relationship between business unit(Store), gender, retired staff
retireST=retire[retire$Business_Unit=="STORES",]
ggplot(retireST, aes(x=Business_Unit, fill=Gender))+
  geom_bar(position='dodge')+
  labs(title="Business Unit(Stores), Gender, Retire")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

################################################################################

##Question 5: Does staff birth era affect their termination
#Analysis 5-1: Find the birth era of staff
summary(latest$Birth_Date)
#******The code below need some time to run******
era = latest %>% mutate(Birth_Era = case_when(Birth_Date >= "1981-01-01" & Birth_Date < "1997-01-01" ~ "Millennials", 
                                                  Birth_Date >= "1965-01-01" & Birth_Date < "1981-01-01" ~ "Gen X",
                                                  Birth_Date >= "1955-01-01" & Birth_Date < "1965-01-01" ~ "Boomers II",
                                                  Birth_Date >= "1946-01-01" & Birth_Date < "1955-01-01" ~ "Boomers I",
                                                  Birth_Date >= "1928-01-01" & Birth_Date < "1946-01-01" ~ "Post War"))
M=sum(era$Birth_Era=="Millennials")
X=sum(era$Birth_Era=="Gen X")
B2=sum(era$Birth_Era=="Boomers II")
B1=sum(era$Birth_Era=="Boomers I")
P=sum(era$Birth_Era=="Post War")
num=c(M,X,B2,B1,P)
per=paste0(round(100*num/sum(num), 2), "%")
per
pie3D(num, labels=per, main="Percentage of Staff Birth Era",explode=0.1,col=c("light green","sky blue","violet","orange","gold"))
legend("topleft",c("Millennials","Gen X","Boomers II","Boomers I","Post War"), fill=c("light green","sky blue","violet","orange","gold"))

#Analysis 5-2: Relation between birth era and termination reason of staff
ggplot(era, aes(fill=Termination_Reason, x=Birth_Era)) + 
  labs(title="Proportion of Terminated Staff in term of Birth Era")+
  geom_bar(position="fill",col="black")
ggplot(era, aes(x=Birth_Era, fill=Termination_Reason))+
  geom_bar(position='dodge')+
  labs(title="Birth Era of Terminated Staff")+
  geom_text(aes(label= ..count..), stat = "count", vjust=-.5, position=position_dodge(.9))

#Analysis 5-3: Relation between birth era and department of resign staff
eraResign=era %>% group_by(Employee_ID) %>% filter(Termination_Reason=="Resignation")
ggplot(eraResign, aes(fill=Birth_Era, x=Department_Name)) + 
  labs(title="Proportion of Resigned Staff in term of Birth Era")+
  geom_bar(position="fill",col="black")+coord_flip()
ggplot(eraResign, aes(x=Department_Name, fill=Birth_Era))+
  geom_bar(position='stack',col="black")+
  labs(title="Birth Era of Resigned Staff")+coord_flip()

################################################################################

##Question 6: Does staff promotion affect their termination
#Analysis 6-1: Relation between promotion and terminated staff
promotion = data %>% select(Employee_ID, Department_Name, Job_Title, Termination_Reason)
promotion = distinct(promotion, across(-Termination_Reason), .keep_all=TRUE)
promotionCount = promotion %>% count(Employee_ID)-1
names(promotionCount)[names(promotionCount)=="n"] = "Promotion"
promotionFinal=merge(latest, promotionCount, by="Employee_ID")
promotionFinalT = promotionFinal[promotionFinal$Status=="TERMINATED",]
ggplot(promotionFinalT, aes(fill=Promotion, x=Termination_Reason)) + 
  labs(title="Promotion of Terminated Staff")+
  geom_bar(position="fill")

#Analysis 6-2: Relation between promotion and staff
ggplot(promotionFinal, aes(fill=Promotion, x=Termination_Reason)) + 
  labs(title="Proportion of Resigned Staff in term of Birth Era")+
  geom_bar(position="fill")
sum(promotionFinal$Promotion!=0)

################################################################################
