#Tan Jun Wei TP062153

#package installation and loading for visualization purposes
#----------------------
install.packages("ggplot2")
install.packages("janitor")
install.packages("ggthemes")
install.packages("grid")
install.packages("hrbrthemes")
install.packages("dplyr")
library(ggthemes)
library(grid)
library(ggplot2)
library(scales)
library(janitor)
library(dplyr)
library(hrbrthemes)
#----------------------

#read csv file
employee_data = read.csv("C:\\Users\\JUN WEI\\Desktop\\employee_attrition.csv", header = TRUE)
employee_data

#view in table form instead of viewing it from console
View(employee_data)

#randomly view10 rows
Sample_n(employee_data, 10)
#--DATA CLEANING--#

nrow(employee_data)
#the columns recorddate_key, birthdate_key and terminationdate_key were omitted since it wont be used.
employee_data<-subset(employee_data, select = -c(recorddate_key,birthdate_key,terminationdate_key))

#clean column names, all attributes will be set to lowercase
employee_data<-clean_names(employee_data)
colnames(employee_data)

#remove duplicate records by taking the latest length of service
employee_data<-employee_data[order(employee_data$length_of_service, decreasing = TRUE),]
employee_data<-employee_data[!duplicated(employee_data$employee_id),]
nrow(employee_data)

#there was a spelling error "Resignaton" in termreason_desc, therefore it changed to "Resignation"
old_termreason<-"Resignaton"
new_termreason<-"Resignation"

for(i in seq_along(employee_data)){
  employee_data[[i]][employee_data[[i]] %in% old_termreason]<-new_termreason
}

#check how the employee_data is stored
class(employee_data)

#check how many rows and columns the data frame has
dim(employee_data)

#Data Summary, min,max of attributes will be known so that when plotting it will be easier
summarydata<-employee_data%>%
  summary(employee_data)
summarydata
summary(employee_data$age)
#orighiredate_key is formated to date and then the year was extracted. 
employee_data%>%mutate(orighiredate_key = format(as.Date(employee_data$orighiredate_key, format = "%m/%d/%Y"), "%Y"))
View(employee_data)
#tabyl is used to check the frequency or percentage in detail
tabyl(employee_data,length_of_service, gender_short)
tabyl(employee_data,age)
tabyl(employee_data,gender_short)
tabyl(employee_data,store_name)

#adorn is used for formatting the tabyl data
employee_data %>% tabyl(length_of_service) %>% adorn_pct_formatting(digits =2,affix_sign=TRUE)
employee_data %>% tabyl(gender_short) %>% adorn_pct_formatting(digits =2,affix_sign=TRUE)
employee_data %>% tabyl(gender_short, length_of_service) %>% adorn_pct_formatting(digits =2,affix_sign=TRUE)


# Analysis X-XX: Find the relationship between gender and length of service.
arrange(employee_data, length_of_service)
ggplot(data=employee_data, aes(fill=gender_short, x=length_of_service))+
  geom_bar( width=0.5, color="black", position="dodge")+
  geom_text(stat="count",aes(label=..count..), vjust=0.9, position = position_dodge(0.9), size=3)+
  scale_fill_brewer(palette = "Pastel1")+
  scale_y_continuous(expand=c(0,0))+
  labs(x="length of service",
       y="number of employees",
       fill= "gender",
       title= "The relationship between number of employees and length of service")


#AnalysisX-XX: #Find the relationship between no of employees and department 
plotdata<-employee_data %>%
  count(department_name)
plotdata


ggplot(plotdata, aes(x=n,y=reorder(department_name,n)))+
  geom_bar(stat="identity", fill = "skyblue")+
  geom_text(aes(label=n),hjust=0)+
  labs(x = "Number of Employees",
       y = "Department",
       title = "Number of Employees by department")

  


#Analysis 1-1: Find the proportion of staff left
staff_data <- employee_data
str(staff_data)

StatusCount<- as.data.frame.matrix(staff_data %>%
 group_by(status_year)%>%
 select(status)%>%
 table())  

StatusCount$TotalEmployees<-StatusCount$ACTIVE+StatusCount$TERMINATED
StatusCount$PercentTerminated<-StatusCount$TERMINATED/(StatusCount$TotalEmployees)*100
StatusCount

mean(StatusCount$PercentTerminated)

#AnalysisX-XX: Find the relationship between terminated employee and status year.
TerminationData<-as.data.frame(employee_data%>%
  filter(status=="TERMINATED"))


ggplot(data=TerminationData, aes(x=status_year,fill=termtype_desc))+
  geom_bar(position=position_stack())+
  geom_text(stat="count",aes(label=..count..), vjust=0.2, color="black", size=3.5)+
  scale_y_discrete(expand=c(0,0))+
  labs(x = "Status Year",
       y = "No of Employees Terminated",
       fill = "Termination Description",
       title = "Number of Employees Terminated Voluntary vs Involuntary by STATUS YEAR")+
  theme_classic()+
  scale_x_continuous(breaks = 2006:2015)



#Analysis X-XX: Find the relationship between terminated employees age with attrition
agedata<-employee_data%>%
  distinct(employee_id,age,city_name,gender_full,status,store_name)%>%
  filter(status =="TERMINATED")%>%
  transform(age = as.numeric(age),
            store_name = as.character(store_name))

agedata

#without density line
h1<-ggplot(agedata,aes(x=as.numeric(age)))+
  geom_histogram(binwidth = 1, fill="#69b3a2", color="#e9ecef")+
  geom_vline(aes(xintercept=mean(as.numeric(age))),
             color="blue", linetype="dashed", size=1)+
  labs(x = "Age",
       y = "No of Employees",
       title="Histogram of Terminated Employees Age")+
  scale_color_brewer(palette="Accent") + 
  theme_classic()+
  scale_x_continuous(breaks = 19:65)
  
  

#with density line
h2<-ggplot(agedata,aes(x=age))+
  geom_histogram(aes(y=..density..),binwidth = 1, fill="#69b3a2", color="#e9ecef")+
  geom_vline(aes(xintercept=mean(age)),
             color="blue", linetype="dashed", size=1)+
  labs(x = "Age",
       y = "Density",
       title="Histogram of Employees Age")+
  geom_density(color="#00203FFF",size=1)+
  scale_color_brewer(palette="Accent") + 
  theme_classic()+
  scale_x_continuous(breaks = 19:65)+
  theme(plot.background = element_rect(colour = "black", size = 1))

#to categorize the age
agedata$age_grp <- agedata$age
agedata$age_grp <- ifelse((agedata$age<18) , '<18',agedata$age_grp)
agedata$age_grp <- ifelse((agedata$age>=18 & agedata$age<=21) , '18-21',agedata$age_grp)
agedata$age_grp <- ifelse((agedata$age>21 & agedata$age<=25) , '21-25',agedata$age_grp)
agedata$age_grp <- ifelse((agedata$age>25 & agedata$age<=30) , '25-30',agedata$age_grp)
agedata$age_grp <- ifelse((agedata$age>30 & agedata$age<=40) , '30-40',agedata$age_grp)
agedata$age_grp <- ifelse((agedata$age>40 & agedata$age<=55) , '40-55',agedata$age_grp)
agedata$age_grp <- ifelse((agedata$age>55) , '55>',agedata$age_grp)
agedata$age_grp<-as.factor(agedata$age_grp)

summary(agedata$age_grp)
View(agedata)

h3<-ggplot(agedata,aes(x=age_grp))+
  geom_bar(fill="darkolivegreen1",color="black") +
  ggtitle("Number of terminated employees by age category")+
  geom_text( stat="count", aes(label=..count..), position = position_stack(vjust= 0.5),
             colour = "black", size = 5)+
  theme(plot.background = element_rect(colour = "black", size = 1))+
  labs(x="Age Category",
       y="Number of Employees")+
  theme_classic()+
  theme(plot.background = element_rect(colour = "black", size = 1))

grid.arrange(h3,h2)

#Analysis X-XX: Find the relationship between number of employees in each city by age category
ggplot(data=agedata, aes(fill = city_name,x = age_grp))+ 
  geom_bar( stat="count",colour="black", width=0.6)+
  geom_text(aes(label=..count..),stat="count",position = "stack",hjust=5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x="Age Category", y="Number of Employees", fill="City name", title="Number of Terminated Employees in each city by age category")+
  theme_classic() 






#Analysis X-XX: Find the relationship between Status Year and Termination Reason of terminated employees.
terminateddata<-employee_data%>%
  select(status_year,status,termreason_desc,termtype_desc,department_name)%>%
  filter(status=="TERMINATED")
terminateddata
  
ggplot(data=terminateddata, aes(x =as.factor(STATUS_YEAR),y = ..count..,fill = as.factor(termreason_desc)))+
  geom_bar(stat="count",position = position_stack(),color="black")+
  geom_text(aes(label=..count..),stat="count",position = "stack",vjust=1)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x="Status Year", y="Number of Employees", fill="Termination Reason", title="The number of terminated employees with termination reason by status year")+
  theme_classic()

#AnalysisX-XX: Find the relationship between Department and Termination Reason of terminated employees
terminateddata

#ExtraFeature
ggplot(data=terminateddata, aes(x =as.factor(department_name),y =..count..,fill =as.factor(termreason_desc)))+
  geom_bar(stat="count",position = position_stack(),color="black")+
  geom_text(aes(label=..count..),stat="count",position = "stack", color="grey0",vjust=0.8)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x="Department", y="Number of terminated employees", fill="Termination Reason", title="The number of terminated employees with termination reason by department")+
  theme_classic()+
  theme(plot.background = element_rect(colour = "black", size = 1))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  scale_fill_brewer(palette="Accent")


#Analysis X-XX:Find the relationship between age and length of service distribution by status

agedata<-employee_data%>%
  select(length_of_service, age,status)

ggplot(data=agedata,aes(x=status, y=age))+
  geom_boxplot(fill="dodgerblue")+
  ggtitle("The relationship between age and length of service by status")+
  stat_summary(fun =mean, geom="point", shape=20, size=14, color="red", fill="red") +
  theme(legend.position="none") +
  theme_classic()+
  theme_economist() +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(family="Tahoma"),
        text = element_text(family = "Tahoma"),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_text(face = "bold", size = 9))


#Analysis X-XX:
department<-c("Meats","Produce","Customer Service")
#data frame for resignation
department_data_rn<-employee_data%>%
  select(department_name, termreason_desc, termtype_desc, status, gender_full, status_year)%>%
  filter(status =="TERMINATED", termreason_desc =="Resignation",department_name %in% department)%>%
  arrange(gender_full, status_year)
department_data_rn

#data frame for layoff
department_data_lf<-employee_data%>%
  select(department_name, termreason_desc, termtype_desc, status, gender_full, status_year)%>%
  filter(status =="TERMINATED", termreason_desc =="Layoff",department_name %in% department)%>%
  arrange(gender_full, status_year)

#data frame for retirement
department_data_rt<-employee_data%>%
  select(department_name, termreason_desc, termtype_desc, status, gender_full, status_year)%>%
  filter(status =="TERMINATED", termreason_desc =="Retirement",department_name %in% department)%>%
  arrange(gender_full, status_year)

g1<-ggplot(data = department_data_lf, aes(x=status_year, fill = department_name, group=department_name ))+
  geom_density(alpha = 0.4)+
  scale_x_continuous(breaks = 2006:2015)+
  facet_wrap(~department_name)+
  theme_ipsum()+
  labs(x = "status year",
       colour = "Department Name",
       title = "The relationship of Density and Status Year by department (Layoff)")+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )

g2<-ggplot(data = department_data_rt, aes(x=status_year, fill= department_name, group=department_name ))+
  geom_density(alpha = 0.4)+
  scale_x_continuous(breaks = 2006:2015)+
  facet_wrap(~department_name)+
  theme_ipsum()+
  labs(x = "status year",
       colour = "Department Name",
       title = "The relationship of Density and Status Year by department (Retirement)")+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )

g3<-ggplot(data = department_data_rn, aes(x=status_year, fill= department_name, group=department_name ))+
  geom_density(alpha = 0.4)+
  facet_wrap(~department_name)+
  theme_ipsum()+
  labs(x = "status year",
       colour = "Department Name",
       title = "The relationship of Density and Status Year by department (Resignation)")+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")
  )

g1
grid.arrange(g2,g3,nrow = 2)



#Find meat cutter stores and the number of terminated employees by status year, by termination reason 
meatdpdatapp<-employee_data%>%
  filter(department_name=="Meats",status == "TERMINATED" )%>%
  count(status_year,department_name,termreason_desc)
meatdpdatapp

meatdp3<-ggplot(data = meatdpdatapp, aes(x=status_year, y=n, shape=termreason_desc, colour = termreason_desc))+
  geom_point(size = 5)+
  labs(x="status year",
       y="Number of Employees")

meatdpdatabar<-employee_data%>%
  filter(department_name=="Meats",status == "TERMINATED" )%>%
  count(city_name,store_name,termreason_desc,status_year,job_title)

meatdp1<-ggplot(meatdpdatabar,aes(x="", y=n, fill=job_title))+
  geom_bar(stat = "Identity",width = 2)+
  coord_polar("y",start = 0)+
  theme_void()+
  labs(fill="Job Title")

meatdp2<-ggplot(meatdpdatabar, aes(x=city_name, fill=job_title))+
  geom_bar(stat = "count")+
  geom_text(aes(label=..count..),stat="count",position = "stack", vjust=1)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(x="City",
       y="Number of Employees",
       label="Job Title")

grid.arrange(meatdp3, meatdp1,meatdp2, meatdp4, nrow=2)  

meatdpdatasc<-employee_data%>%
  select(city_name,store_name,termreason_desc,status_year,job_title, length_of_service,department_name,status)%>%
  filter(department_name=="Meats",status == "TERMINATED")%>%
  arrange(length_of_service)%>%
  count(length_of_service, job_title)


meatdp4<-ggplot(meatdpdatasc,aes(x=as.numeric(length_of_service), y=n, color=job_title))+
  geom_point()+
  geom_text(aes(label=n), vjust=1)+
  geom_line()+
  scale_x_continuous(breaks=3:25)+
  labs(x="Length Of Service",
       y="Number of Employees")+
  theme_classic()


  











