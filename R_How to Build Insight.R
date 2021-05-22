# Import insurance_loss data
Insurance=read.csv('D:/Bootcamp Data Science/Dibimbing/file/Insurance_Loss.csv')
head(Insurance)

# check dimension and columns name of data
dim(Insurance)
names(Insurance)

# load packages
library(ggplot2)
library(dplyr)

### 1. Outliers data
# IQR and outliers 
# IQR and outliers
Q1 = quantile(Insurance$Losses, 0.25, na.rm = TRUE)
Q3 = quantile(Insurance$Losses, 0.75, na.rm = TRUE)
IQR = Q3-Q1
Lower_Whisker = Q1-(1.5*IQR)
Upper_Whisker = Q3+(1.5*IQR)
ggplot(Insurance, aes(x=Policy.Number,y=Losses,color = factor(cut(Losses, c(-Inf,Lower_Whisker,Upper_Whisker,Inf))))) +
  geom_point(color=ifelse(Insurance$Losses >= Lower_Whisker & Insurance$Losses <= Upper_Whisker, "black", "red")) + 
  ggtitle("Outliers Data") + geom_hline(yintercept=Upper_Whisker, linetype="dashed", color = "blue") + theme(plot.title = element_text(hjust = 0.5))

#Check outliers data with Multiple Boxplot
boxplot(Insurance$Years.of.Driving.Experience, Insurance$Number.of.Vehicles, 
        Insurance$Vehicle.Age, Insurance$Losses,
        main = "Multiple boxplots for comparision",
        data = Insurance,
        names = c("Years.of.Driving.Experience", "Number.of.Vehicles", "Vehicle.Age", "Losses"),
        col = "yellow",
        border = "brown"
)

# Check missing data
for(i in names(Insurance)){
  print(paste(i,':', sum(is.na(Insurance$i))))
}

### 2. Age vs Losses

df_age = Insurance %>%
  group_by(Age) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses))
ggplot(data=df_age,aes(x=Age,y=Average_Losses, color = Frequency)) + 
  geom_point() + ggtitle("Age vs Average Losses") + theme(plot.title = element_text(hjust = 0.5))

df_age1=Insurance %>%
  group_by(Age) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses), Total_Losses = mean(Losses)*n())
ggplot(data=df_age1,aes(x=Age,y=Total_Losses, color = Frequency)) + geom_point() + 
  ggtitle("Age vs Total Losses") + theme(plot.title = element_text(hjust = 0.5))

### 3. Years of Driving Experience vs Losses

df_experience=Insurance %>%
  group_by(Years.of.Driving.Experience) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses)) 
ggplot(data=df_experience,aes(x=Years.of.Driving.Experience,y=Average_Losses, color = Frequency)) + 
  geom_point() + ggtitle("Years of Driving Experience vs Average Losses") + theme(plot.title = element_text(hjust = 0.5))

df_experience1=Insurance %>%
  group_by(Years.of.Driving.Experience) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses), Total_Losses = mean(Losses)*n())
ggplot(data=df_experience1,aes(x=Years.of.Driving.Experience,y=Total_Losses, color = Frequency)) + geom_point() + 
  ggtitle("Years of Driving Experience vs Total Losses") + theme(plot.title = element_text(hjust = 0.5))

### 4. Marital Status vs Losses

df_status=Insurance %>%
  group_by(Married) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses))
ggplot(data=df_status, aes(x=Married, y=Average_Losses, fill = Frequency)) +
  geom_bar(stat="identity") + ggtitle("Marital Status vs Average Losses") + theme(plot.title = element_text(hjust = 0.5))

df_status=Insurance %>%
  group_by(Married) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses), Total_Losses = mean(Losses)*n())
ggplot(data=df_status, aes(x=Married, y=Total_Losses, fill = Frequency)) +
  geom_bar(stat="identity") + ggtitle("Marital Status vs Total Losses") + theme(plot.title = element_text(hjust = 0.5))

### 5. Gender vs Losses

df_gender=Insurance %>%
  group_by(Gender) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses))
ggplot(data=df_gender, aes(x=Gender, y=Average_Losses, fill = Frequency)) +
  geom_bar(stat="identity") + ggtitle("Gender vs Average Losses") + theme(plot.title = element_text(hjust = 0.5))

df_gender=Insurance %>%
  group_by(Gender) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses), Total_Losses = mean(Losses)*n())
ggplot(data=df_gender, aes(x=Gender, y=Total_Losses, fill = Frequency)) +
  geom_bar(stat="identity") + ggtitle("Gender vs Total Losses") + theme(plot.title = element_text(hjust = 0.5))

### 6. Number of Vehicles vs Losses

df_number=Insurance %>%
  group_by(Number.of.Vehicles) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses))
ggplot(data=df_number, aes(x=Number.of.Vehicles, y=Average_Losses, fill = Frequency)) + 
  geom_bar(stat="identity") + ggtitle("Number of Vehicles vs Average Losses") + theme(plot.title = element_text(hjust = 0.5))

df_number=Insurance %>%
  group_by(Number.of.Vehicles) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses), Total_Losses = mean(Losses)*n())
ggplot(data=df_number, aes(x=Number.of.Vehicles, y=Total_Losses, fill = Frequency)) + geom_bar(stat="identity") + 
  ggtitle("Number of Vehicles vs Total Losses") + theme(plot.title = element_text(hjust = 0.5))

### 7. vehicle Age vs Losses

df_vg=Insurance %>%
  group_by(Vehicle.Age) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses))
ggplot(data=df_vg, aes(x=Vehicle.Age, y=Average_Losses, fill = Frequency)) +
  geom_bar(stat="identity") + ggtitle("Vehicle Age vs Average Losses") + 
  theme(plot.title = element_text(hjust = 0.5))

df_vg=Insurance %>%
  group_by(Vehicle.Age) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Average = mean(Losses), Frequency = n(), Total_Losses = mean(Losses)*n())
ggplot(data=df_vg, aes(x=Vehicle.Age, y=Total_Losses, fill = Frequency)) +
  geom_bar(stat="identity") + ggtitle("Vehicle Age vd Total Losses") + 
  theme(plot.title = element_text(hjust = 0.5))

### 8. Fuel Type vs Losses

df_FT=Insurance %>%
  group_by(Fuel.Type) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Frequency = n(), Average_Losses = mean(Losses)) 
ggplot(data=df_FT, aes(x=Fuel.Type, y=Average_Losses, fill = Frequency)) +
  geom_bar(stat="identity") + ggtitle("Fuel Type vs Average Losses") + 
  theme(plot.title = element_text(hjust = 0.5))

df_FT=Insurance %>%
  group_by(Fuel.Type) %>%
  filter(Losses>mean(Losses)) %>%
  summarise(Average = mean(Losses), Frequency = n(), Total_Losses = mean(Losses)*n()) 
ggplot(data=df_FT, aes(x=Fuel.Type, y=Total_Losses, fill = Frequency)) +
  geom_bar(stat="identity") + ggtitle("Fuel Type vs Total Losses") + 
  theme(plot.title = element_text(hjust = 0.5))
