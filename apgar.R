#Dataset Load
library(readr)
Dataset_Final <- read_csv("C:/Users/Diego/Desktop/Dataset_Final.csv", col_names = FALSE)
Dataset_Final <- Dataset_Final[-(1),,drop=FALSE]

#Individual Analysis
m = mean(as.numeric(Dataset_Final$X4))
d = sd(as.numeric(Dataset_Final$X4))
data = Dataset_Final
data <- data[,-(1:7),drop=FALSE]
data <- lapply(data, as.numeric)
data <- as.data.frame(data)

# Questions weight
Q1 = rep.int(0, dim(data)[1])
Q2 = rep.int(0, dim(data)[1])
Q3 = rep.int(0, dim(data)[1])
Q4 = rep.int(0, dim(data)[1])
Q5 = rep.int(0, dim(data)[1])

for (i in 1:5) 
{
  Q1 = Q1+data[,i]*(i-1)
  Q2 = Q2+data[,5+i]*(i-1)
  Q3 = Q3+data[,10+i]*(i-1)
  Q4 = Q4+data[,15+i]*(i-1)
  Q5 = Q5+data[,20+i]*(i-1)
}
mQ1 = mean(Q1)
mQ2 = mean(Q2)
mQ3 = mean(Q3)
mQ4 = mean(Q4)
mQ5 = mean(Q5)
Qt = Q1+Q2+Q3+Q4+Q5
mQt = mean(Qt)
s = table(Qt) 
N = sum((Qt >= 17)*1)
med_N = sum(((Qt >= 17)*Qt))/N
L = sum((Qt >= 13 & Qt <= 16)*1)
med_L = sum(((Qt >= 13 & Qt <= 16)*Qt))/L
M = sum((Qt >= 10 & Qt <= 12)*1)
med_M = sum(((Qt >= 10 & Qt <= 12)*Qt))/M
S = sum((Qt <= 9)*1)
med_S = sum(((Qt <= 9)*Qt))/S

#Familiar Analysis
members = as.numeric(Dataset_Final$X3)
fam_med = mean(table(members))
apgar_fam = c(mean(Qt[1:2]),mean(Qt[3:4]),mean(Qt[5:6]),mean(Qt[7]),mean(Qt[8]),mean(Qt[9]),
              mean(Qt[10:13]),mean(Qt[14:18]),mean(Qt[19:20]),mean(Qt[21:22]),mean(Qt[23]),
              mean(Qt[24]),mean(Qt[25]),mean(Qt[26:27]),mean(Qt[28:31]),mean(Qt[32:34]),
              mean(Qt[35:38]),mean(Qt[39:40]),mean(Qt[41:42]),mean(Qt[43:45]),mean(Qt[46:47]),
              mean(Qt[48]),mean(Qt[49]),mean(Qt[50]),mean(Qt[51:52]),mean(Qt[53:54]),mean(Qt[55]),
              mean(Qt[56]),mean(Qt[57:58]),mean(Qt[59:60]),mean(Qt[61:63]),mean(Qt[64:65]),mean(Qt[66:67]),
              mean(Qt[68:71]),mean(Qt[72:73]),mean(Qt[74:75]),mean(Qt[76:77]))
r_a = range(apgar_fam)
med_r_a =mean(r_a)
round_apgar = round(apgar_fam)
N1 = sum((round_apgar >= 17)*1)
med_N1 = sum(((round_apgar >= 17)*round_apgar))/N1
L1 = sum((round_apgar >= 13 & round_apgar <= 16)*1)
med_L1 = sum(((round_apgar >= 13 & round_apgar <= 16)*round_apgar))/L1
M1 = sum((round_apgar >= 10 & round_apgar <= 12)*1)
med_M1 = sum(((round_apgar >= 10 & round_apgar <= 12)*round_apgar))/M1
S1 = sum((round_apgar <= 9)*1)
med_S1 = sum(((round_apgar <= 9)*round_apgar))/S1

### Plot percentage
library(ggplot2)
cl = c('N','M','O','S')
Type = c('Individual','Individual','Individual','Individual','Familiar','Familiar','Familiar','Familiar')
per1 = c(N,L,M,S)/length(Qt)*100
per2 = c(N1,L1,M1,S1)/length(round_apgar)*100
Taxonomy = c(cl,cl)
Percentage = c(per1,per2)
datf <- data.frame(Percentage, Taxonomy, Type)
Taxonomy2 = factor(datf$Taxonomy, cl)

ggplot(data=datf, aes(x=Taxonomy2, y=Percentage, fill=Type)) + 
  labs(y="Percentage", x = "Taxonomy") + geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()


# ANOVA
Ni = ((Qt >= 17)*1)
Mi = ((Qt >= 13 & Qt <= 16)*2)
Oi = ((Qt >= 10 & Qt <= 12)*3)
Si = ((Qt <= 9)*4)
Ti = Ni+Mi+Oi+Si
Nf = ((round_apgar >= 17)*5)
Mf = ((round_apgar >= 13 & round_apgar <= 16)*6)
Of = ((round_apgar >= 10 & round_apgar <= 12)*7)
Sf = ((round_apgar <= 9)*8)
Tf = Nf+Mf+Of+Sf
vec1 = c(Qt,round_apgar)
Class = c(Ti,Tf)
df <- data.frame(vec1, Class)

library(dplyr)
group_by(df, Class) %>%
  summarise(
    count = n(),
    mean = mean(vec1, na.rm = TRUE),
    sd = sd(vec1, na.rm = TRUE)
  )

#install.packages("ggpubr")
library("ggpubr")
levels(df$Class) <- c("Ni","Mi","Oi","Si","Nf","Mf","Of","Sf")
ggboxplot(df, x = "Class", y = "vec1", 
          color = "Class", palette = c("#00AFBB", "#E7B800", "#FC4E07","#D82E2E", "#00AFBB", "#E7B800", "#FC4E07","#D82E2E"),
          ylab = "APGAR Test Evaluation", xlab = "Dysfunctionality Degree", add = "jitter")
df[, 'Class'] <- as.factor(df[, 'Class'])
res.aov <- aov(vec1 ~ Class, data = df)
summary(res.aov)
#install.packages("multcomp")
library(multcomp)
summary(glht(res.aov, linfct = mcp(Class = "Tukey")))


## Time Analysis
Q2018 = Qt[1:23]
summary(Q2018)
Q2019 = Qt[24:50]
summary(Q2019)
Q2020 = Qt[51:77]
summary(Q2020)
