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
L = sum((Qt >= 13 & Qt <= 16)*1)
M = sum((Qt >= 10 & Qt <= 12)*1)
S = sum((Qt <= 9)*1)



if(Qt >= 17) {
  N = sum 
}

