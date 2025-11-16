rm(list=ls())  #清空环境内变量
options(stringsAsFactors = F)  #避免自动将字符串转换为R语言因子
# 加载必要的包
library(stringr)
library(patchwork)
library(plyr)
library(dplyr)
library(this.path)
library(readxl)
path=dirname(this.path()) #获取当前目录名
setwd(path)               #设置工作目录
list.files()  #查看当前工作目录下的文件
data = readxl::read_xlsx('COH plasma data.xlsx')
clean_data = data[17:245,]
clean_data = cbind(Name = clean_data$Name,clean_data[,9:222])
clean_data = na.omit(clean_data)
NAME = clean_data$Name
clean_data = clean_data[,-1]
clean_data <- as.data.frame(lapply(clean_data, as.numeric))
row.names(clean_data) = NAME
group = data.frame(row.names = colnames(clean_data),
                   Group = t(na.omit(data[data$No == "Group (1/0)",9:222])))
group$Group =as.factor(group$Group)
save(clean_data,file = "data.Rdata")
save(group,file = "group.Rdata")

