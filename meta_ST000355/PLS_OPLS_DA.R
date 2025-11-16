rm(list=ls())  #清空环境内变量
options(stringsAsFactors = F)  #避免自动将字符串转换为R语言因子
# 加载必要的包
library(stringr)
library(patchwork)
library(plyr)
library(dplyr)
library(this.path)
library(readxl)
library(ggplot2)
library(ropls)#用于PLS/OPLS-DA
path=dirname(this.path()) #获取当前目录名
setwd(path)               #设置工作目录
list.files()  #查看当前工作目录下的文件
###
load("data.Rdata")
load("group.Rdata")
group$g <- "Control"  # 添加分组信息
group$g[group$Group == 1] <- "Breast cancer"
Group = group$g
###PCA
#data(sacurine)示例数据
#dataMatrix<-sacurine$dataMatrix
data.pca <- opls(t(clean_data))
###PLS-DA
data.plsda <- opls(t(clean_data), Group)
plot(data.plsda,
     typeVc= "x-score",
     parAsColFcVn= Group,
     parPaletteVc=c("#F5AB5E", "#5F97C6"))
###OPLS-DA
data.oplsda <- opls(t(clean_data), Group,predI = 1, orthoI = NA)
#提取VIP值；
VIP <- data.oplsda@vipVn
#将VIP值合并到丰度矩阵中；
df_withVIP <- data.frame(clean_data,VIP)
save(df_withVIP,file = "data_VIP.Rdata")
