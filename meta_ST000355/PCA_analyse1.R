rm(list=ls())  #清空环境内变量
options(stringsAsFactors = F)  #避免自动将字符串转换为R语言因子
# 加载必要的包
library(stringr)
library(patchwork)
library(plyr)
library(dplyr)
library(this.path)
library(readxl)
library(FactoMineR)
library(factoextra)
library(ggplot2)
path=dirname(this.path()) #获取当前目录名
setwd(path)               #设置工作目录
list.files()  #查看当前工作目录下的文件
###
load("data.Rdata")
load("group.Rdata")
#pca <- prcomp(t(clean_data), scale. = FALSE, center = TRUE)  #行=代谢物，列=样本
#pca_scores <- as.data.frame(pca$x[, 1:5])  # 取前5个主成分
group$g <- "Control"  # 添加分组信息
group$g[group$Group == 1] <- "Breast cancer"
# 计算主成分解释率
#explained_var <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
#PCA绘图
dat.pca1 <- PCA(t(clean_data), graph = FALSE)
p.ca1 <- fviz_pca_ind(dat.pca1,#dat.pca 为PCA对象
                      geom.ind = c("point",'text'),#"point"、"text"、c("point","text")
                      label = "ind",     # 显示所有标签
                      repel = TRUE,      # 避免标签重叠
                      
                      alpha.ind = 1,     # 点透明度0~1
                      shape.ind = 16,    # 点的形状
                      pointsize = 1,     # 点的大小
                      #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      
                      col.ind = group$g,         # 按种类分组
                      palette = "jco",   #分组颜色
                      
                      addEllipses = TRUE,          #添加置信椭圆
                      #ellipse.type = "confidence", #置信椭圆类型
                      #ellipse.level = 0.05,        #置信水平
                      
                      legend.title = "Batch") +
  theme(plot.title = element_blank()) # 移除标题
print(p.ca1)

