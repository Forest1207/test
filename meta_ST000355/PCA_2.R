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
library(tidyverse)
library(vegan)
library(ggh4x)
library(ggrepel)
path=dirname(this.path()) #获取当前目录名
setwd(path)               #设置工作目录
list.files()  #查看当前工作目录下的文件
###
load("data.Rdata")
load("group.Rdata")
asv = clean_data
group$g <- "Control"  # 添加分组信息
group$g[group$Group == 1] <- "Breast cancer"
group = as.data.frame(cbind(sample = row.names(group),group = group$g))
#########执行PCA##############
PCA<-prcomp(t(asv),scal=TRUE)
# 主成分的标准差
PCA$sdev
# 载荷矩阵，包含每个主成分的特征向量
PCA$rotation
# 用于中心化的均值向量
PCA$center
# 用于标准化的标准差向量
PCA$scale
PCA$x
# 统计摘要
PCA_sum<-summary(PCA)
PCA_sum
#提取出PC1及PC2的坐标
PC12<-PCA_sum$x[,1:2]
#计算各主成分解释度
pc<-PCA_sum$importance[2,]*100
#将绘图数据和分组合并
PC_bac<-as.data.frame(PC12)%>% 
      rownames_to_column(var='sample')%>%# 行名转列名，与group分组文件根据smple列进行左连接（将PC12的全部内容保留，添加group的信息）
      left_join(.,group,by='sample')
##################定制颜色映射及主题##################
#定制后面绘图中映射的颜色
color_1<-c("#F5AB5E","#947959","#5F97C6","#F09496","#D2D2D2","#9C9BE9","#AFC778","#EFDBB9","#A3CDEA","#F4DFDD")
#定制后面绘图中的主题
Classic<-theme_bw(base_size=15)+ theme(panel.grid.major=element_blank(), # 移除主网格线
                                       panel.grid.minor=element_blank(), # 移除次网格线
                                       axis.text.x=element_text(size=16,color="black"),
                                       axis.text.y=element_text(size=16,color="black"),
                                       axis.title.y=element_text(size=18,color="black"),
                                       axis.title.x=element_text(size=18,color="black"),
                                       axis.ticks.length=unit(-0.2, "cm"), # 设置坐标轴刻度线的长度
                                       # legend.position='none',
                                       legend.position=c(0.001,0.999),
                                       legend.title=element_blank(), # 移除图例标题
                                       legend.key=element_blank(),  # 图例键为空
                                       legend.text=element_text(color="black",size=9,face="bold"), # 定义图例文本
                                       legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
                                       legend.key.width=unit(0.5,'cm'), # 定义图例水平大小
                                       legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
                                       legend.background=element_blank(), # 设置背景为空
                                       legend.box.background=element_rect(colour="black"), # 图例绘制边框
                                       legend.justification=c(0.0001,1))
############## 基础绘图+添加置信圆 ###################
P1<-ggplot(data=PC_bac, aes(x=PC1, y=PC2, color=group))+ 
  geom_point(size=4,alpha=0.8,stroke=3,shape=16)+
  # 绘制点图并设定大小 
  labs(x=paste0("PC1 (",pc[1],"%)"),
       y=paste0("PC2 (",pc[2],"%)"))+# 将x、y轴标题改为贡献度
  scale_color_manual(values=color_1)+# 点的颜色设置
  scale_fill_manual(values=color_1)+
  scale_shape_manual(values=c(10:19))+
  Classic
#ggsave("PCA.pdf", plot=P1, width=10, height=8)
#ggsave("PCA2.PNG", plot=P1, width=10, height=8)
P2<-P1+ # 添加置信圆
  stat_ellipse(data=PC_bac, geom="polygon",
               # 定义置信水平、线性
               level=0.95, linetype=2,
               # 定义粗细，透明度
               size=0.5, alpha=0.1,aes(fill=group))
####################### 添加几何图形+质心 ##########################
P3<-P1+geom_point(size=6,alpha=0.5)+ 
  geom_polygon(aes(x=PC1, y=PC2, fill=group, color=group),alpha=0.2,linetype=1,linewidth=1.5)
P4<-P1+# 计算几何体的质心位置，即几何体的“重心”
  stat_midpoint(aes(xend=PC1, yend=PC2, colour=group),
                geom="segment", crop_other=F,alpha=0.3, size=3)
# 添加密度曲线
library(ggExtra)
P5 <- ggMarginal(P2,type=c('density'), margins='both',size=3.5, groupColour=F, groupFill=T)

