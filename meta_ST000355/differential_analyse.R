rm(list=ls())  #清空环境内变量
options(stringsAsFactors = F)  #避免自动将字符串转换为R语言因子
# 加载必要的R包
library(tidyverse)  # 用于数据处理和转换
library(stringr)
library(patchwork)
library(plyr)
library(dplyr)
library(this.path)
library(readxl)
library(ggplot2)
path=dirname(this.path()) #获取当前目录名
setwd(path)               #设置工作目录
list.files()  #查看当前工作目录下的文件
###
load("data.Rdata")
load("group.Rdata")
#group$g <- "Control"  # 添加分组信息
#group$g[group$Group == 1] <- "Breast cancer"
# 转换分组信息为数据框（样本列+分组列）
group_df <- group %>%
  rownames_to_column("sample") %>%  # 将行名（样本）转换为列
  rename(group = 2)  # 重命名分组列为"group"
# 提取对照组和乳腺癌组的样本名
ctrl_samples <- group_df %>% filter(group == 0) %>% pull(sample)  # 对照组样本
case_samples <- group_df %>% filter(group == 1) %>% pull(sample)  # 乳腺癌组样本
#统计分析函数（t检验+差异倍数）
calc_metab_stats <- function(metab_row) {
  # 提取当前代谢物在两组中的值
  ctrl_vals <- metab_row[ctrl_samples]  # 对照组数值
  case_vals <- metab_row[case_samples]  # 乳腺癌组数值
  
  # 计算两组均值
  ctrl_mean <- mean(ctrl_vals, na.rm = TRUE)  # 对照组均值
  case_mean <- mean(case_vals, na.rm = TRUE)  # 乳腺癌组均值
  
  # 计算差异倍数（FD = 乳腺癌组均值 / 对照组均值）
  fd <- case_mean / ctrl_mean
  
  # 进行t检验（Welch's t检验，默认不等方差）
  t_test <- t.test(case_vals, ctrl_vals, na.rm = TRUE)
  p_value <- t_test$p.value  # 提取P值
  
  # 返回结果
  return(data.frame(p_value = p_value, fd = fd, stringsAsFactors = FALSE))
}
#批量分析所有代谢物
# 对代谢物数据的每一行（每个代谢物）应用统计函数
data_result = data.frame(row.names = row.names(clean_data))
data_result$p_value = 0
data_result$log2FC = 0
result <- unlist(apply(clean_data, 1, calc_metab_stats))
j = 1
for(i in row.names(clean_data)){
  data_result$p_value[row.names(data_result) == i] = result[j]
  j = j+1
  data_result$log2FC[row.names(data_result) == i] = log2(result[j])
  j = j+1
}
#FDR多重检验矫正
data_result <- data_result %>%
  mutate(fdr = p.adjust(p_value, method = "fdr"))  # 计算FDR
###添加VIP
load('data_VIP.Rdata')
data_result$VIP = df_withVIP$VIP
####差异代谢物(fdr<0.05,log2FC>1/log2FC<-1,VIP>1)筛选
FC = 2
df = data_result
df$group <- "Non-significant"
#p值筛选
#df$group[df$log2FC < 0 & df$p_value < 0.05] <- "Significant"
#df$group[df$log2FC > 0 & df$p_value < 0.05] <- "Significant"
#df$group[df$log2FC < -log2(FC) & df$p_value < 0.05] <- "Significant Down"
#df$group[df$log2FC > log2(FC) & df$p_value < 0.05] <- "Significant Up"
#修正后p值筛选
df$group[df$log2FC < 0 & df$fdr < 0.05] <- "Significant"
df$group[df$log2FC > 0 & df$fdr < 0.05] <- "Significant"
df$group[df$log2FC < -log2(FC) & df$fdr < 0.05] <- "Significant Down"
df$group[df$log2FC > log2(FC) & df$fdr < 0.05] <- "Significant Up"
df$group <- factor(df$group,
                   levels = c("Significant Up", "Significant Down", "Significant","Non-significant"))
#查看分布
table(df$group)
###
Qvalue <- 0.05
df_up <- subset(df, fdr < Qvalue & df$log2FC > log2(FC))
df_up <- head(df_up[order(df_up$log2FC, decreasing = T), ], 10)
df_down <- subset(df, fdr < Qvalue & df$log2FC < -log2(FC))
df_down <- head(df_down[order(df_down$log2FC, decreasing = F), ], 10)
rownames(df)<-as.character(rownames(df))
df_label<-df[c(rownames(df_down),row.names(df_up)),]
df_label$label <-as.character(rownames(df_label))
# 设置x轴的范围
xmax <- round(max(abs(df$log2FC)) * 1.3 + 0.5)
#火山图绘制
p <- ggplot(
  df, aes(x = log2FC, y = -log10(fdr), colour = group)
) +
  xlim(c(-xmax, xmax)) +
  geom_point( alpha=.7, size = 2, fill = "transparent") 
p
#调整分类散点的颜色、字体大小
p <- ggplot(
  df, aes(x = log2FC, y = -log10(fdr), colour = group)
) +
  xlim(c(-xmax, xmax)) +
  geom_point(aes(size = abs(log2FC), alpha = abs(log2FC),shape = factor(VIP > 1, levels = c(FALSE, TRUE), labels = c("VIP≤1", "VIP>1"))
  ), fill = "transparent")+
  # 调整散点的颜色
  scale_color_manual(
    values = c("#A13336","#128FCB","#d2daf9","#d2dae2"
    )[levels(df$group) %in% unique(df$group)]
  ) +
  # 添加qvalue和FC阈值的边界线
  geom_vline(
    xintercept = c(-log2(FC), log2(FC)), lty = 2, col = "grey",
    lwd = 0.4
  ) +
  geom_hline(yintercept = -log10(Qvalue),lty = 2, col = "grey", lwd = 0.4
  ) +
  # 设置xy轴以及图片的标题
  labs(
    x = expression(paste(log[2], "FoldChange")),
    y = expression(paste("-", log[10], "fdr"))
  ) +
  labs( title = "Volcano_plot"
  ) +
  theme_bw() +
  # 细节调整
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),# 标题的位置日调整
    legend.position = "right", # 图例的位置
    legend.title = element_blank(), # 图例背景色
    panel.grid = element_blank(), # 图片背景色调为空白
    legend.key.height = unit(0.5, "cm"),
    legend.text = element_text(size = 10), # 设置图例字体大小
    panel.border = element_rect(color = "black", size = 1, fill = NA),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15)
  )+
  theme(
    legend.text = element_text(color = 'black',size = 15))+
  labs(color = "")+
  theme(legend.text.align = 0)+
  guides(color = guide_legend(override.aes = aes(size = 3)))
p
#给火山图添加FC、q值的边界线，差异上下调数量以及标签
p <- ggplot(
  df, aes(x = log2FC, y = -log10(fdr), colour = group)
) +
  xlim(c(-xmax, xmax)) +
  #geom_point(alpha = 0.8, size = 1) +
  geom_point( aes(size = abs(log2FC), alpha = abs(log2FC),shape = factor(VIP > 1, levels = c(FALSE, TRUE), labels = c("VIP≤1", "VIP>1"))
  ), fill = "transparent")+
  # 调整散点的颜色
  scale_color_manual(
    values = c("#A13336","#128FCB","#d2daf9","#d2dae2"
    )[levels(df$group) %in% unique(df$group)]
  ) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  # 添加qvalue和FC阈值的边界线
  geom_vline(
    xintercept = c(-log2(FC), log2(FC)), lty = 2, col = "grey",
    lwd = 0.4
  ) +
  geom_hline(yintercept = -log10(Qvalue),lty = 2, col = "grey", lwd = 0.4
  ) +
  # 设置xy轴以及图片的标题
  labs(
    x = expression(paste(log[2], "FoldChange")),
    y = expression(paste("-", log[10], "fdr"))
  ) +
  labs( title = "Volcano_plot"
  ) +
  theme_bw() +
  # 细节调整
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),# 标题的位置日调整
    legend.position = "right", # 图例的位置
    legend.title = element_blank(), # 图例背景色
    panel.grid = element_blank(), # 图片背景色调为空白
    legend.key.height = unit(0.5, "cm"),
    legend.text = element_text(size = 10), # 设置图例字体大小
    panel.border = element_rect(color = "black", size = 1, fill = NA),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15)
  )+
  theme(
    legend.text = element_text(color = 'black',size = 15))+
  labs(color = "")+
  theme(legend.text.align = 0)+
  guides(color = guide_legend(override.aes = aes(size = 3)))


ycoord <- min(-log10(df$fdr)) +
  diff(
    range(
      -log10(df$fdr)[!is.infinite(-log10(df$fdr))]
    )
  ) * .97 * .95

count_info <- data.frame(
  x = c(-xmax, xmax * 0.6),
  y = c(
    ycoord,
    ycoord
  ),
  # 添加差异信息的标签
  label = c(
    paste0(
      "Down\nfdr <", Qvalue, ": ",
      nrow(subset(df, fdr < Qvalue & df$log2FC < 0
      )
      ),
      "\nSig. : ",
      nrow(subset(df, fdr < Qvalue & df$log2FC < -log2(FC)
      )
      )
    ),
    paste0(
      "Up\nfdr <", Qvalue, ": ",
      nrow(subset(df,fdr < Qvalue & df$log2FC > 0
      )
      ),
      "\nSig. : ",
      nrow(subset(df,fdr < Qvalue & df$log2FC > log2(FC)
      )
      )
    )
  )
)

p <- p + geom_text(
  parse = F,
  data = count_info,
  color = "black",
  size = 3.6,
  hjust = 0,
  aes(x = x, y = y, label = label)
)


# 添加需要标记的基因信息并做细节调整

if (nrow(df_label) > 0) {
  p <- p + geom_label_repel(
    data = df_label,
    aes(label = label),
    color = "black",
    #geom = "segment",
    label.size = .9,
    size = 4,
    segment.size=1,
    box.padding = .6,
    max.overlaps = Inf,
    min.segment.length = 0.25,
    segment.alpha = .4,
    show.legend = FALSE
  )
}
p
#结果保存
#检测全代谢物（背景文件）
write.table(rownames(df),file = "result/all_metabolite_name.txt",row.names=FALSE,col.names = FALSE,quote = FALSE)
#差异代谢物
write.table(rownames(df_label),file = "result/significant_metabolite_name.txt",row.names=FALSE,col.names = FALSE,quote = FALSE)
#全代谢物
data_name_output = paste("result/","all_data", as.character(nrow(df)), ".txt", sep="")
write.table(df,file = data_name_output,sep = '\t',quote = F)
#差异代谢物
data_name_output = paste("result/","significant_all_data", as.character(nrow(df[df$fdr < Qvalue,])), ".txt", sep="")
write.table(df[df$fdr < Qvalue,],file = data_name_output,sep = '\t',quote = F)
#显著上调代谢物
data_name_output = paste("result/","significant_up", as.character(nrow(df[df$group=="Significant Up",])), ".txt", sep="")
write.table(df[df$group=="Significant Up",],file = data_name_output,sep = '\t',quote = F)
#显著下调代谢物
data_name_output = paste("result/","significant_down", as.character(nrow(df[df$group=="Significant Down",])), ".txt", sep="")
write.table(df[df$group=="Significant Down",],file = data_name_output,sep = '\t',quote = F)
