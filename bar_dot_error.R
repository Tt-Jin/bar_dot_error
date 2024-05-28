library(ggplot2)
library(ggsignif)
library(dplyr)

# 清空环境变量
rm(list=ls())
# 设置你的工作目录
setwd(".")

# step1 花的种类和花瓣长度为例
data <- iris %>% select(Species, Sepal.Length) 

# 计算平均值、标准差、标准误
stat_data <- data %>%
  group_by(Species) %>%
  summarise( 
    n=n(),
    mean=mean(Sepal.Length),
    sd=sd(Sepal.Length),
    se=sd(Sepal.Length)/sqrt(n)
  )

# step2 指定分组进行比较
comparisons <- list(c("setosa", "versicolor"), 
                    c("versicolor", "virginica"), 
                    c("setosa", "virginica"))

# step3 指定检验方法，支持t.test, wilcox.test等
test <- "t.test"

# step 绘图
#mycol = c("#4E4E56", "#DA635D", "#B1938B")
mycol = c("#000000", "#339966", "#339966")
p <- ggplot() +
  geom_bar(data = stat_data, 
           aes(x = Species, y = mean),
           fill = "white", size = 1, color = mycol,
           position = "dodge",
           stat = "identity",
           width = 0.6) +
  geom_errorbar(data = stat_data, 
                aes(x=Species, y=NULL, ymin=mean-se, ymax=mean+se), 
                position = "dodge", 
                width=0.2, 
                color = mycol,
                size=1) + 
  geom_jitter(data = data, 
              aes(x = Species, y = Sepal.Length, fill = Species, color = Species),
              size = 1.2, 
              width = 0.3) + 
  scale_color_manual(values = mycol) + 
  geom_signif(data = data, mapping = aes(x = Species, y = Sepal.Length),
              comparisons = comparisons,
              test = test, 
              map_signif_level = TRUE, 
              y_position = c(8,8.5,9),
              tip_length = c(0,0,0,0,0,0),
              textsize = 5, 
              step_increase = 0.2) + # 避免绘制显著性结果时发生重合
  scale_y_continuous(limits = c(0, 12), expand = c(0,0)) + 
  theme_classic(base_line_size = 0.7) +
  labs(title = "", x = "", y = "Mean Length (cm)") +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13,
                                   colour = "black",
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   angle = 45),
        axis.text.y = element_text(size = 13,
                                   colour = "black",
                                   vjust = 0.5,
                                   hjust = 0.5),
        axis.ticks.length.x = unit(0.2,'cm'),
        axis.ticks.length.y = unit(0.2,'cm')
  )
print(p)

