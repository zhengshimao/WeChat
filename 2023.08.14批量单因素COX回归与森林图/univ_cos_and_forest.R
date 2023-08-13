rm(list = ls())

library(survival)
library(dplyr)
library(plyr)
library(openxlsx)


library(ggplot2)
library(ggtext)
library(aplot)
library(tidyr)
library(patchwork)

df <- lung
#假设我们要对如下5个特征做单因素cox回归分析
covariates <- c("age", "sex",  "ph.ecog","ph.karno", "pat.karno","meal.cal" ,"wt.loss")
#分别对每一个变量，构建生存分析的公式
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))


#循环对每一个特征做cox回归分析
univ_models <- lapply( univ_formulas, function(x){coxph(x, data = lung)})

#提取HR，95%置信区间和p值
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         #获取p值
                         P_value<-signif(x$wald["pvalue"], digits=2)
                         #获取HR
                         HR <-signif(x$coef[2], digits=2);
                         #获取95%置信区间
                         HR_confint_lower_95 <- signif(x$conf.int[,"lower .95"], 2)
                         HR_confint_upper_95 <- signif(x$conf.int[,"upper .95"],2)
                         HR_95CI <- paste0(HR, " (", 
                                           HR_confint_lower_95, "-", HR_confint_upper_95, ")")
                         res <- data.frame("HR_confint_lower_95" = HR_confint_lower_95,
                                           "HR_confint_upper_95" = HR_confint_upper_95,
                                           "HR" = HR,
                                            "HR_95CI" = HR_95CI,
                                           "P_value" = P_value)
                         return(res)
                       })
#转换成数据框
univ_cox <- ldply(univ_results,data.frame)
colnames(univ_cox)[1] <- "Clinical_factors"

write.xlsx(univ_cox, file = "univ_cos.xlsx")

#######################################################################

# 参数准备----
df <- univ_cox #数据
tab_header_bold = TRUE #是否给表格header行加粗
line_width <- 0.5 # 表格水平线和误差棒图的x轴线宽度
error_bar_height <- 0.35 # 误差线两侧高度
ref_line <- 1 # 无效线
plot_title <- "Univarite"
# 准备图中表格的表头内容list # 只修改右侧将要展示的内容
tab_header = list(Clinical_factors = "Clinical factors", 
                  HR_95CI = "Hazard ratio (95% CI)", 
                  P_value = "P value")

# 准备数据----
## 将表格的列名和第一列均拷贝一份儿放到数据表中
tmp_df <- df %>% select(Clinical_factors, HR_95CI, P_value) %>%  
  #rbind(colnames(.),.) %>%  # df[1:nrow(df),]
  rbind(tab_header,.) %>%
  cbind("clinical" = .[,"Clinical_factors"],.)
tmp_df$clinical <- factor(tmp_df$clinical, levels = rev(tmp_df$clinical))

## 宽表格转长表格
tmp_df <- tmp_df %>% pivot_longer(cols = 2:ncol(.), names_to = "x", values_to = "label")

## 表格header内容替换与是否加粗。# table header and bold labels
###部分label加粗，即表格header加粗。使用geom_richtext实现加粗。
tmp_df$label_bold <- sapply(tmp_df$label, function(x){
  if(x %in% unlist(tab_header)){
    if(tab_header_bold){ # 是否给表格header行加粗
      paste0("<b>",x,"</b>") 
    }else{
      x
    }
    
  }else{
    x
  }
}, simplify = T) 

## 绘制表格----
p_tab <- ggplot(data = tmp_df,aes(x = x, y = clinical))+
  geom_tile(color = "white",fill = "white")+ #表格边框与填充
  geom_richtext(aes(label = label_bold),label.color = NA)+ # 启用ggtext并去掉边框
  #geom_text(aes(label = if_else(label %in% header, expression(bold(label)),label) ))+
  #geom_text(aes(label = label_bold))+
  # coord_cartesian(ylim = c(0,7))+
  # scale_x_discrete(expand = c(0,0))+
  # scale_y_discrete(expand = c(0,0))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_line(linewidth = line_width),
        axis.line.y = element_blank(),
        text = element_text()
  )+
  geom_hline(yintercept = c(length(unique(tmp_df$clinical))- 0.5), linewidth = line_width)

p_tab  

## 准备误差棒表格
### 通过添加一个空行，类对齐表格中的表头部分。
tmp_error <- rbind(rep(NA,ncol(df)),df)
tmp_error$Clinical_factors[1] <- tab_header[[1]] # "Clinical factors", tab_header[["Clinical_factors"]]

### 因子化排序
tmp_error$Clinical_factors <- factor(tmp_error$Clinical_factors, levels = rev(tmp_error$Clinical_factors))

### 尝试geom_path的数据准备
#tmp_error <- tmp_error %>% mutate(path_x = 1, path_y = c(rep(0, nrow(.)-1),nrow(.)-0.5))

p_errorbar <- ggplot(data = tmp_error)+
  geom_point(aes(x = HR, y = Clinical_factors), # 设置点
             shape = "diamond",
             color = "blue",
             size = 2
  )+
  geom_errorbarh(aes(xmin = HR_confint_lower_95, # 设置误差线
                     xmax =HR_confint_upper_95,
                     y = Clinical_factors),
                 linewidth = line_width, # 线的宽度
                 height = error_bar_height # 两端误差线的宽度
  )+
  # coord_cartesian(ylim = c(0, 8))+
  # scale_x_discrete(expand = c(0,0))+
  # scale_y_discrete(expand = c(0,0))+
  theme_classic()+
  theme(title = element_blank(),
        axis.line.x = element_line(linewidth = line_width), # 下坐标线
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        
  )+
  expand_limits(x=0)+
  #geom_hline(yintercept = c(nrow(tmp_error)-0.5), linewidth = line_width)+ #上坐标线
  # geom_path(aes(x=path_x, y = path_y))
  # annotate("segment",x=1,xend = 1,y=0,yend = nrow(tmp_error)-0.5)
  geom_vline(xintercept = ref_line, color = "grey50") # 绘制无效线
#geom_segment(aes(x =1, xend = 1, y = 0, yend = nrow(tmp_error)-0.5), inherit.aes = FALSE)

# geom_segment(aes(x =1, xend = 1, y = 0, yend = nrow(tmp_error)-0.5),
#              color = "grey60",linewidth = line_width,size = 0.1)  # 无效线
p_errorbar


## aplot拼图：可用
# p <-  aplot::insert_left(p_errorbar, p_tab, width = 1)
## patchwork拼图：可用
p <- p_tab + p_errorbar + plot_annotation(title = plot_title,
                                     theme = theme(plot.title = element_text(hjust = 0.5)))
p
ggsave(filename = "forest.pdf", plot = p, width = 11, height = 5)
ggsave(filename = "forest.png", plot = p, width = 11, height = 5,dpi = 600)

















