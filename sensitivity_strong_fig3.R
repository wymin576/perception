# 清空环境 + 设置路径
rm(list = ls())
setwd("C:\\Users\\jj\\Desktop\\hsscjuly\\hss2")
# 加载必需包
# install.packages('latex2exp')
library(cmdstanr);library(posterior);library(tidyverse);
library(latex2exp)  # 公式显示必须加载
dir(pattern = '.rds')
# ----------------------------------------------------
# 1. 读取两个模型的后验结果
# ----------------------------------------------------
# 模型1：包含全部 31 个省份（含小样本/无样本省份）
draws_norm <- readRDS('poststratification_norm_chat.rds')$draws() 

# 模型2：
draws_strong <- readRDS('poststratification_chat_strong.rds')$draws()
# ----------------------------------------------------
# 2. 转换为标准后验对象格式
# ----------------------------------------------------
draws_norm_obj <- as_draws_array(draws_norm)
draws_strong_obj <- as_draws_array(draws_strong)


# ----------------------------------------------------
# 3. 提取小区域估计参数 phi
# ----------------------------------------------------
phi_draws_norm <- subset_draws(draws_norm_obj, 
                               variable = paste0("phi[", 1:31, "]"))
phi_draws_strong <- subset_draws(draws_strong_obj, 
                                variable = paste0("phi[", 1:31, "]"))


# ----------------------------------------------------
# 4. 省份英文名映射（严格对应 phi[1]~phi[31]）
# ----------------------------------------------------
names_31 <-  c(
  "Anhui", "Beijing", "Fujian", "Gansu", "Guangdong",
  "Guangxi", "Guizhou", "Hainan", "Hebei", "Henan",
  "Heilongjiang", "Hubei", "Hunan", "Jilin", "Jiangsu",
  "Jiangxi", "Liaoning", "Inner Mongolia", "Ningxia", "Qinghai",
  "Shandong", "Shanxi", "Shaanxi", "Shanghai", "Sichuan",
  "Tianjin", "Tibet", "Xinjiang", "Yunnan", "Zhejiang",
  "Chongqing"
)

# ----------------------------------------------------
# 5. 替换参数名为省份英文名 + 对齐省份
# ----------------------------------------------------
variables(phi_draws_norm) <- names_31
variables(phi_draws_strong) <- names_31

# 从31省中，只保留26省共有的省份（自动对齐）
phi_draws_norm_aligned <- subset_draws(phi_draws_norm, 
                                       variable = names_31) 


# ----------------------------------------------------
# 6. 转为矩阵：行=后验样本，列=省份
# ----------------------------------------------------
phi_norm_mat <- as_draws_matrix(phi_draws_norm_aligned)  # 4000 × 26
phi_strong_mat <- as_draws_matrix(phi_draws_strong)        # 4000 × 26


diff_mat <- phi_norm_mat - phi_strong_mat  # 4000 × 26
# ----------------------------------------------------
# 8. 转为长数据（ggplot绘图必需）
# 计算后验差异的统计量
diff_summary <- data.frame(
  province = names_31,
  mean_diff = colMeans(diff_mat),
  sd_diff = apply(diff_mat, 2, sd),
  q5 = apply(diff_mat, 2, quantile, 0.05),
  q50 = apply(diff_mat, 2, quantile, 0.5),
  q95 = apply(diff_mat, 2, quantile, 0.95)
)
summary(diff_summary)

ggplot(diff_summary, aes(x = province, y = q50)) +
  geom_errorbar(aes(ymin = q5, ymax = q95), 
                color = "#8ab4f8",
                width = 0.5) +
  geom_point(color = "black", size = 1.5) +
  # 加 y=0 水平参考线
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  labs(x = "", y = latex2exp::TeX(r"(\textbf{$\hat{\phi}_{p}^{1} - \hat{\phi}_{p}^{2}$})")) +
  scale_y_continuous(limits = c(-0.25, 0.25), 
                     breaks = seq(-0.25, 0.25, 0.1)) +
  theme_minimal() +
  theme(
    # 去掉所有网格线
    panel.grid = element_blank(),
    
    # 显示 X 轴和 Y 轴线条
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # 坐标轴文字样式
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    
    # 纯白背景
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    plot.margin = margin(1, 1, 1, 1, "cm")
  )


ggsave('fig5_strong.jpg',
       width = 10,      # 增加宽度
       height = 5,      # 增加高度
       dpi = 600,       # 提高分辨率
       bg = "white")
