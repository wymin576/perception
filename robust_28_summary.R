# 清空环境 + 设置工作路径
rm(list = ls())
setwd("C:\\Users\\jj\\Desktop\\hsscjuly\\hss2")

# 加载包：Stan建模、后验处理、绘图
library(cmdstanr);library(posterior);
library(tidyverse);library(latex2exp)  

# ----------------------------------------------------
# 1. 读取两个模型的后验抽样结果
#  - draws_norm_31   : 31个省份（含缺失样本省份）
#  - draws_norm_28   : 28个省份（剔除无样本省份）
# ----------------------------------------------------
draws_norm_31 <- readRDS('poststratification_norm_chat.rds')$draws() 
draws_norm_28 <- readRDS('poststratification_norm_28.rds')$draws()

# 转换为 posterior 包标准数组格式（迭代×链×参数）
draws_norm_obj <- as_draws_array(draws_norm_31)
draws_hyper_obj <- as_draws_array(draws_norm_28)

# ----------------------------------------------------
phi_draws_norm <- subset_draws(draws_norm_obj, 
                               variable = paste0("phi[", 1:31, "]"))
phi_draws_hyper <- subset_draws(draws_hyper_obj, 
                                variable = paste0("phi[", 1:28, "]"))

# 转换为矩阵：行=抽样，列=省份
phi_norm_mat <- as_draws_matrix(phi_draws_norm)  # 4000 × 31
phi_hyper_mat <- as_draws_matrix(phi_draws_hyper)  # 4000 × 28

# ----------------------------------------------------
# 3. 给矩阵列名赋值【省份英文名称】
#  - 31省名称
#  - 28省名称（剔除了新疆、西藏、海南）
# ----------------------------------------------------
colnames(phi_norm_mat) <-  c(
  "Anhui", "Beijing", "Fujian", "Gansu", "Guangdong",
  "Guangxi", "Guizhou", "Hainan", "Hebei", "Henan",
  "Heilongjiang", "Hubei", "Hunan", "Jilin", "Jiangsu",
  "Jiangxi", "Liaoning", "Inner Mongolia", "Ningxia", "Qinghai",
  "Shandong", "Shanxi", "Shaanxi", "Shanghai", "Sichuan",
  "Tianjin", "Tibet", "Xinjiang", "Yunnan", "Zhejiang",
  "Chongqing"
)

colnames(phi_hyper_mat) <- c(
  "Anhui", "Beijing", "Fujian", "Gansu", "Guangdong",
  "Guangxi", "Guizhou", "Hebei", "Henan", "Heilongjiang",
  "Hubei", "Hunan", "Jilin", "Jiangsu", "Jiangxi",
  "Liaoning", "Inner Mongolia", "Ningxia", "Qinghai",
  "Shandong", "Shanxi", "Shaanxi", "Shanghai",
  "Sichuan", "Tianjin", "Yunnan", "Zhejiang", "Chongqing"
)

# ----------------------------------------------------
# 4. 关键步骤：对齐省份 + 排序后计算差异
# 目的：比较同一省份在两个模型中的后验差异
# 做法：按分位数对齐排序 → 相减（稳健对比）
# ----------------------------------------------------

# 从31省矩阵中【自动提取】28省共有的省份（核心对齐）
common_provs <- colnames(phi_hyper_mat)
phi_norm_aligned <- phi_norm_mat[, common_provs]

diff_mat <- phi_norm_aligned - phi_hyper_mat  # 4000 × 28
# ----------------------------------------------------
# 5. 长数据格式转换（ggplot2绘图必需）
# ----------------------------------------------------
# 计算后验差异的统计量
diff_summary <- data.frame(
  province = common_provs,
  mean_diff = colMeans(diff_mat),
  sd_diff = apply(diff_mat, 2, sd),
  q5 = apply(diff_mat, 2, quantile, 0.05),
  q50 = apply(diff_mat, 2, quantile, 0.5),
  q95 = apply(diff_mat, 2, quantile, 0.95)
)

print(diff_summary %>% mutate(across(-province, ~round(., 3))))


ggplot(diff_summary, aes(x = province, y = q50)) +
  geom_errorbar(aes(ymin = q5, ymax = q95), 
                color = "#8ab4f8",
                width = 0.5) +
  geom_point(color = "black", size = 1.5) +
  # 加 y=0 水平参考线
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  labs(x = "", y = latex2exp::TeX(r"(\textbf{$\hat{\phi}_{p}^{31} - \hat{\phi}_{p}^{28}$})")) +
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

ggsave('fig5_28.jpg',
       width = 10,      # 增加宽度
       height = 5,      # 增加高度
       dpi = 600,       # 提高分辨率
       bg = "white")



