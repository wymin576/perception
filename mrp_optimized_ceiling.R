rm(list = ls())
library(cmdstanr);library(tidyverse)
# data preparation
setwd("C:\\Users\\jj\\Desktop\\hsscjuly\\hss2")
# importing the data from multiple sources
df <- read.csv("survey_ceiling.csv") %>% 
  select(-X) 
psframe <- read.csv("psframe.csv") |> 
  select(-X) |> rename(prov = unit)

province1 <- read.csv("province2.csv") %>% 
  select(prov,ratio_urban_rural,rate_coresid) 
province2 <- read.csv("province3.csv") %>% 
  select(-X) 
names(province2)
province <- merge(province1,province2,
                  by = 'prov') 
names(df);names(psframe);names(province)

# 直接替换成数字
province$unit <- as.integer(as.factor(province$prov))

psframe$unit <- match(psframe$prov, province$prov)

df$unit <- match(df$prov, province$prov)

names_factor <- c('unit','prov',"area","unit")
names_numeric <- setdiff(names(province),
                         names_factor)
# 主成分分析

df2 <- province |> column_to_rownames("prov")  |> 
  transmute(Urir = ratio_urban_rural,
            Gpc = per_capita,
            College = college,
            Gdpgr = ratio_of_gdp,
            Hkt = migration_mean,
            Mir = mot_il_mean,
            Flfr = fat_rural_mean,
            Hfss80 = status_yuv_top_mean,
            Hfss20 = status_yuv_bottom_mean,
            Coresidence = rate_coresid)

# 相关系数矩阵
library(writexl)

cor_matrix <- round(cor(df2), 3)

# 保留下三角，其他位置设为 NA
cor_matrix[upper.tri(cor_matrix)] <- NA

cor_matrix_df <- data.frame(
  Variable = rownames(cor_matrix),
  cor_matrix,
  row.names = NULL
)

write_xlsx(
  cor_matrix_df,
  "correlation_matrix_ceil.xlsx"
)

library(psych) 
# fa.parallel(df2, fa="pc")
# the selection of rotate method
# "none", "varimax", "quartimax", "simplimax"
pca_none <- principal(df2, nfactors = 5, 
          rotate = "none", 
          scores = TRUE)
pca_none$loadings

pca_varimax <- principal(df2, nfactors = 5, 
          rotate = "varimax", 
          scores = TRUE)
pca_varimax$loadings

pca_quartimax <- principal(df2, nfactors = 5, 
          rotate = "quartimax", 
          scores = TRUE)
pca_quartimax$loadings

pca_simplimax <- principal(df2, nfactors = 5, 
          rotate = "simplimax", 
          scores = TRUE)
pca_simplimax$loadings

# 选择"varimax"

# 提取主成分得分（用于后续回归或聚类）
pca_varimax <- principal(df2, nfactors = 5, 
                         rotate = "varimax", 
                         scores = TRUE)


# 查看载荷矩阵（已经很清楚）
loadings_df <- as.data.frame.matrix(
  round(unclass(pca_varimax$loadings),3))

loadings_df <- cbind(Variable = rownames(loadings_df), loadings_df)
rownames(loadings_df) <- NULL

write_xlsx(loadings_df, "pca_loadings_ceil.xlsx")

# 提取主成分得分
scores_varimax <- as.data.frame(pca_varimax$scores)


# 将得分合并回原始数据（假设原数据有省份信息）
province <- cbind(province |> select(prov,area), scores_varimax) 

# 查看结果

four_way_array <- array(data = psframe$freq,
                        dim = c(31, 4, 2, 2),
                        dimnames = list(
                          state = 1:31,age = 1:4,
                          gender = c("1", "2"),
                          urban = c("1", "2") ))

# 假设你的数据已经准备好
data_for_stan <- list(
  N = nrow(df),
  state = df$unit,
  age = df$age,
  gender = df$gender,
  urban = df$urban,
  y = df$up,
  
  RC1 = province$RC1,
  RC2 = province$RC2,
  RC3 = province$RC3,
  RC4 = province$RC4,
  RC5 = province$RC5,
  area = province$area,
  pop = four_way_array

)

# 编译和运行
mod <- cmdstan_model("perception.stan")
mod
fit <- mod$sample(
  data = data_for_stan,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  save_cmdstan_config=TRUE
)

# ------------------------------------------
# Nation-level and Province-level Estimates
library(posterior)
fit_var <- fit$metadata()$variables

fit_var[grep('phi',fit_var)]

draws <- fit$draws()  # 返回 draws_array 对象

summary_stats <- summarise_draws(draws) 

phi_tot <- summary_stats[grep("tot_phi",
                              summary_stats$variable), ]
state_tot <- summary_stats[grep("phi",
                                summary_stats$variable)[-32], ]
state_tot$prov <- province$unit

library(viridis)

# 按中位数排序

state_tot$prov_en <- c("Anhui", "Beijing", "Fujian", "Gansu", "Guangdong", "Guangxi", 
                       "Guizhou", "Hainan", "Hebei", "Henan", "Heilongjiang", "Hubei", 
                       "Hunan", "Jilin", "Jiangsu", "Jiangxi", "Liaoning", "Inner Mongolia", 
                       "Ningxia", "Qinghai", "Shandong", "Shanxi", "Shaanxi", "Shanghai", 
                       "Sichuan", "Tianjin", "Tibet", "Xinjiang", "Yunnan", "Zhejiang", 
                       "Chongqing")

state_tot <- state_tot %>%
  mutate(prov_en = factor(prov_en, levels = prov_en[order(median)])         )

min(state_tot$q5);max(state_tot$q95)
write.csv(state_tot,'state_perception_ceil.csv')
# 绘图
library(scales)  # 需要先加载scales包
max(state_tot$q95);min(state_tot$q5)

ggplot(state_tot, aes(x = reorder(prov_en, median), y = median)) +
  # 95% 可信区间 - 注意这里用垂直误差线
  geom_errorbar(aes(ymin = q5, ymax = q95), 
                width = 0.15, 
                color = "gray40", 
                size = 0.7) +
  # 点估计 - 使用单色
  geom_point(color = "#2c7bb6", 
             size = 4, 
             shape = 16) +
  # 总体中位数参考线（现在是水平线）
  geom_hline(yintercept = median(state_tot$median), 
             linetype = "dashed", 
             color = "red", 
             size = 0.8,
             alpha = 0.6) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = "Perceived upward mobility rate") +
  # 设置y轴为百分率格式
  scale_y_continuous(labels = percent, limits = c(0.3, 0.8)) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50"),
    # 去掉所有网格线
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # 保留完整的边框（上下左右）
    panel.border = element_rect(color = "black", 
                                fill = NA, size = 0.5),
    axis.line = element_blank()
  )

# 如果需要保存
ggsave("poststratification_ceiling.png", 
       width = 10, height = 8, dpi = 300)



state_tot$name <- state_tot$prov_en
# 生成地图fig4
# 1. 加载包
library(rnaturalearth)
china_prov <- ne_states(country = "China", returnclass = "sf")
china_prov$name[china_prov$name == "Inner Mongol"] <- "Inner Mongolia"
china_prov$name[china_prov$name == "Xizang"] <- "Tibet"

# 4. 合并地图 + 你的数据 fig1_sorted

map_data <- china_prov %>% 
  left_join(state_tot %>% select(name, median), by="name")

library(sf);library(dplyr)
names(map_data)
ggplot() +
  # 省级填充（支持率）
  geom_sf(
    data = map_data,
    aes(fill = median),
    color = "red",
    size = 0.1
  ) +
  # 配色
  scale_fill_viridis_c(
    option = "A",
    na.value = "gray90",
    name = "Perceived upward mobility rate"
  ) +
  
  labs(title = NULL,x = NULL,y = NULL ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"),
    panel.grid = element_blank(),
    # ✅ 隐藏 X 轴和 Y 轴的刻度数字标签
    axis.text = element_blank(),
    
    # ✅ 隐藏 X 轴和 Y 轴的刻度短线
    axis.ticks = element_blank()
  )

ggsave('fig4_map_ceil.png',
       width = 10,      # 增加宽度
       height = 10,      # 增加高度
       dpi = 600,       # 提高分辨率
       bg = "white")


library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(ggplot2)

# 准备中国省份数据
china_prov <- ne_states(country = "China", returnclass = "sf")
china_prov$name[china_prov$name == "Inner Mongol"] <- "Inner Mongolia"
china_prov$name[china_prov$name == "Xizang"] <- "Tibet"

map_data <- china_prov %>% 
  left_join(state_tot %>% select(name, median), by = "name")

# ============================================================
# 地理意义上的秦岭-淮河线（精确版本）
# ============================================================
# 秦岭段：西起甘肃东南部（104.5°E），东到河南西部（112.5°E）
# 沿秦岭主脊线，大致呈西北-东南走向
# 淮河段：从河南南部（112.5°E）到江苏入海口（121.5°E）
# 沿淮河主河道
library(sf)

## ==============================
## Qinling–Huaihe line
## CRS: EPSG:4326
## ==============================

qh <- data.frame(
  lon = c(
    ## Qinling
    104.55,104.90,105.30,105.80,106.20,106.70,
    107.10,107.50,107.90,108.30,108.70,109.10,
    109.50,109.90,110.30,110.70,111.10,111.50,
    111.90,112.20,
    
    ## Huai River
    112.50,112.90,113.30,113.70,114.10,
    114.50,114.90,115.30,115.70,116.10,
    116.50,116.90,117.30,117.70,118.10,
    118.50,118.90,119.30,119.70,120.10,
    120.50,120.90,121.20
  ),
  
  lat = c(
    ## Qinling
    34.78,34.72,34.60,34.48,34.35,34.20,
    34.05,33.92,33.82,33.72,33.62,33.55,
    33.50,33.45,33.40,33.36,33.31,33.27,
    33.23,33.20,
    
    ## Huai River
    33.18,33.12,33.05,32.98,32.90,
    32.84,32.78,32.72,32.66,32.60,
    32.53,32.46,32.38,32.30,32.22,
    32.13,32.04,31.95,31.87,31.79,
    31.72,31.66,31.60
  )
)

qinling_huaihe_sf <-
  st_as_sf(qh,
           coords = c("lon","lat"),
           crs = 4326) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")



# 创建胡焕庸线
line_coords <- matrix(c(
  127.5, 50.2,   # Heihe
  98.5, 25.0     # Tengchong
), ncol = 2, byrow = TRUE)

ht_line <- st_sfc(st_linestring(line_coords), crs = 4326)

# 创建标注点数据
points_df <- data.frame(
  lon = c(130, 98),
  lat = c(52.2, 25.0),
  label = c("Heihe", "Tengchong")
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# ============================================================
# 绘制地图
# ============================================================
ggplot() +
  geom_sf(
    data = map_data,
    aes(fill = median),
    color = "red",
    size = 0.1
  ) +
  # 秦岭-淮河线
  geom_sf(
    data = qinling_huaihe_sf,
    colour = "#C00000",
    linewidth = 1,
    lineend = "round"
  ) + 
  geom_sf(data = ht_line, linetype = "dashed", 
          size = 1, color = "black") +
  geom_sf_text(data = points_df, aes(label = label), 
               size = 3, fontface = "bold",
               # 修改点：
               # Tengchong (第2个点): x 从 -4 变为 -5.5 (更靠左)
               # Heihe (第1个点):    y 从 -4 变为 -5.5 (更靠上)
               nudge_x = c(-1.5, -1.5),  
               nudge_y = c(-1.5, 1.5)) +
  # 添加文字标注
  annotate(
    "text",
    x = 128.5,           # 文字位置经度
    y = 32.2,            # 文字位置纬度（在线条上方一点）
    label = "The Qinling–Huaihe Line",
    size = 3,
    color = "#C00000",
    fontface = "bold",
    hjust = 0.5
  ) +
  scale_fill_viridis_c(
    option = "A",
    na.value = "gray90",
    name = "Perceived upward mobility rate"
  ) +
  labs(title = NULL, x = NULL, y = NULL) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave('fig4_map_qh_ceil.png',
       width = 10,      # 增加宽度
       height = 10,      # 增加高度
       dpi = 600,       # 提高分辨率
       bg = "white")
