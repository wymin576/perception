rm(list = ls())
library(cmdstanr);library(tidyverse);library(psych)
# ------------------------Second section: 31--------------------
setwd("C:\\Users\\jj\\Desktop\\hsscjuly\\hss2")

# data preparation
# importing the data from multiple sources
df <- read.csv("survey.csv") %>% 
  select(-X) 
psframe <- read.csv("psframe.csv") |> 
  select(-X) |> rename(prov = unit)

province1 <- read.csv("province2.csv") %>% 
  select(prov,ratio_urban_rural,rate_coresid) 
province2 <- read.csv("province3.csv") %>% 
  select(-X) 

province <- merge(province1,province2,
                  by = 'prov') 

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
            Hfss90 = status_yuv_top_mean,
            Hfss10 = status_yuv_bottom_mean,
            Coresidence = rate_coresid)

# 提取主成分得分
pca_varimax <- principal(df2, nfactors = 5, 
                         rotate = "varimax", 
                         scores = TRUE)


# 查看载荷矩阵
loadings_df <- as.data.frame.matrix(
  round(unclass(pca_varimax$loadings),3))

loadings_df <- cbind(Variable = rownames(loadings_df), loadings_df)
rownames(loadings_df) <- NULL



# 提取主成分得分
scores_varimax <- as.data.frame(pca_varimax$scores)


# 将得分合并回原始数据
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
fit <- cmdstan_model("perception.stan")$sample(
  data = data_for_stan,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  save_cmdstan_config=TRUE
)

fit$save_object('poststratification_norm_chat.rds')

fit_strong <- cmdstan_model("perception_strong.stan")$sample(
  data = data_for_stan,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  save_cmdstan_config=TRUE
)

fit_strong$save_object("poststratification_chat_strong.rds") 

fit_weak <- cmdstan_model("perception_weak.stan")$sample(
  data = data_for_stan,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  save_cmdstan_config = TRUE
)

fit_weak$save_object("poststratification_chat_weak.rds") 

# removing the province-level random effects
fit_de <- cmdstan_model("perception_de.stan")$sample(
  data = data_for_stan,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  save_cmdstan_config=TRUE
)

fit_de$save_object("poststratification_chat_pooling.rds") 

# ------------------------Second section: 28--------------------
setwd("C:\\Users\\jj\\Desktop\\hsscjuly\\hss2")
df <- read.csv("survey.csv") %>% 
  select(-X) 
# removing three provinces without observation:hainan,xinjiang and tibet
psframe <- read.csv("psframe.csv") |> 
  select(-X) |> rename(prov = unit) |> 
  filter(!prov %in% c("新疆", "海南", "西藏"))

province1 <- read.csv("province2.csv") %>% 
  select(prov,ratio_urban_rural,rate_coresid) 

province2 <- read.csv("province3.csv") %>% 
  select(-X) 

province <- merge(province1,province2,
                  by = 'prov') |> 
  filter(!prov %in% c("新疆", "海南", "西藏"))


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
            Hfss90 = status_yuv_top_mean,
            Hfss10 = status_yuv_bottom_mean,
            Coresidence = rate_coresid)

# 提取主成分得分
pca_varimax <- principal(df2, nfactors = 5, 
                         rotate = "varimax", 
                         scores = TRUE)


# 查看载荷矩阵
loadings_df <- as.data.frame.matrix(
  round(unclass(pca_varimax$loadings),3))

loadings_df <- cbind(Variable = rownames(loadings_df), loadings_df)
rownames(loadings_df) <- NULL

# 提取主成分得分
scores_varimax <- as.data.frame(pca_varimax$scores)


# 将得分合并回原始数据
province <- cbind(province |> select(prov,area), scores_varimax) 

# 查看结果

four_way_array <- array(data = psframe$freq,
                        dim = c(28, 4, 2, 2),
                        dimnames = list(
                          state = 1:28,age = 1:4,
                          gender = c("1", "2"),
                          urban = c("1", "2") ))

table(df$unit,useNA = 'always')
df |> filter(is.na(unit))
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
fit_28 <- cmdstan_model("perception_28.stan")$sample(
  data = data_for_stan,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  save_cmdstan_config=TRUE
)

fit_28$save_object('poststratification_norm_28.rds')


# ------------------------Three section:27 provinces --------------------
# removing four provinces without observation and with small sample
df <- read.csv("survey.csv") %>% select(-X) |> 
  filter(!prov %in% c("新疆", "海南", "西藏","内蒙古"))

psframe <- read.csv("psframe.csv") |> 
  select(-X) |> rename(prov = unit) |> 
  filter(!prov %in% c("新疆", "海南", "西藏","内蒙古"))

province <- merge(province1,province2,
                  by = 'prov') |> 
  filter(!prov %in% c("新疆", "海南", "西藏","内蒙古"))


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
            Hfss90 = status_yuv_top_mean,
            Hfss10 = status_yuv_bottom_mean,
            Coresidence = rate_coresid)

# 提取主成分得分（用于后续回归或聚类）
pca_varimax <- principal(df2, nfactors = 5, 
                         rotate = "varimax", 
                         scores = TRUE)


# 查看载荷矩阵（已经很清楚）
loadings_df <- as.data.frame.matrix(
  round(unclass(pca_varimax$loadings),3))

loadings_df <- cbind(Variable = rownames(loadings_df), loadings_df)
rownames(loadings_df) <- NULL



# 提取主成分得分
scores_varimax <- as.data.frame(pca_varimax$scores)


# 将得分合并回原始数据（假设原数据有省份信息）
province <- cbind(province |> select(prov,area), scores_varimax) 

# 查看结果

four_way_array <- array(data = psframe$freq,
                        dim = c(27, 4, 2, 2),
                        dimnames = list(
                          state = 1:27,age = 1:4,
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
fit_27 <- cmdstan_model("perception_27.stan")$sample(
  data = data_for_stan,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  save_cmdstan_config=TRUE
)

fit_27$save_object('poststratification_norm_27.rds')
