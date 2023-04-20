# 安装并加载所需的包
install.packages("sf")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("maps")
install.packages("maptools")

library(sf) # 处理地理空间数据
library(ggplot2) # 绘制图形
library(rnaturalearth) # 获取自然地球数据
library(rnaturalearthdata) # 获取自然地球数据
library(rgeos) # 提供地理空间操作函数
library(maps) # 提供地图数据
library(maptools) # 提供地图工具

# 从rnaturalearth包获取世界地图数据
world <- ne_countries(scale = "medium", returnclass = "sf")

country_replacements <- c(
  "Turkiye" = "Turkey",
  "Russian Federation" = "Russia",
  "Bahamas, The" = "Bahamas",
  "Gambia, The" = "Gambia",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Brunei Darussalam" = "Brunei",
  "Yemen, Rep." = "Yemen",
  "Micronesia, Fed. Sts." = "Micronesia",
  "Congo, Rep." = "Republic of the Congo",
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Eswatini" = "Swaziland",
  "Egypt, Arab Rep." = "Egypt",
  "Syrian Arab Republic" = "Syria",
  "Cote d'Ivoire" = "Ivory Coast",
  "Iran, Islamic Rep." = "Iran",
  "Lao PDR" = "Laos",
  "Venezuela, RB" = "Venezuela"
)

# 假设我们有一个名为raw_data的数据框，其中包含国家名称和预期寿命数据
# 需要确保国家名称与地图数据中的名称匹配，可能需要对数据进行一些预处理

# 将生命周期数据与地图数据合并
world_data <- merge(world, raw_data, by.x = "name", by.y = "Country")

# 使用ggplot2绘制Choropleth图
# 其中颜色填充基于Life_expectancy变量（预期寿命）
ggplot(data = world_data) +
  geom_sf(aes(fill = Life_expectancy)) + # 基于预期寿命填充颜色
  scale_fill_gradient(low = "blue", high = "red", name = "Life Expectancy") + # 定义颜色渐变
  theme_minimal() + # 使用简约主题
  labs(title = "Life Expectancy by Country in 2015") # 设置图表标题

unique(raw_data$Country)