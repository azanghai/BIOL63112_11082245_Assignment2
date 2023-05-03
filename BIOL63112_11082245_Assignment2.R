# load tidyverse package for data wrangling and processing
library(tidyverse)
# load visdat package for identifying missing values and data types
library(visdat)
# load Hmisc package for data visualization and correlation analysis
library(Hmisc)
# load corrplot package for correlation analysis
library(corrplot)
# load skimr package for data summary
library(skimr)
# load lme4 package for linear mixed models
library(lme4)
# load lmerTest package for linear mixed models
library(lmerTest)
# load performance package for model selection
library(performance)

library(sf)

library(rnaturalearth) # 获取自然地球数据
library(rnaturalearthdata) # 获取自然地球数据
library(maps)
library(olsrr)
library(modelr)

# variable explanation
# https://www.kaggle.com/datasets/lashagoch/life-expectancy-who-updated
# This dataset was corrected and updated to ensure the accurity of the data.
# Country: List of the 179 countries
# Region: 179 countries are distributed in 9 regions. E.g. Africa, Asia, Oceania, European Union, Rest of Europe and etc.
# Year: Years observed from 2000 to 2015
# Infant_deaths: Represents infant deaths per 1000 population
# Under_five_deaths: Represents deaths of children under five years old per 1000 population
# Adult_mortality: Represents deaths of adults per 1000 population
# Alcohol_consumption: Represents alcohol consumption that is recorded in liters of pure alcohol per capita with 15+ years old
# Hepatitis_B: Represents % of coverage of Hepatitis B (HepB3) immunization among 1-year-olds.
# Measles: Represents % of coverage of Measles containing vaccine first dose (MCV1) immunization among 1-year-olds
# BMI: Represents the mean BMI of the population
# Polio: Represents % of coverage of Polio (Pol3) immunization among 1-year-olds
# Diphtheria: Represents % of coverage of Diphtheria (Dip3) immunization among 1-year-olds
# Incidents_Hiv: Incidents of HIV per 1000 population aged 15-49
# GDP_per_capita: Represents the GDP per capita in current USD
# Population_mln: Total population in millions
# Thinness_ten_nineteen_years: Prevalence of thinness among adolescents aged 10-19 years. BMI < -2 standard deviations below the median.
# Thinness_five_nine_years: Prevalence of thinness among children aged 5-9 years. BMI < -2 standard deviations below the median.
# Schooling: Average years that people aged 25+ spent in formal education
# Economy_status_Developed: Developed country
# Economy_status_Developing: Developing country
# Life_expectancy: Average life expectancy of both genders in different years from 2010 to 2015

# 最初选择的各种预测因素是否真的会影响预期寿命？实际影响预期寿命的预测变量是什么？
# 一个预期寿命值较低（<65）的国家是否应该增加医疗保健支出以提高其平均寿命？
# 婴儿和成人死亡率如何影响预期寿命？
# 预期寿命与饮食习惯、生活方式、运动、吸烟、饮酒等有正相关还是负相关？
# 学校教育对人类寿命有何影响？
# 预期寿命与饮酒有正相关还是负相关？
# 人口稠密的国家是否往往具有较低的预期寿命？
# 免疫覆盖率对预期寿命有何影响？

# RQ1: What are the factors that affect life expectancy?
# RQ2: What is the impact of education on life expectancy?
# RQ3: 欧盟地区和亚洲地区关于饮酒与预期寿命的关系是什么？
# RQ4: 预期寿命与GDP的关系如何？

raw_data = read_csv("Life-Expectancy-Data-Updated.csv")

# check data types and missing values
vis_dat(raw_data)

skim(raw_data) %>%
  filter(skim_type == "numeric")

# correct country names for plotting
raw_data$Country[raw_data$Country == "Turkiye"] = "Turkey"
raw_data$Country[raw_data$Country == "Russian Federation"] = "Russia"
raw_data$Country[raw_data$Country == "Bahamas, The"] = "Bahamas"
raw_data$Country[raw_data$Country =="Gambia, The"] = "Gambia"
raw_data$Country[raw_data$Country =="Kyrgyz Republic"] = "Kyrgyzstan"
raw_data$Country[raw_data$Country == "Brunei Darussalam"] = "Brunei"
raw_data$Country[raw_data$Country == "Yemen, Rep."] = "Yemen"
raw_data$Country[raw_data$Country =="Micronesia, Fed. Sts."] = "Micronesia"
raw_data$Country[raw_data$Country =="Congo, Rep."] = "Congo"
raw_data$Country[raw_data$Country =="Congo, Dem. Rep."] = "Dem. Rep. Congo"
raw_data$Country[raw_data$Country =="Eswatini"] = "Swaziland"
raw_data$Country[raw_data$Country =="Egypt, Arab Rep."] = "Egypt"
raw_data$Country[raw_data$Country =="Syrian Arab Republic"] = "Syria"
raw_data$Country[raw_data$Country =="Cote d'Ivoire"] = "Ivory Coast"
raw_data$Country[raw_data$Country =="Iran, Islamic Rep."] = "Iran"
raw_data$Country[raw_data$Country =="Lao PDR"] = "Laos"
raw_data$Country[raw_data$Country =="Venezuela, RB"] = "Venezuela"
raw_data$Country[raw_data$Country =="Central African Republic"] = "Central African Rep."

# merge Economy_status_Developed and Economy_status_Developing into one column
raw_data = raw_data %>%
  mutate(Economy_status = ifelse(Economy_status_Developed == 1, "Developed", "Developing")) %>%
  select(-Economy_status_Developed, -Economy_status_Developing) %>%
  mutate(Economy_status = factor(Economy_status)) %>%
  mutate(Country = factor(Country)) %>%
  mutate(Region = factor(Region)) %>%
  mutate(Year = factor(Year)) %>%
  mutate(Economy_status = fct_relevel(Economy_status, "Developed","Developing"))

str(raw_data)
# summarize the data
summarized_data = raw_data %>%
  group_by(Country) %>%
  # summarize mean but keep region and economy status
  summarize_if(is.numeric, mean) %>%
  left_join(raw_data %>% select(Country,Region,Economy_status) %>% distinct(), by = "Country")



correlation_data = lapply(summarized_data, as.numeric) %>%
  as.data.frame() %>%
  as.matrix() %>%
  rcorr()

cors = round(correlation_data$r,2)
# extract the p-value from the correlation matrix
mat = correlation_data$P
# replace the NA value in the p-value matrix with 0, otherwise the corrplot function will retuen an error.
mat[is.na(mat)] = 0
# plot the correlation matrix
# corrplot function is used to plot the correlation matrix. the meaning of each parameter can be found in the corrplot package using ?corrplot.
corrplot(
  # correlation coefficient matrix
  cors,
  # p-value matrix
  p.mat = mat,
  # plot the upper triangle of the correlation matrix
  # method = "shade" selects the visualization method. Here, we use the shade method(simplely because it looks better than the other methods).
  method = "shade",
  # plot the upper triangle of the correlation matrix
  type = 'upper',
  # insig = 'blank' means that the correlation coefficient which is not significant will be displayed as blank.
  insig = 'blank',
  # add the correlation coefficient on the plot and set the color as black
  addCoef.col = 'black',
  # order the variables based on the hierarchical clustering. This allows us to see the relationship between the variables more clearly.
  order = 'hclust',
  # set the color of the text lables as black
  tl.col = 'black',
  # display the correlation coefficient on the diagonal of the plot
  diag = T,
  # set the position of the text lables as left-top
  tl.pos = 'lt',
  # select cor higher than 0.05 to be displayed.
  sig.level = 0.05,
  # set the size of the text lables as 0.9
  tl.cex = 0.9,
  # let the text lables be rotated by 30 degrees
  tl.srt = 30
)
# the code below is simillar to the code above, but it plots the lower triangle of the correlation matrix.
# arguements that is the same as the code above is not commented.
corrplot(
  cors,
  method = "shade",
  type = 'lower',
  addCoef.col = 'black',
  order = 'hclust',
  tl.col = 'black',
  # add = T means that the plot will be added to the previous plot.
  add = T,
  # tl.pos = 'n' means that the text lables will not be displayed(because it's already displayed in the previous plot).
  tl.pos = 'n',
  # cl.pos = 'n' means that the color legend will not be displayed(because it's already displayed in the previous plot).
  cl.pos = 'n'
)
# add the title to the plot
title("Correration Matrix for All Variables")









# 地区热图，综合热图 Y
# 分地区探索/国家/年份探索————————意义是什么？
# 使用什么模型？
# 如何解释结果
# 学校在不同地区的生存预测强度如何？



world <- ne_countries(scale = "medium", returnclass = "sf")

# 将生命周期数据与地图数据合并
world_data <- merge(world, summarized_data, by.x = "name", by.y = "Country")


# 使用ggplot2绘制Choropleth图
# 其中颜色填充基于Life_expectancy变量（预期寿命）
ggplot(data = world_data) +
  geom_sf(aes(fill = Life_expectancy)) + # 基于预期寿命填充颜色
  scale_fill_gradient(low = "blue", high = "yellow", name = "Life Expectancy") + # 定义颜色渐变
  theme_minimal() + # 使用简约主题
  labs(title = "Life Expectancy by Country") # 设置图表标题

# RQ1: What are the factors that affect the life expectancy?
RQ1.fit1 = lm(Life_expectancy ~ . - Region - Economy_status -Country, data = summarized_data)
check_model(RQ1.fit1)
summary(RQ1.fit1)

summarized_data = summarized_data %>%
  mutate(average_vaccination_rate = rowMeans(select(.,Measles,Hepatitis_B,Polio,Diphtheria))) %>%
  mutate(average_adcolescent_thinness_rate = rowMeans(select(.,Thinness_ten_nineteen_years,Thinness_five_nine_years)))
names(summarized_data)

RQ1.fit2 = lm(Life_expectancy ~ . - Region - Economy_status -Country - Infant_deaths - Measles - Hepatitis_B - Polio - Diphtheria - Thinness_ten_nineteen_years - Thinness_five_nine_years, data = summarized_data)
summary(RQ1.fit2)
check_model(RQ1.fit2)

RQ1.fit2.step = ols_step_both_p(RQ1.fit2)
summary(RQ1.fit2.step$model)
check_model(RQ1.fit2.step$model)


# RQ2:What is the impact of education on life expectancy?
ggplot(data = raw_data,aes(x = Schooling, y = Life_expectancy, color = Region)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Country)

RQ2.fit1 = lmer(Life_expectancy ~ Schooling+(Schooling|Country), data = raw_data)
summary(RQ2.fit1)

coef(RQ2.fit1)$Country %>%
  filter(Schooling < 0)


# RQ3: 对于饮酒量高的国家，是否会影响预期寿命？
alcohol_data1 = raw_data %>%
  filter(Life_expectancy > 80)
RQ3.fit1 = lmer(Life_expectancy ~ Alcohol_consumption+(Alcohol_consumption|Country), data = alcohol_data1)
summary(RQ3.fit1)
check_model(RQ3.fit1)
coef(RQ3.fit1)

alcohol_data2 = raw_data %>%
  # filter(Alcohol_consumption>7.78) %>%
  filter(Region == "European Union")

RQ3.fit2 = lmer(Life_expectancy ~ Alcohol_consumption+(1|Country), data = alcohol_data2)
summary(RQ3.fit2)
check_model(RQ3.fit2)
coef(RQ3.fit2)

alcohol_data3 = raw_data %>%
  # filter(Alcohol_consumption>7.78) %>%
  filter(Region == "Asia")
RQ3.fit3 = lmer(Life_expectancy ~ Alcohol_consumption+(1|Country), data = alcohol_data3)
summary(RQ3.fit3)
check_model(RQ3.fit3)
coef(RQ3.fit3)

alcohol_data2  = alcohol_data2%>%
  add_predictions(RQ3.fit2,var = "EUpred")
alcohol_data3  = alcohol_data3%>%
    add_predictions(RQ3.fit3,var = "Asiapred")


ggplot() +
  geom_point(aes(x =Alcohol_consumption , y =Life_expectancy,colour = Country),data = alcohol_data2) +
  geom_line(aes(x = Alcohol_consumption, y = EUpred,group = Country), data = alcohol_data2, color = "red") +
  geom_point(aes(x =Alcohol_consumption , y =Life_expectancy,colour = Country),data = alcohol_data3) +
  geom_line(aes(x = Alcohol_consumption, y = Asiapred,group = Country), data = alcohol_data3, color = "blue")+
  geom_point(aes(x =Alcohol_consumption , y =Life_expectancy,colour = Country),data = alcohol_data5) +
  geom_line(aes(x = Alcohol_consumption, y = AfricaPred,group = Country), data = alcohol_data5, color = "red")
  # draw prediction line use the model

alcohol_data4 = raw_data %>%
  filter((Region == "European Union") | (Region == "Asia"))

RQ3.fit4 = lm(Life_expectancy ~ I(Alcohol_consumption^2), data = alcohol_data4)
summary(RQ3.fit4)
RQ3.fit5 = lm(Life_expectancy ~ Alcohol_consumption, data = alcohol_data4)
summary(RQ3.fit5)

alcohol_data4$pred = predict(RQ3.fit4)
ggplot(data = alcohol_data4, aes(x = Alcohol_consumption, y = Life_expectancy)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")

compare_performance(RQ3.fit4,RQ3.fit5)
table(raw_data$Region)

# GDP predict life expectancy
GDP_data = raw_data
ggplot(data = raw_data, aes(x = GDP_per_capita, y = Life_expectancy,colour = Economy_status)) +
  geom_point()

RQ4.fit1 = lm(Life_expectancy ~ I(log(GDP_per_capita)), data = raw_data)
summary(RQ4.fit1)
check_model(RQ4.fit1)
GDP_data = GDP_data %>%
  add_predictions(RQ4.fit1, var = "pred")

coef(RQ4.fit1)

ggplot(data = GDP_data, aes(x = GDP_per_capita, y = Life_expectancy)) +
    geom_point() +
    geom_line(aes(y = pred), color = "red")

ggplot(data = raw_data, aes(x = BMI, y = Life_expectancy,colour = Country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
