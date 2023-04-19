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


raw_data = read_csv("Life-Expectancy-Data-Updated.csv")

# check data types and missing values
vis_dat(raw_data)

skim(raw_data) %>%
  filter(skim_type == "numeric")

# merge Economy_status_Developed and Economy_status_Developing into one column
raw_data = raw_data %>%
  mutate(Economy_status = ifelse(Economy_status_Developed == 1, "Developed", "Developing")) %>%
  select(-Economy_status_Developed, -Economy_status_Developing) %>%
  mutate(Economy_status = factor(Economy_status)) %>%
  mutate(Country = factor(Country)) %>%
  mutate(Region = factor(Region)) %>%
  mutate(Year = factor(Year)) %>%
  mutate(Economy_status = fct_relevel(Economy_status, "Developed","Developing"))

correlation_data = lapply(raw_data, as.numeric) %>%
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

m1 = lm(Life_expectancy ~ Year, data = raw_data)
summary(m1)
ggplot(raw_data, aes(x = Year, y = Life_expectancy,colour =Economy_status )) +
  geom_point() +
  geom_line(aes(group = Country), size = 1) +
  labs(title = "Life Expectancy vs Year", x = "Year", y = "Life Expectancy")

raw_data %>%
  group_by(Country) %>%
  skim() %>%
  filter(skim_variable == "Life_expectancy")

names(raw_data)

# analysis the data use Linear mixed model.
m2 = lmer(Life_expectancy ~ Infant_deaths + Under_five_deaths + Adult_mortality + Alcohol_consumption + Hepatitis_B + Measles + BMI + Polio + Diphtheria + Incidents_HIV + GDP_per_capita + Population_mln + Thinness_ten_nineteen_years + Thinness_five_nine_years + Schooling+(1|Region)+(1|Year), data = raw_data)
summary(m2)
check_model(m2)

# 地区热图，综合热图
# 分地区探索/国家/年份探索————————意义是什么？
# 使用什么模型？
# 如何解释结果
# 学校在不同地区的生存预测强度如何？

