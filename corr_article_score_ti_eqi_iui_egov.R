library(readxl)
library(psych)
library(xlsx)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(ggpubr)
library(extrafont)
library(cowplot)

options(scipen = 999)
index_corr <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/CO.R.E./Project/Tables/correlation_indexes.xlsx")
index_corr <- data.frame(index_corr)

#### Normalizing

test=as.data.frame(index_corr)
country_order <- test$Country
test$Country <- NULL
test <- as_tibble(scale(test))
class(test)
test <- data.frame(test)

write.xlsx(test, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/CO.R.E./Project/Tables/all_index_normaliz1.xlsx")

#fit regression model
model_a <- lm(test$CORE~test$TI_var_20_21 + 
              test$EQI_var_17_21 +
              test$eGov_Bench_var_20_21 +
              test$UN_OnlineServIndex_20_22 +
              test$UN_E.GovIndex_20_22 +
              test$eu_special_baro_19_22)


model_a$fitted.values
#get model summary
model1 <-summary(model_a)
model1
mean(model1$residuals^2)
sd(model1$residuals)

#Finding the two-tailed t critical value
qt(p=.05/2, df=21, lower.tail=FALSE)


#fit regression model removing independent variables

model_b <- lm(test$CORE~test$TI_var_20_21 + 
              #test$EQI_var_17_21 +
              test$eGov_Bench_var_20_21 +
              test$UN_OnlineServIndex_20_22 +
              test$UN_E.GovIndex_20_22 +
              test$eu_special_baro_19_22)

model_b$fitted.values
#get model summary
model2<-summary(model_b)
model2
mean(model2$residuals^2)
sd(model2$residuals)
class(model2)

require(sjPlot)
core_model<-tab_model(model1, model2)



set_text_color(set_tb_borders(as_huxtable(model_summ)), "orange")

#Finding the two-tailed t critical value

qt(p=.05/2, df=22, lower.tail=FALSE)



#TEST 1 Model with all countries

data <- data.frame(pred = predict(model), actual = test$CORE)
countries <- index_corr$Country

ggplot(data, aes(pred, actual)) +
  geom_point() +
  geom_label_repel(aes(label = countries), size = 3) +
  scale_x_continuous(name="Z-scores for all the indexes excluding the SCO.R.E.") +
  scale_y_continuous(name="Z-scores for the SCO.R.E.") +
  #annotate("text", x=-1, y=1, label = c("Pearson Correlation Coefficient = 0.70")) + 
  stat_cor(method = "pearson", label.x = -3, label.y = 3) +
  geom_rangeframe() + 
  theme_bw() +
  #theme_set(theme_gray(base_size = 12, base_family = 'Helvetica')) + 
  geom_abline()


