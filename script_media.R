
library(tidyverse)
library(broom)
library(agricolae)
library(report)
library(dunn.test)
library(ggpubr)
library(rstatix)
library(flextable)
library(raster)
library(cmstatr)
library(MASS)
library(emmeans)
library(multcomp)
#library(emmGrid)
media.fls <- read_csv("data/data_entry_evaluation_FLS_sporulation_media_light copy.csv") 

#function removing outliers
get_range <- function(mynumber) {
  bb <- boxplot.stats(mynumber)
  cc <- bb$stats
  dd <- max(cc)
  ee <- min(cc)
  return(data.frame(upper = dd, lower = ee))
}


media.fls.2 <- 
  media.fls %>% mutate(
  ID = as.factor(ID),
  experimental_replicate = as.factor(experimental_replicate),
  media = as.factor(media),
  condition = as.factor(condition)
) %>%  rowwise() %>% mutate(average_conidia_per_square_hemocytometer = mean(
  c(
    `spores  upper square 1`,
    `spores upper square 2`,
    `spores upper square  3`,
    `spores upper square  4`,
    `spores  lower square 1`,
    `spores lower square 2`,
    `spores lower square  3`,
    `spores lower square  4`))) %>% dplyr::select(ID,experimental_replicate, media, condition, average_conidia_per_square_hemocytometer )

# good plot by media (DV8,DV8 filter paper, PDA,PGM,and SSLB) and by light regime (light, light-dark)
ggplot(data = media.fls.2,
       aes(x = media, y = average_conidia_per_square_hemocytometer)) + geom_boxplot(aes(color = media)) + facet_wrap( ~ condition) + geom_point(size =
                                                                                                                                                  2)+ theme(
                                                                                                                                                    plot.title = element_text(
                                                                                                                                                      size = 18,
                                                                                                                                                      face = "bold",
                                                                                                                                                      hjust = 0.5,
                                                                                                                                                      family = "Arial"
                                                                                                                                                    ),
                                                                                                                                                    axis.title = element_text(
                                                                                                                                                      size = 18,
                                                                                                                                                      face = "bold",
                                                                                                                                                      hjust = 0.5
                                                                                                                                                    ),
                                                                                                                                                    axis.text = element_text(
                                                                                                                                                      face = "bold",
                                                                                                                                                      size = 18,
                                                                                                                                                      family = "Arial"
                                                                                                                                                    ),
                                                                                                                                                    axis.text.y = element_text(
                                                                                                                                                      angle = 20,
                                                                                                                                                      hjust = 1),
                                                                                                                                                    panel.background = element_rect(fill = "white", colour = "grey50")
                                                                                                                                                  ) 



# filter out outliers by function get_range
media.fls.3 <-
  media.fls.2 %>% group_by(ID, media, condition) %>%  mutate(average_conidia_per_square_hemocytometer_range = list(get_range(
    average_conidia_per_square_hemocytometer
  ))) %>% unnest(cols = c(average_conidia_per_square_hemocytometer_range)) %>% filter(
    average_conidia_per_square_hemocytometer <= upper &
      average_conidia_per_square_hemocytometer >= lower
  ) %>% ungroup() 

# testing condition (light or dark-light) of object with no utliers

kruskal.test(media.fls.3$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.3$condition), data= media.fls.3 )
kruskal.condition <- tidy(kruskal.test(media.fls.3$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.3$condition), data= media.fls.3 ))
kruskal.condition.2 <- flextable::flextable(kruskal.condition %>% mutate(p.value =
                                                      as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.condition.2, path = "nice_table_kruskal_condition.docx")

#summary means condition (light or dark-light)

means.condition <- flextable(media.fls.3 %>% group_by(condition) %>% 
                               summarize(mean = mean(average_conidia_per_square_hemocytometer, na.rm=TRUE), sd = sd(average_conidia_per_square_hemocytometer, na.rm=TRUE), n = n(),
                                         se = sd / sqrt(n), cv= cv (average_conidia_per_square_hemocytometer))%>%
                               arrange(desc(mean)) %>% 
                               mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.condition, path = "means.condition.docx")

means.media <- flextable(media.fls.3 %>% group_by(media) %>% 
                               summarize(mean = mean(average_conidia_per_square_hemocytometer, na.rm=TRUE), sd = sd(average_conidia_per_square_hemocytometer, na.rm=TRUE), n = n(),
                                         se = sd / sqrt(n), cv= cv (average_conidia_per_square_hemocytometer))%>%
                               arrange(desc(mean)) %>% 
                               mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.media, path = "means.media_fls3.docx")



#same plots similar as above but now with no outliers and by ID

ggplot(data = media.fls.3,
       aes(x = media, y = average_conidia_per_square_hemocytometer)) + geom_boxplot() + facet_wrap(~ ID) + geom_point(aes(color =
                                                                                                      condition)) + theme(axis.text.x = element_text(
                                                                                                        face = "bold",
                                                                                                        size = 10,
                                                                                                        family = "Arial", 
                                                                                                        angle = 20
                                                                                                      )) 

#After removing outliers,  Good by experimental replicate in face wrap
#media (DV8,DV8 filter paper, PDA,PGM,and SSLB) and  light regime (light, light-dark) by experimental replicate

ggplot(data = media.fls.3,
       aes(x = media, y = average_conidia_per_square_hemocytometer)) + geom_boxplot(aes(color = media)) + facet_grid(  condition ~ experimental_replicate) + geom_point(size =
                                                                                                                                                                          2)+ theme(
                                                                                                                                                                            plot.title = element_text(
                                                                                                                                                                              size = 10,
                                                                                                                                                                              face = "bold",
                                                                                                                                                                              hjust = 0.5,
                                                                                                                                                                              family = "Arial"
                                                                                                                                                                            ),
                                                                                                                                                                            axis.title = element_text(
                                                                                                                                                                              size = 10,
                                                                                                                                                                              face = "bold",
                                                                                                                                                                              hjust = 0.5
                                                                                                                                                                            ),
                                                                                                                                                                            axis.text = element_text(
                                                                                                                                                                              face = "bold",
                                                                                                                                                                              size = 10,
                                                                                                                                                                              family = "Arial"
                                                                                                                                                                            ),
                                                                                                                                                                            axis.text.x = element_text(
                                                                                                                                                                              angle = 20,
                                                                                                                                                                              hjust = 1),
                                                                                                                                                                            panel.background = element_rect(fill = "white", colour = "grey50")
                                                                                                                                                                          ) +theme(legend.text=element_text(size=15), legend.title = element_text(size=15)) + theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20))





##Statistic ANalyses

##GLM model 

model1 <- aov(media.fls.3$average_conidia_per_square_hemocytometer~media.fls.3$media*media.fls.3$condition,media.fls.3)
summary.aov(model1)
model2 <-  aov(media.fls.3$average_conidia_per_square_hemocytometer~media.fls.3$media+media.fls.3$condition,media.fls.3)
summary.aov(model2)

#no normal distribution
residuals1 <- residuals(model1)
shapiro.test(residuals1)
levene_test(media.fls.3$average_conidia_per_square_hemocytometer~media.fls.3$media*media.fls.3$condition,media.fls.3)
residuals2 <- residuals(model2)
shapiro.test(residuals2)


#model1.GLM <- glm(average_conidia_per_square_hemocytometer ~ media * condition, family = poisson(link = "log"), data = media.fls.3)
summary(model1.GLM)

model1.GLM <- glm(average_conidia_per_square_hemocytometer ~ media * condition, family = negative.binomial(theta = 1), data = media.fls.3)
#Checking deviance residauls
qqnorm(residuals(model1.GLM, type = "deviance"))
qqline(residuals(model1.GLM, type = "deviance"), col = "red")

#Checking Overdispersion

# Check for overdispersion
# A ratio much greater than 1 suggests overdispersion, no overdisperosion
print(model1.GLM$deviance / model1.GLM$df.residual) 

anova_model1.GLM <- aov(average_conidia_per_square_hemocytometer ~ media * condition, data = media.fls.3)
Anova(model1.GLM, type = "III") # Or type = "II" depending on your needs
# Summarize the ANOVA results
summary(anova_model1.GLM)
# Perform Tukey's HSD test
tukey_results <- TukeyHSD(anova_model1.GLM)
# Print the results
print(tukey_results)



post_hoc_results <- emmeans(anova_model1.GLM, pairwise ~ media * condition, adjust = "tukey")

# View the results
summary(post_hoc_results)


# Get EMMs on the default log scale
emm_log_scale <- emmeans(model1.GLM, ~ media * condition)


# Get EMMs on the original response (count) scale
emm_response_scale <- emmeans(model1.GLM, ~ media * condition, type = "response")
#emmeans(model1.GLM, specs = ~ media * condition)
#Snippe5
cld_output <- cld(emm_response_scale, Letters = LETTERS, alpha = 0.05, 
                  adjust = "Tukey", # Adjust p-values (Tukey is default for pairwise)
                  # Use capital letters
                  sort = TRUE,reversed = T)
summary(cld_output)
print(cld_output)

cld_output.2 <- flextable::flextable(cld_output)
flextable::save_as_docx(cld_output.2, path = "pairwise_least_squares_means_tukey.docx")

#Plot of interactions
#Visualize the interaction
emmip(model1.GLM, media ~condition) +
  geom_point() +
  geom_line(aes(group = media)) +
  labs(y = "Expected Breaks (Response Scale)") +
  theme_minimal()
