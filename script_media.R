
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
media.fls <- read_csv("data/data_entry_evaluation_FLS_sporulation_media_light copy.csv") 

#functions
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
    `spores lower square  4`))) %>% select(ID,experimental_replicate, media, condition, average_conidia_per_square_hemocytometer )

#media.fls.3 <- media.fls.2 %>% group_by(ID, media, condition) %>% 
  
  
  
  
  
  #s#ummarize(aver_upper_squa_1 = mean(`spores  upper square 1`),
                                                                           aver_upper_squa_2 = mean(`spores upper square 2`),                        
                                                                           aver_upper_squa_3 = mean(`spores upper square  3`),
                                                                              aver_upper_squa_4 = mean(`spores upper square  4`),
                                                                          aver_lower_squa_1 = mean(`spores  lower square 1`),
                                                                         aver_lower_squa_2 = mean(`spores lower square 2`), 
                                                                        aver_lower_squa_3 = mean(`spores lower square  3`),
                                                                         aver_lower_squa_4 = mean(`spores lower square  4`)
                                                                                                   
                                                                                                   ) %>% ungroup()
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          



#%>% rename("growth_14days"=growth...6) %>% group_by(ID, experimental_replicate,pathogen,condition) #%>% summarise(mean(growth...6))


# media.fls.3 <- media.fls.2 %>% rowwise() %>% mutate(average_upper = mean(
#   c(
#     aver_upper_squa_1,
#     aver_upper_squa_2,
#     aver_upper_squa_3,
#     aver_upper_squa_4
#   )
# )) %>% mutate(average_lower = mean(
#   c(
#     aver_lower_squa_1,
#     aver_lower_squa_2,
#     aver_lower_squa_3,
#     aver_lower_squa_4
#   )
# ))%>% select(ID, media, condition, average_upper, average_lower)
# 
# 
# 
# good
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



ggplot(data = substratum.inoculum.2,
       aes(x = substratum, y = growth_14days)) + geom_boxplot() + facet_wrap(~ ID) + geom_point(aes(color =
                                                                                                   condition, shape = pathogen), size = 3)
# filter out outliers by function get_range
media.fls.3 <-
  media.fls.2 %>% group_by(ID, media, condition) %>%  mutate(average_conidia_per_square_hemocytometer_range = list(get_range(
    average_conidia_per_square_hemocytometer
  ))) %>% unnest(cols = c(average_conidia_per_square_hemocytometer_range)) %>% filter(
    average_conidia_per_square_hemocytometer <= upper &
      average_conidia_per_square_hemocytometer >= lower
  ) %>% ungroup() 

# testing condition
kruskal.test(media.fls.3$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.3$condition), data= media.fls.3 )
kruskal.condition <- tidy(kruskal.test(media.fls.3$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.3$condition), data= media.fls.3 ))
kruskal.condition.2 <- flextable::flextable(kruskal.condition %>% mutate(p.value =
                                                      as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.condition.2, path = "nice_table_kruskal_condition.docx")

#summary means condition

means.condition <- flextable(media.fls.3 %>% group_by(condition) %>% 
                               summarize(mean = mean(average_conidia_per_square_hemocytometer, na.rm=TRUE), sd = sd(average_conidia_per_square_hemocytometer, na.rm=TRUE), n = n(),
                                         se = sd / sqrt(n), cv= cv (average_conidia_per_square_hemocytometer))%>%
                               arrange(desc(mean)) %>% 
                               mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.condition, path = "means.condition.docx")

# cocnatani unite two groups i one columns and reduce the name of it
media.fls.4 <- media.fls.3  %>% 
  mutate(media = fct_recode(media, "DV8p" = "DV8_filter_paper")) %>% 
  mutate(condition = fct_recode(condition, "lt" = "light")) %>% 
  mutate(condition = fct_recode(condition, "dk_lt" = "light-dark")) %>% 
  unite(treatment, media, condition, sep = "_") %>%
  mutate(treatment = as.factor(treatment)) 
###removing "light" condition
media.fls.5 <-  media.fls.3 %>% 
  mutate(condition = fct_recode(condition, "dark-light" = "light-dark")) %>% 
  filter(condition=="dark-light")


#same but better with no outliers

ggplot(data = media.fls.3,
       aes(x = media, y = average_conidia_per_square_hemocytometer)) + geom_boxplot() + facet_wrap(~ ID) + geom_point(aes(color =
                                                                                                      condition)) + theme(axis.text.x = element_text(
                                                                                                        face = "bold",
                                                                                                        size = 10,
                                                                                                        family = "Arial", 
                                                                                                        angle = 20
                                                                                                      )) 

#mody and play previous factor to get better graph
# Good by experimental replicate

ggplot(data = media.fls.2,
aes(x = media, y = average_conidia_per_square_hemocytometer)) + geom_boxplot(aes(color = experimental_replicate))  + geom_point(size =
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

# run good by experimental replicate
ggplot(data = media.fls.3,
       aes(x = media, y = average_conidia_per_square_hemocytometer)) + geom_boxplot(aes(color = experimental_replicate))  + geom_point(size =
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



# Good by experimental replicate in face wrap
ggplot(data = media.fls.2,
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

#After removing outliers,  Good by experimental replicate in face wrap

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






#Shapiro test for normality
tidy(shapiro.test(media.fls.2$average_conidia_per_square_hemocytometer)) #i.e var dep

#report(tidy(shapiro.test(media.fls.2$average_conidia_per_square_hemocytometer))) #i.e var dep

#no outliers
tidy(shapiro.test(media.fls.3$average_conidia_per_square_hemocytometer)) #i.e var dep

#report(tidy(shapiro.test(media.fls.3$average_conidia_per_square_hemocytometer))) #i.e var dep

#by  media fls5 ("light" condition removed)

tidy(shapiro.test(media.fls.5$average_conidia_per_square_hemocytometer)) #i.e var dep

#report(tidy(shapiro.test(media.fls.5$average_conidia_per_square_hemocytometer))) #i.e var dep

# Kruskal wallis

kruskal.test(media.fls.2$average_conidia_per_square_hemocytometer~ as.factor( media.fls.2$media), data= media.fls.2)


kruskal.test(media.fls.3$average_conidia_per_square_hemocytometer~ as.factor( media.fls.3$media), data= media.fls.3)
#by treatment fls4
kruskal.test(media.fls.4$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.4$treatment), data= media.fls.4 )
kruskal <- tidy(kruskal.test(media.fls.4$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.4$treatment), data= media.fls.4 ))
kruskal.2 <- flextable::flextable(kruskal%>% mutate(p.value =
                        as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.2, path = "nice_table_kruskal_treatment.docx")
#by  media fls5 ("light" condition removed)
kruskal.test(media.fls.5$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.5$media), data= media.fls.5 )
kruskal.media <- tidy(kruskal.test(media.fls.5$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.5$media), data= media.fls.5 ))
kruskal.media.2 <- flextable::flextable(kruskal.media%>% mutate(p.value =
                                                      as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.media.2, path = "nice_table_kruskal_media.docx")


# dunn test

dunn.test(media.fls.2$average_conidia_per_square_hemocytometer , as.factor(media.fls.2$condition),method = 'bonferroni')

dunn.test(media.fls.2$average_conidia_per_square_hemocytometer , as.factor(media.fls.2$media),method = 'bonferroni')

#no outliers


dunn.test(media.fls.3$average_conidia_per_square_hemocytometer , as.factor(media.fls.3$condition),method = 'bonferroni')

dunn.test(media.fls.3$average_conidia_per_square_hemocytometer , as.factor(media.fls.3$media),method = 'bonferroni')
#by treatment fls4 
test_dunn <- dunn.test(media.fls.4$average_conidia_per_square_hemocytometer , as.factor(media.fls.4$treatment),method = 'bonferroni')
#  test_dunn<-flextable::flextable( dunn.test(media.fls.4$average_conidia_per_square_hemocytometer , as.factor(media.fls.4$treatment),method = 'bonferroni'))
#  flextable::save_as_docx(test_dunn, path = "nice_table_test_dunn.docx")
x <- test_dunn$comparisons
y <- test_dunn$P.adjusted
z <- test_dunn$Z
x_name <- "comparisons"
y_name <- "P.adjusted"
z_name <- "Z"
df <- data.frame(x,z, y)
names(df) <- c(x_name,z_name, y_name)
df

df.2 <- df%>%  mutate(P.adjusted =
                as.character(signif(P.adjusted, digits =1)))%>% 
  mutate(Z =                 as.character(signif(Z, digits =2)))

df.3 <- flextable::flextable(df.2)
flextable::save_as_docx(df.3, path = "nice_table_test_dunn_treatment.docx")

#by media fls5 ("light" condition removed)
test_dunn.media <- dunn.test(media.fls.5$average_conidia_per_square_hemocytometer , as.factor(media.fls.5$media),method = 'bonferroni')
# test_dunn.media<-flextable::flextable( dunn.test(media.fls.5$average_conidia_per_square_hemocytometer , as.factor(media.fls.5$media),method = 'bonferroni'))
# flextable::save_as_docx(test_dunn.media, path = "nice_table_test_dunn_media.docx")
x.media <- test_dunn.media$comparisons
y.media <- test_dunn.media$P.adjusted
z.media <- test_dunn.media$Z
x_name <- "comparisons"
y_name <- "P.adjusted"
z_name <- "Z"
mex <- data.frame(x.media,z.media, y.media)
names(mex) <- c(x_name,z_name,y_name)
mex

mex.2 <- mex%>%  mutate(P.adjusted =
                        as.character(signif(P.adjusted, digits =1)))%>%
  mutate(Z =                 as.character(signif(Z, digits =2)))

mex.3 <- flextable::flextable(mex.2)
flextable::save_as_docx(mex.3, path = "nice_table_test_dunn_media.docx")


#summary means media


means.media <- flextable(media.fls.5 %>% group_by(media) %>% 
                           summarize(mean = mean(average_conidia_per_square_hemocytometer, na.rm=TRUE), sd = sd(average_conidia_per_square_hemocytometer, na.rm=TRUE), n = n(),
                                     se = sd / sqrt(n), cv= cv (average_conidia_per_square_hemocytometer)) %>%
                           arrange(desc(mean)) %>% 
                               mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.media, path = "means.media.docx")


#Grapht treatment fls4 

View(stat.test <- media.fls.4 %>% dunn_test(average_conidia_per_square_hemocytometer ~ treatment, p.adjust.method = "bonferroni"))
stat.test <- stat.test %>% add_xy_position(x = "treatment")
View(stat.test)

ggboxplot(media.fls.4, x = "treatment", y = "average_conidia_per_square_hemocytometer", fill = "treatment") +
  stat_pvalue_manual(stat.test, hide.ns = FALSE)
  

#Grapht media fls5 ("light" condition removed)
View(stat.test.media <- media.fls.5 %>% dunn_test(average_conidia_per_square_hemocytometer ~ media, p.adjust.method = "bonferroni"))
stat.test.media <- stat.test.media%>% add_xy_position(x = "media")
View(stat.test.media)


ggboxplot(media.fls.5, x = "media", y = "average_conidia_per_square_hemocytometer", fill = "media") +
  stat_pvalue_manual(stat.test.media, hide.ns = FALSE)

