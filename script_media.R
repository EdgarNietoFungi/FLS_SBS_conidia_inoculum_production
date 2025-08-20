
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
    `spores lower square  4`))) %>% select(ID,experimental_replicate, media, condition, average_conidia_per_square_hemocytometer )

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

#reduce/change shortly the name of  each levels and created a new column named "Treatment"from concatenating: "media, and condition" levels
media.fls.4 <- media.fls.3  %>% 
  mutate(media = fct_recode(media, "DV8p" = "DV8_filter_paper")) %>% 
  mutate(condition = fct_recode(condition, "lt" = "light")) %>% 
  mutate(condition = fct_recode(condition, "dk_lt" = "light-dark")) %>% 
  unite(treatment, media, condition, sep = "_") %>%
  mutate(treatment = as.factor(treatment)) 
###removing "light" condition, and switching written level light-dark to dark-light
media.fls.5 <-  media.fls.3 %>% 
  mutate(condition = fct_recode(condition, "dark-light" = "light-dark")) %>% 
  filter(condition=="dark-light")


#same plots similar as above but now with no outliers and by ID

ggplot(data = media.fls.3,
       aes(x = media, y = average_conidia_per_square_hemocytometer)) + geom_boxplot() + facet_wrap(~ ID) + geom_point(aes(color =
                                                                                                      condition)) + theme(axis.text.x = element_text(
                                                                                                        face = "bold",
                                                                                                        size = 10,
                                                                                                        family = "Arial", 
                                                                                                        angle = 20
                                                                                                      )) 

#modifyy and play previous factor to get better graph
# Good plot  media (DV8,DV8 filter paper, PDA,PGM,and SSLB)  by experimental replicate

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

# run good by media (DV8,DV8 filter paper, PDA,PGM,and SSLB)  by experimental replicate 
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
#media (DV8,DV8 filter paper, PDA,PGM,and SSLB) and   light regime (light, light-dark) by experimental replicate

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
#Shapiro test for normality
tidy(shapiro.test(media.fls.2$average_conidia_per_square_hemocytometer)) #i.e var dep

#with no  outliers
tidy(shapiro.test(media.fls.3$average_conidia_per_square_hemocytometer)) #i.e var dep

#by  object media fls5 (where "light" condition removed)

tidy(shapiro.test(media.fls.5$average_conidia_per_square_hemocytometer)) #i.e var dep

#Still the same, data are no normal! 

# Kruskal wallis test by media 
kruskal.test(media.fls.2$average_conidia_per_square_hemocytometer~ as.factor( media.fls.2$media), data= media.fls.2)
# Kruskal wallis test by media with no outliers
kruskal.test(media.fls.3$average_conidia_per_square_hemocytometer~ as.factor( media.fls.3$media), data= media.fls.3)
# Kruskal wallis test by treatment, the object "fls4"
kruskal.test(media.fls.4$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.4$treatment), data= media.fls.4 )
kruskal <- tidy(kruskal.test(media.fls.4$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.4$treatment), data= media.fls.4 ))
kruskal.2 <- flextable::flextable(kruskal%>% mutate(p.value =
                        as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.2, path = "nice_table_kruskal_treatment.docx")
# Kruskal wallis test by  media fls5 (where "light" condition removed)
kruskal.test(media.fls.5$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.5$media), data= media.fls.5 )
kruskal.media <- tidy(kruskal.test(media.fls.5$average_conidia_per_square_hemocytometer   ~ as.factor( media.fls.5$media), data= media.fls.5 ))
kruskal.media.2 <- flextable::flextable(kruskal.media%>% mutate(p.value =
                                                      as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.media.2, path = "nice_table_kruskal_media.docx")


# dunn test by condition (light or dark-light)
dunn.test(media.fls.2$average_conidia_per_square_hemocytometer , as.factor(media.fls.2$condition),method = 'bonferroni')
# dunn test by media (DV8,DV8 filter paper, PDA,PGM,and SSLB)
dunn.test(media.fls.2$average_conidia_per_square_hemocytometer , as.factor(media.fls.2$media),method = 'bonferroni')

## dunn test with no outliers by condition (light or dark-light)
dunn.test(media.fls.3$average_conidia_per_square_hemocytometer , as.factor(media.fls.3$condition),method = 'bonferroni')
## dunn test with no outliers by media (DV8,DV8 filter paper, PDA,PGM,and SSLB)
dunn.test(media.fls.3$average_conidia_per_square_hemocytometer , as.factor(media.fls.3$media),method = 'bonferroni')
#dunn test with no outliers by media by treatment fls4 
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

# dunn test of object fls5 with no outliers by media (DV8,DV8 filter paper, PDA,PGM,and SSLB) in "light" condition removed)
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


#Graph by treatment  of object fls4  with no outliers

View(stat.test <- media.fls.4 %>% dunn_test(average_conidia_per_square_hemocytometer ~ treatment, p.adjust.method = "bonferroni"))
stat.test <- stat.test %>% add_xy_position(x = "treatment")
View(stat.test)

ggboxplot(media.fls.4, x = "treatment", y = "average_conidia_per_square_hemocytometer", fill = "treatment") +
  stat_pvalue_manual(stat.test, hide.ns = FALSE)
  

#Grapht  of object fls5  with no outliers by media where ("light" condition removed)

View(stat.test.media <- media.fls.5 %>% dunn_test(average_conidia_per_square_hemocytometer ~ media, p.adjust.method = "bonferroni"))
stat.test.media <- stat.test.media%>% add_xy_position(x = "media")
View(stat.test.media)


ggboxplot(media.fls.5,
          x = "media",
          y = "average_conidia_per_square_hemocytometer",
          fill = "media") +
  stat_pvalue_manual(stat.test.media, hide.ns = FALSE) + theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(
      face = "bold",
      size = 15,
      family = "Arial",
      angle = 10,
      hjust = 1
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 15,
      family = "Arial"
    ),
    panel.background = element_rect(fill = "white", colour = "grey50"), legend.text = element_text(size = 15), legend.title = element_text(size = 15)
  ) + scale_y_continuous(limits = c(0, 200)) +    labs(y = "average_conidia_per_square(mm2)_hemocytometer") # Rename the y-axis
+    labs(y = expression (bold("average_conidia_per_mm"^2*"_hemocytometer"))) # Rename the y-axis

