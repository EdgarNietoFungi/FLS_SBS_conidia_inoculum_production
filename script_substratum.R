
library(tidyverse)
library(broom)
library(ezec)
library(DescTools)
#library(rstatix)
#library(ggpubr)
library(agricolae)
library(multcompView)
library(ggpubr)
library(dunn.test)
library(cmstatr)
library(flextable)
#library(dunn.test)
#library(plyr)


substratum.inoculum <- read_csv("data/substratum_inoculum_growth_copy.csv") 

substratum.inoculum.2 <- 
  substratum.inoculum %>% mutate(
    ID = as.factor(ID),
    experimental_replicate = as.factor(experimental_replicate),
    pathogen = as.factor(pathogen),
    substratum = as.factor(substratum),
    condition = as.factor(condition)
  ) %>% rename("growth_14days"=growth...6) %>% 
  rename("growth_30days"=growth...8) %>%  
  group_by(ID, pathogen, condition, substratum) %>% 
  mutate(ind = sum(is.na(growth_14days))) %>%
  ungroup() %>% 
    filter(!ind>= 4)%>%
  select(-ind) %>%  ungroup()

#removing SBS_rice_suspension since CV is high

substratum.inoculum.3 <- substratum.inoculum.2%>% filter(condition!="suspension"| pathogen !="SBS"| substratum !="rice")

substratum.inoculum.3.alternative <- substratum.inoculum.2 %>% filter(condition=="suspension") %>% 
  mutate(condition = fct_recode(condition, "susp" = "suspension")) %>%
  mutate(substratum = fct_recode(substratum, "mil" = "millet")) %>%
  mutate(substratum = fct_recode(substratum, "sor" = "sorghum")) %>%
  #unite(treatment, pathogen, substratum, condition, sep = "_") 
  mutate(treatment = paste(pathogen, substratum, condition, sep = '_')) %>% 
  mutate(treatment = as.factor(treatment)) 

#
# silica.gel.2 <- silica.gel %>% mutate(
#   ID = as.factor(ID),
#   Fungi = as.factor(Fungi),
#   condition = as.factor(condition),
#   Revived = as.factor(Revived)) 

#checking replicates

ggplot(data = substratum.inoculum.2,
aes(x = substratum, y = growth_14days,color =
experimental_replicate)) + geom_boxplot()  + geom_point() + theme(legend.key.size = unit(1.5, "cm"),
legend.key.width = unit(0.5,"cm"))+ geom_jitter()


#checking condition
ggplot(data = substratum.inoculum.2,
aes(x = substratum, y = growth_14days,color =
condition)) + geom_boxplot()  + geom_point() + theme(legend.key.size = unit(1.5, "cm"),
legend.key.width = unit(0.5,"cm"))+ geom_jitter()

#ggplot(
 # data = substratum.inoculum.2,
  #aes(x = substratum, y = growth_14days,colour = pathogen,
   #   shape = condition)) + geom_boxplot() + facet_wrap( ~ experimental_replicate)


# very good, improve the background
ggplot(data = substratum.inoculum.2,
       aes(x = substratum, y = growth_14days)) + geom_boxplot() + facet_wrap(~ ID) + geom_point(aes(color =
                                                                                                   condition, shape = pathogen), size = 3) + geom_jitter()
# very good, improve the background
ggplot(data = substratum.inoculum.2,
       aes(x = substratum, y = growth_14days)) + geom_boxplot() + facet_wrap(~ ID) + geom_point(aes(color =
                                                                                                      experimental_replicate), size = 3) + theme(legend.key.size = unit(1.5, "cm"),
                                                                                                                                                 legend.key.width = unit(0.5,"cm"))+ geom_jitter()

# fair, improve the background
ggplot(data = substratum.inoculum.2,
       aes(x = substratum, y = growth_14days)) + geom_boxplot() + facet_wrap(~ experimental_replicate) + geom_point(aes(color =
                                                                                                      condition, shape = pathogen), size = 3) + theme(legend.key.size = unit(1.5, "cm"),
                                                                                                                                                      legend.key.width = unit(0.5,"cm"))

# Publication


ggplot(data = substratum.inoculum.2,
       aes(x = substratum, y = growth_14days)) + geom_boxplot() + facet_wrap(~ experimental_replicate) + geom_point(aes(color =
                                                                                                                          condition, shape = pathogen), size = 3) + theme(
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
                                                                                                                          ) +theme(legend.text=element_text(size=18), legend.title = element_text(size=20))



##gooodd Publuication

ggplot(data = substratum.inoculum.2,
       aes(x = substratum, y = growth_14days)) + geom_boxplot() + facet_wrap(~ ID) + geom_point(aes(color =
                                                                                                      condition, shape = pathogen), size = 3) + theme(
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
                                                                                                          size = 18,angle = 20,
                                                                                                          family = "Arial"
                                                                                                        ),
                                                                                                        axis.text.y = element_text(
                                                                                                          angle = 20,
                                                                                                          hjust = 1),
                                                                                                        panel.background = element_rect(fill = "white", colour = "grey50")
                                                                                                      ) +theme(legend.text=element_text(size=18), legend.title = element_text(size=20))+theme(strip.text.x = element_text(size = 20))




#testing condition
kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$condition), data= substratum.inoculum.3 )
kruskal.condition.subs <- tidy(kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$condition), data= substratum.inoculum.3 ))
kruskal.condition.subs.2 <- flextable::flextable(kruskal.condition.subs %>% mutate(p.value =
                                                                           as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.condition.subs.2, path = "nice_table_kruskal_subst_condition.docx")

###means condition
means.condition <- flextable(substratum.inoculum.2 %>% group_by(condition) %>% 
                               summarize(mean = mean(growth_14days, na.rm=TRUE), sd = sd(growth_14days, na.rm=TRUE), n = n(),
                                         se = sd / sqrt(n),cv= (se/mean)*100 )%>%
                               arrange(desc(mean)) %>% 
                               mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.condition, path = "means.condition.docx")

####Dunn test by condition  
test_dunn.subs <- dunn.test(substratum.inoculum.3$growth_14days , as.factor(substratum.inoculum.3$condition),method = 'bonferroni')
# test_dunn.media<-flextable::flextable( dunn.test(media.fls.5$average_conidia_per_square_hemocytometer , as.factor(media.fls.5$media),method = 'bonferroni'))
# flextable::save_as_docx(test_dunn.media, path = "nice_table_test_dunn_media.docx")
x.subs <- test_dunn.subs$comparisons
y.subs <- test_dunn.subs$P.adjusted
z.subs <- test_dunn.subs$Z
x_name <- "comparisons"
y_name <- "P.adjusted"
z_name <- "Z"
italy <- data.frame(x.subs,z.subs,y.subs)
names(italy) <- c(x_name,z_name,y_name)
italy

italy.2 <- italy%>%  mutate(P.adjusted =
                          as.character(signif(P.adjusted, digits =1)))%>%
  mutate(Z =                 as.character(signif(Z, digits =2)))

italy.3 <- flextable::flextable(italy.2)
flextable::save_as_docx(italy.3, path = "nice_table_test_dunn_substratum_condition.docx")



#testing substratum(media)
kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$substratum), data= substratum.inoculum.3 )
kruskal.media.subs <- tidy(kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$substratum), data= substratum.inoculum.3 ))
kruskal.media.subs.2 <- flextable::flextable(kruskal.media.subs %>% mutate(p.value =
                                                                                     as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.media.subs.2, path = "nice_table_kruskal_subst_media.docx")

###means substratum
means.substratum <- flextable(substratum.inoculum.2 %>% group_by(substratum) %>% 
                                summarize(mean = mean(growth_14days, na.rm=TRUE), sd = sd(growth_14days, na.rm=TRUE), n = n(),
                                          se = sd / sqrt(n), cv= (se/mean)*100 )%>%
                                arrange(desc(mean)) %>%
                                mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.substratum, path = "means.substratum.docx")

####Dunn test by substratum(media)  
test_dunn.subs <- dunn.test(substratum.inoculum.3$growth_14days , as.factor(substratum.inoculum.3$substratum),method = 'bonferroni')
# test_dunn.media<-flextable::flextable( dunn.test(media.fls.5$average_conidia_per_square_hemocytometer , as.factor(media.fls.5$media),method = 'bonferroni'))
# flextable::save_as_docx(test_dunn.media, path = "nice_table_test_dunn_media.docx")
x.subs <- test_dunn.subs$comparisons
y.subs <- test_dunn.subs$P.adjusted
z.subs <- test_dunn.subs$Z
x_name <- "comparisons"
y_name <- "P.adjusted"
z_name <- "Z"
spain <- data.frame(x.subs,z.subs,y.subs)
names(spain) <- c(x_name,z_name,y_name)
spain

spain.2 <- spain%>%  mutate(P.adjusted =
                              as.character(signif(P.adjusted, digits =1)))%>%
  mutate(Z =                 as.character(signif(Z, digits =2)))

spain.3 <- flextable::flextable(spain.2)
flextable::save_as_docx(spain.3, path = "nice_table_test_dunn_substratum_media.docx")

#testing pathogen
kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$pathogen), data= substratum.inoculum.3 )
kruskal.pathogen.subs <- tidy(kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$pathogen), data= substratum.inoculum.3 ))
kruskal.pathogen.subs.2 <- flextable::flextable(kruskal.pathogen.subs %>% mutate(p.value =
                                                                                     as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.pathogen.subs.2, path = "nice_table_kruskal_subst_pathogen.docx")

###means pathogen
means.pathogen <- flextable(substratum.inoculum.2 %>% group_by(pathogen) %>% 
                               summarize(mean = mean(growth_14days, na.rm=TRUE), sd = sd(growth_14days, na.rm=TRUE), n = n(),
                                         se = sd / sqrt(n), cv= (se/mean)*100) %>%
                               arrange(desc(mean)) %>% 
                               mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.pathogen, path = "means.pathogen.docx")

####Dunn test by pathogen  
test_dunn.pathogen <- dunn.test(substratum.inoculum.3$growth_14days , as.factor(substratum.inoculum.3$pathogen),method = 'bonferroni')
# test_dunn.media<-flextable::flextable( dunn.test(media.fls.5$average_conidia_per_square_hemocytometer , as.factor(media.fls.5$media),method = 'bonferroni'))
# flextable::save_as_docx(test_dunn.media, path = "nice_table_test_dunn_media.docx")
x.pathogen <- test_dunn.pathogen$comparisons
y.pathogen <- test_dunn.pathogen$P.adjusted
z.pathogen <- test_dunn.pathogen$Z
x_name <- "comparisons"
y_name <- "P.adjusted"
z_name <- "Z"
france <- data.frame(x.pathogen,z.pathogen,y.pathogen)
names(france) <- c(x_name,z_name,y_name)
france

france.2 <- france%>%  mutate(P.adjusted =
                              as.character(signif(P.adjusted, digits =1)))%>%
  mutate(Z =                 as.character(signif(Z, digits =2)))

france.3 <- flextable::flextable(france.2)
flextable::save_as_docx(france.3, path = "nice_table_test_dunn_substratum_pathogen.docx")










# removing condition susp and cocnatani unite two groups i one columns and reduce the name of it
substratum.inoculum.4 <- substratum.inoculum.3  %>% 
  filter(condition=="suspension") %>% 
  mutate(condition = fct_recode(condition, "susp" = "suspension")) %>%
  mutate(substratum = fct_recode(substratum, "mil" = "millet")) %>%
  mutate(substratum = fct_recode(substratum, "sor" = "sorghum")) %>%
  #unite(treatment, pathogen, substratum, condition, sep = "_") 
   mutate(treatment = paste(pathogen, substratum, condition, sep = '_')) %>% 
  mutate(treatment = as.factor(treatment)) 

###removing light condition
# media.fls.5 <-  media.fls.3 %>%
#   mutate(condition = fct_recode(condition, "dark-light" = "light-dark")) %>%
#   filter(condition=="dark-light")

# data_analysis
#Shapiro test for normality
tidy(shapiro.test(substratum.inoculum.2$growth_14days)) #i.e var dep

#report(tidy(shapiro.test(substratum.inoculum.2$growth_14days))) #i.e var dep

# Kruskal wallis for every group

tidy(kruskal.test(substratum.inoculum.2$growth_14days~ as.factor( substratum.inoculum.2$pathogen), data= substratum.inoculum.2))

#by treatment subt 3
kruskal.test(substratum.inoculum.4$growth_14days~ as.factor( substratum.inoculum.4$treatment), data= substratum.inoculum.4)
kruskal <- tidy(kruskal.test(substratum.inoculum.4$growth_14days   ~ as.factor( substratum.inoculum.4$treatment), data= substratum.inoculum.4))

kruskal.2 <- flextable::flextable(kruskal%>% mutate(p.value =
                                                      as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.2, path = "nice_table_kruskal_subtratum_treatment.docx")

#Dunn test
#by treatment subst 3
test_dunn.subs <- dunn.test(substratum.inoculum.4$growth_14days , as.factor(substratum.inoculum.4$treatment),method = 'bonferroni')
# test_dunn.media<-flextable::flextable( dunn.test(media.fls.5$average_conidia_per_square_hemocytometer , as.factor(media.fls.5$media),method = 'bonferroni'))
# flextable::save_as_docx(test_dunn.media, path = "nice_table_test_dunn_media.docx")
x.subs <- test_dunn.subs$comparisons
y.subs <- test_dunn.subs$P.adjusted
z.subs <- test_dunn.subs$Z
x_name <- "comparisons"
y_name <- "P.adjusted"
z_name <- "Z"
usa <- data.frame(x.subs,z.subs,y.subs)
names(usa) <- c(x_name,z_name,y_name)
usa

usa.2 <- usa%>%  mutate(P.adjusted =
                          as.character(signif(P.adjusted, digits =1)))%>%  
  mutate(Z =                 as.character(signif(Z, digits =2)))

usa.3 <- flextable::flextable(usa.2)
flextable::save_as_docx(usa.3, path = "nice_table_test_dunn_subs_treatment.docx")


####means treatment
means.treatment <- flextable(substratum.inoculum.3.alternative %>% group_by(treatment) %>% 
                               summarize(mean = mean(growth_14days, na.rm=TRUE), sd = sd(growth_14days, na.rm=TRUE), n = n(),
                                         se = sd / sqrt(n), cv= (se/mean)*100) %>%
                               arrange(desc(mean)) %>% 
                               mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.treatment, path = "means.treatment.docx")




#Grapht by treatment 
View(stat.test.subs <- substratum.inoculum.4%>% dunn_test(growth_14days ~ treatment, p.adjust.method = "bonferroni"))
stat.test.subs <- stat.test.subs%>% add_xy_position(x = "treatment", step.increase = 0.5)
View(stat.test.subs)


ggboxplot(substratum.inoculum.4, x = "treatment", y = "growth_14days", fill = "treatment") +
  stat_pvalue_manual(stat.test.subs, hide.ns = FALSE)+ theme(
  panel.border = element_rect(
    colour = "black",
    fill = NA,
    size = 1
  ),
  axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
  axis.text.x = element_text(
    face = "bold",
    size = 9,
    family = "Arial",
    angle = 10,
    hjust = 1
  ),
  axis.text.y = element_text(
    face = "bold",
    size = 9,
    family = "Arial"
  ),
  panel.background = element_rect(fill = "white", colour = "grey50")
) 

# +    labs(y = "growth_28days") # Rename the y-axis

