
library(tidyverse)
library(broom)
library(ezec)
library(DescTools)
library(rstatix)
#library(ggpubr)
library(agricolae)
library(multcompView)
library(ggpubr)
library(dunn.test)
library(cmstatr)
library(flextable)
library(dunn.test)
library(read)
library(officer)
#library(plyr)


substratum.inoculum <- read_csv("data/substratum_inoculum_growth_copy.csv") 
#Reading and removing replicates with ≥ 4 with NA  that means CONTAMINATIONS (4 is half of the total of repeats per experimental unit) 
substratum.inoculum.2 <- 
  substratum.inoculum %>% mutate(
    ID = as.factor(ID),
    experimental_replicate = as.factor(experimental_replicate),
    pathogen = as.factor(pathogen),
    substratum = as.factor(substratum),
    condition = as.factor(condition)
  ) %>% rename("growth_14days"=growth...6) %>% 
  rename("growth_30days"=growth...8) %>% 
  mutate(pathogen = fct_recode(pathogen, "C. sojina" = "FLS", "S. glycines" = "SBS"))%>%
  group_by(ID, pathogen, condition, substratum) %>% 
  mutate(ind = sum(is.na(growth_14days))) %>%
  ungroup() %>% 
    filter(!ind>= 4)%>%
  dplyr::select(-ind) %>%  ungroup()

#CREATING OBJECTS
# object "substratum.inoculum.3" NO removing SBS_rice_suspension since CV is high and renaming levels of condition (plug or conidia)

substratum.inoculum.3 <- substratum.inoculum.2 #%>% filter(condition!="suspension"| pathogen !="Sglycines"| substratum !="rice")

# object "substratum.inoculum.3" alternative; to use further in the script
substratum.inoculum.3.alternative <- substratum.inoculum.2 %>% filter(condition=="suspension") %>% 
  mutate(condition = fct_recode(condition, "susp" = "suspension")) %>%
  mutate(substratum = fct_recode(substratum, "mil" = "millet")) %>%
  mutate(substratum = fct_recode(substratum, "sor" = "sorghum")) %>%
  #unite(treatment, pathogen, substratum, condition, sep = "_") 
  mutate(treatment = paste(pathogen, substratum, condition, sep = '_')) %>% 
  mutate(treatment = as.factor(treatment)) 

#VISULAZITAION OF DATA on variable"growth_14days"
#checking replicates by substratum (sorghum, millet or rice) 

ggplot(data = substratum.inoculum.2,
aes(x = substratum, y = growth_14days,color =
experimental_replicate)) + geom_boxplot()  + geom_point() + theme(legend.key.size = unit(1.5, "cm"),
legend.key.width = unit(0.5,"cm"))+ geom_jitter()


#checking condition (plug or conidia) by substratum (sorghum, millet or rice) 
ggplot(data = substratum.inoculum.2,
aes(x = substratum, y = growth_14days,color =
condition)) + geom_boxplot()  + geom_point() + theme(legend.key.size = unit(1.5, "cm"),
legend.key.width = unit(0.5,"cm"))+ geom_jitter()

# very good plot  substratum (sorghum, millet or rice) by IDs (24,35,42,46,201,775) by condition (plug or conidia) and by pathogen (FLS or SBS), improve the background
ggplot(data = substratum.inoculum.2,
       aes(x = substratum, y = growth_14days)) + geom_boxplot() + facet_wrap(~ ID) + geom_point(aes(color =
                                                                                                   condition, shape = pathogen), size = 3) + geom_jitter()
# very good plot substratum (sorghum, millet or rice) by IDs (24,35,42,46,201,775) by condition (plug or conidia) and by pathogen (FLS or SBS), improve the background
ggplot(data = substratum.inoculum.2,
       aes(x = substratum, y = growth_14days)) + geom_boxplot() + facet_wrap(~ ID) + geom_point(aes(color =
                                                                                                      experimental_replicate), size = 3) + theme(legend.key.size = unit(1.5, "cm"),
                                                                                                                                                 legend.key.width = unit(0.5,"cm"))+ geom_jitter()

# fair plot substratum (sorghum, millet or rice)  by experimental replication,   by condition (plug or conidia) and by pathogen (FLS or SBS), improve the background
ggplot(data = substratum.inoculum.2,
       aes(x = substratum, y = growth_14days)) + geom_boxplot() + facet_wrap(~ experimental_replicate) + geom_point(aes(color =
                                                                                                      condition, shape = pathogen), size = 3) + theme(legend.key.size = unit(1.5, "cm"),
                                                                                                                                                      legend.key.width = unit(0.5,"cm"))

# Publication plot substratum (sorghum, millet or rice) by experimental replication,   by condition (plug or conidia) and by pathogen (FLS or SBS)
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



##good plot substratum (sorghum, millet or rice) by experimental replication,   by condition (plug or conidia) and by pathogen (FLS or SBS) for publication

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



#STATISTICAL ANALYSIS for the variable "growth_14days": when using summarize, used original table without removing rice 
#Shapiro test for normality

tidy(shapiro.test(substratum.inoculum.2$growth_14days)) #i.e var dep

# Kruskal wallis for every group

tidy(kruskal.test(substratum.inoculum.2$growth_14days~ as.factor( substratum.inoculum.2$pathogen), data= substratum.inoculum.2))

#TESTING KRUSKAL WALLIS SINCE NO NORMALITY

#testing condition (plug or conidia)
kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$condition), data= substratum.inoculum.3 )
kruskal.condition <- tidy(kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$condition), data= substratum.inoculum.3 ))
kruskal.condition.2 <- flextable::flextable(kruskal.condition %>% mutate(p.value =
                                                                           as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.condition.2, path = "nice_table_kruskal_subst_condition.docx")

###means condition (plug or  conidia)
means.condition <- flextable(substratum.inoculum.2 %>% group_by(condition) %>% 
                               summarize(mean = mean(growth_14days, na.rm=TRUE), sd = sd(growth_14days, na.rm=TRUE), n = n(),
                                         se = sd / sqrt(n),cv= (se/mean)*100 )%>%
                               arrange(desc(mean)) %>% 
                               mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.condition, path = "means.condition.docx")

####Dunn test by condition (plug or conidia)
test_dunn <- dunn.test(substratum.inoculum.3$growth_14days , as.factor(substratum.inoculum.3$condition),method = 'bonferroni')
x <- test_dunn$comparisons
y <- test_dunn$P.adjusted
z <- test_dunn$Z
x_name <- "comparisons"
y_name <- "P.adjusted"
z_name <- "Z"
italy <- data.frame(x,z,y)
names(italy) <- c(x_name,z_name,y_name)
italy

italy.2 <- italy%>%  mutate(P.adjusted =
                          as.character(signif(P.adjusted, digits =1)))%>%
  mutate(Z =                 as.character(signif(Z, digits =2)))

italy.3 <- flextable::flextable(italy.2)
flextable::save_as_docx(italy.3, path = "nice_table_test_dunn_substratum_condition.docx")



#testing substratum aka known as "media" to avoid confusions with the original name of the object (sorghum, millet or rice)
kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$substratum), data= substratum.inoculum.3 )
kruskal.media <- tidy(kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$substratum), data= substratum.inoculum.3 ))
kruskal.media.2 <- flextable::flextable(kruskal.media %>% mutate(p.value =
                                                                                     as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.media.2, path = "nice_table_kruskal_subst_media.docx")

###means substratum aka known as "media" to avoid confusions with the original name of the object (sorghum, millet or rice)
means.substratum <- flextable(substratum.inoculum.2 %>% group_by(substratum) %>% 
                                summarize(mean = mean(growth_14days, na.rm=TRUE), sd = sd(growth_14days, na.rm=TRUE), n = n(),
                                          se = sd / sqrt(n), cv= (se/mean)*100 )%>%
                                arrange(desc(mean)) %>%
                                mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.substratum, path = "means.substratum.docx")

####Dunn test by substratum (sorghum, millet or rice)   
test_dunn <- dunn.test(substratum.inoculum.3$growth_14days , as.factor(substratum.inoculum.3$substratum),method = 'bonferroni')
y <- test_dunn$P.adjusted
z <- test_dunn$Z
x_name <- "comparisons"
y_name <- "P.adjusted"
z_name <- "Z"
spain <- data.frame(x,z,y)
names(spain) <- c(x_name,z_name,y_name)
spain

spain.2 <- spain%>%  mutate(P.adjusted =
                              as.character(signif(P.adjusted, digits =1)))%>%
  mutate(Z =                 as.character(signif(Z, digits =2)))

spain.3 <- flextable::flextable(spain.2)
flextable::save_as_docx(spain.3, path = "nice_table_test_dunn_substratum_media.docx")

#testing pathogen (FLS or SBS)
kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$pathogen), data= substratum.inoculum.3 )
kruskal.pathogen <- tidy(kruskal.test(substratum.inoculum.3$growth_14days  ~ as.factor( substratum.inoculum.3$pathogen), data= substratum.inoculum.3 ))
kruskal.pathogen.2 <- flextable::flextable(kruskal.pathogen %>% mutate(p.value =
                                                                                     as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.pathogen.2, path = "nice_table_kruskal_subst_pathogen.docx")

###means pathogen (FLS or SBS)
means.pathogen <- flextable(substratum.inoculum.2 %>% group_by(pathogen) %>% 
                               summarize(mean = mean(growth_14days, na.rm=TRUE), sd = sd(growth_14days, na.rm=TRUE), n = n(),
                                         se = sd / sqrt(n), cv= (se/mean)*100) %>%
                               arrange(desc(mean)) %>% 
                               mutate_if(is.numeric, round, 2))
flextable::save_as_docx(means.pathogen, path = "means.pathogen.docx")

####Dunn test by pathogen  (FLS or SBS) 
test_dunn.pathogen <- dunn.test(substratum.inoculum.3$growth_14days , as.factor(substratum.inoculum.3$pathogen),method = 'bonferroni')
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


# removing condition susp, reduce/change shortly the name of  each levels and created a new column named "Treatment"  from concatenating: "pathogen, substratum, and condition"
substratum.inoculum.4 <- substratum.inoculum.3  %>% 
  filter(condition=="suspension") %>% 
  mutate(condition = fct_recode(condition, "susp" = "suspension")) %>%
  mutate(substratum = fct_recode(substratum, "mil" = "millet")) %>%
  mutate(substratum = fct_recode(substratum, "sor" = "sorghum")) %>%
  #unite(treatment, pathogen, substratum, condition, sep = "_") 
   mutate(treatment = paste(pathogen, substratum, condition, sep = '_')) %>% 
  mutate(treatment = as.factor(treatment)) 

#by treatment 
kruskal.test(substratum.inoculum.4$growth_14days~ as.factor( substratum.inoculum.4$treatment), data= substratum.inoculum.4)
kruskal <- tidy(kruskal.test(substratum.inoculum.4$growth_14days   ~ as.factor( substratum.inoculum.4$treatment), data= substratum.inoculum.4))
kruskal.2 <- flextable::flextable(kruskal%>% mutate(p.value =
                                                      as.character(signif(p.value, digits =2))) %>%  mutate(p.value = sub("e", "10^", p.value)) %>%  mutate_if(is.numeric, round, 2))

flextable::save_as_docx(kruskal.2, path = "nice_table_kruskal_subtratum_treatment.docx")

#Dunn test by treatment 
test_dunn <- dunn.test(substratum.inoculum.4$growth_14days , as.factor(substratum.inoculum.4$treatment),method = 'bonferroni')
x <- test_dunn$comparisons
y <- test_dunn$P.adjusted
z <- test_dunn$Z
x_name <- "comparisons"
y_name <- "P.adjusted"
z_name <- "Z"
usa <- data.frame(x,z,y)
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

#Graph by treatment 
View(stat.test.subs <- substratum.inoculum.4%>% dunn_test(growth_14days ~ treatment, p.adjust.method = "bonferroni"))
stat.test.subs <- stat.test.subs%>% add_xy_position(x = "treatment", step.increase = 0.5)
View(stat.test.subs)

#Plot for publication
A <- ggboxplot(substratum.inoculum.4,
          x = "treatment",
          y = "growth_14days",
          fill = "treatment") +
  stat_pvalue_manual(stat.test.subs, hide.ns = FALSE,inherit.aes = F) + theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x = element_text(
      face = "bold.italic",
      size = 11,
      family = "Arial",
      angle = 15,
      hjust = 1
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 15,
      family = "Arial"
    ),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) + theme(legend.position = "none") + scale_y_continuous(breaks = c(0, 25, 50,75,100)) + labs(x = "Treatment", y = "Growth at 14 DAI (%)") + labs(tag = "A") +     theme(plot.tag = element_text(
    face = "bold",
    family = "Arial",
    size = 20
  ))
A
# +    labs(y = "growth_28days") # Rename the y-axis

# hola <- ggarrange(A,B, ncol = 1,  align = "v", widths = 1, heights = 1)
# hola
# ggsave("holamean.png", width = 8.24, height = 5.07, units = "in", dpi = 400)
# 
# 
# doc_png <- read_docx()
# doc_png <- body_add_img(doc_png, "holamean.png", width = 6.53, height = 4.017)
# 
# 
