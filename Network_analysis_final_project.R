### Network Analysis Final Project 

  # Clara Vetter (12562009)

  # Deadline: 17.12.2020 

  # TOPIC: Comparing the network structure of COVID-19 related stress in 
    # extroverts vs. introverts 

# Differences between extroverts and introverts in 
# network structure of covid-19-related stress

library("mgm")
library("dplyr")
library("qgraph")
library("ggpubr")
library("rstatix")
library("bootnet")
library("NetworkComparisonTest")


# small world structure: is network clustered, short path length
# centrality: which nodes are important
# connectivity
# clustering
# closeness & betweenness


df <- read.csv("/Users/claravetter/Documents/Master/Psych Methods/Network Analysis/Project/NA_2020_data.csv",header=T)
head(df)

# select my variables 
my_df <- df %>%
  select(introvert.extrovert,Q56,Q57,Q58,Q59,Q60,Q61,Q62) %>% 
  filter(!is.na(introvert.extrovert))

my_df %>%
  group_by(introvert.extrovert) %>% 
  count()
# introverts: 301, extroverts: 197

head(my_df)

# 56. I am worried about catching Covid-19.
# 57. I am worried that Covid-19 will negatively impact my financial situation.
# 58. I am worried about coming into contact with foreigners because they might have Covid-19. 
# 59. I am worried that people around me will infect me with Covid-19.
# 60. I have trouble concentrating because I keep thinking about Covid-19.
# 61. I regularly search the internet for information on Covid-19.
# 62. The government's guidelines to prevent the spread of Covid-19 are appropriate.


items <- c("I am worried about catching Covid-19", 
           "I am worried that Covid-19 will negatively impact my financial situation",
           "I am worried about coming into contact with foreignersd because they might have Covid-19",
           "I am worried that people around me will infect me with Covid-19",
           "I have trouble concentrating because I keep thinking about Covid-19",
           "I regularly search the internet for information on Covid-19",
           "The government's guidelines to prevent the spread of Covid-19 are appropriate")

items_short <- c("D",
                     "SEC",
                     "X",
                     "C",
                     "T",
                     "CC",
                     "R")



# test for normality 
# visually
hist(my_df$Q56)
ggdensity(my_df$Q56)

hist(my_df$Q57)
ggdensity(my_df$Q57)

hist(my_df$Q58)
ggdensity(my_df$Q58)

hist(my_df$Q59)
ggdensity(my_df$Q59)

hist(my_df$Q60)
ggdensity(my_df$Q60)

hist(my_df$Q61)
ggdensity(my_df$Q61)

hist(my_df$Q62)
ggdensity(my_df$Q62)

# shapiro-wilk test
my_df %>% shapiro_test(Q56,Q57,Q58,Q58,Q59,Q60,Q61,Q62)
# all not normally distributed 
  # therefore, I am going to use Spearman correlation 

# missing values 
sum(is.na(my_df$Q56)) # 4
sum(is.na(my_df$Q57)) # 5
sum(is.na(my_df$Q58)) # 6
sum(is.na(my_df$Q59)) # 4
sum(is.na(my_df$Q60)) # 5
sum(is.na(my_df$Q61)) # 6
sum(is.na(my_df$Q62)) # 4

# as there are not many NAs, I decided to remove them from the dataset
# my_df <- my_df %>%
#  filter(!is.na(Q56)&!is.na(Q57)&!is.na(Q58)&!is.na(Q59)&!is.na(Q60)&!is.na(Q61)&!is.na(Q62)) %>% 
#   mutate(Q56 = Q56)
# not necessary with EBICglasso --> pairwise

# rename variables
names(my_df)[2:8] <- c(items_short)

# transform extraversion variable to numeric
my_df$introvert.extrovert <- as.numeric(as.factor(my_df$introvert.extrovert))

head(my_df$introvert.extrovert)

### NETWORK WHOLE SAMPLE

# estimate network
total_network <- my_df %>%
  #select(-introvert.extrovert) %>%
  estimateNetwork(default = c("mgm"), corMethod = "spearman",
                  verbose = FALSE)



?qgraph
# stability and centrality measures 
pdf("/Users/claravetter/Documents/Master/Psych Methods/Network Analysis/Project/total_network.pdf")
# plot the estimated network 
plot <- qgraph(total_network$graph, 
               layout = "spring", 
               theme = "colorblind",
               groups = c(1,0,0,0,0,0,0,0,0),
               labels = c("E",items_short),
               vsize = 15)

centralityPlot(total_network, include = c("Strength","Closeness","Betweenness"),
               labels = c("E",items_short),
               verbose = FALSE,
               decreasing = TRUE)

set.seed(12562009)
boot_nonparametric <- bootnet(total_network, 
                              nBoots = 1000, 
                              nCores = 1)

plot(boot_nonparametric,
     order = "sample", 
     labels = FALSE)

plot(boot_nonparametric,
     plot = "difference", 
     onlyNonZero = TRUE, 
     order = "sample")

set.seed(12562009)
boot_casedrop <- bootnet(total_network,
                         nBoots = 1000,
                         type = "case",
                         statistics = c("strength", "betweenness", "closeness"))

plot(boot_casedrop, statistics = c("strength","betweenness","closeness"))
corStability(boot_casedrop)

dev.off()




### EXTROVERTS VS. INTROVERTS
# extroverts df 
extroverts_df <- my_df %>%
  filter(introvert.extrovert == "Extrovert") 

# introverts df
introverts_df <- my_df %>%
  filter(introvert.extrovert == "Introvert") 

# estimate networks for extroverts and introverts  
extroverts_network <- extroverts_df %>%
  select(-introvert.extrovert) %>%
  estimateNetwork(default = c("EBICglasso"),
                  corMethod = "spearman") 

introverts_network <- introverts_df %>%
  select(-introvert.extrovert) %>%
  estimateNetwork(default = c("EBICglasso"),
                  corMethod = "spearman") 

# layout
L <- averageLayout(extroverts_network,introverts_network)

# plot networks

pdf("/Users/claravetter/Documents/Master/Psych Methods/Network Analysis/Project/extroverts_introverts_networks.pdf")
layout(1)
extroverts_plot <- qgraph(extroverts_network$graph, 
                          layout = L, 
                          theme = "colorblind", 
                          labels = items_short,
                          label.cex = 1,
                          label.scale = F)


layout(1)
introverts_plot <- qgraph(introverts_network$graph, 
                          layout = L, 
                          theme = "colorblind",
                          labels = items_short,
                          #label.prop = 0.1,
                          label.cex = 1,
                          label.scale = F)


centralityPlot(extroverts_network,include = c("Strength","Closeness","Betweenness"),
               labels = items_short,
               verbose = FALSE,
               decreasing = TRUE)

centralityPlot(introverts_network,include = c("Strength","Closeness","Betweenness"),
               labels = items_short,
               verbose = FALSE,
               decreasing = TRUE)

# correlation of edge weights
weights_extroverts <- extroverts_network$graph
edge_weights_extroverts <- weights_extroverts[lower.tri(weights_extroverts,diag=F)]

weights_introverts <- introverts_network$graph
edge_weights_introverts <- weights_introverts[lower.tri(weights_introverts,diag=F)]

cor(edge_weights_extroverts,edge_weights_introverts) #0.87

# plot
layout(1)
plot(edge_weights_extroverts,
     edge_weights_introverts,
     xlab = "extroverts",
     ylab = "introverts")
abline(lm(edge_weights_introverts~edge_weights_extroverts))
title(paste0("R = ", round(cor(edge_weights_extroverts,edge_weights_introverts),2)))

dev.off()

# nct test comparsion 
set.seed(12562009)
nct <- NCT(extroverts_network, 
                introverts_network, 
                test.edges = TRUE, 
                edges = "all", 
                test.centrality = TRUE, 
                centrality = c("strength", "betweenness", "closeness"),
                it = 1000)

# global strength invariance p-value
nct$glstrinv.pval # not significant 0.054 --> omnibus test not significant; groups do not differ significantly

# network structure invariance p-value
nct$nwinv.pval # not significant 0.079 

# edge invariance p-value
nct$einv.pvals # significant: Traumatic - Compulsive Checking (p = 0.014)




