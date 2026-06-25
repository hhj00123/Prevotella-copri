install.packages("pacman")

pacman::p_load(tidyverse,reshape2,psych)
taxonomy =read.csv('TAXON.CSV',row.names = 1)
env=read.csv("sample.CSV",row.names = 1)

env <- env[rownames(taxonomy), ]

r_result <- corr.test(taxonomy, env, method = 'spearman')

r <- 
  r_result$r %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'r'))

p <- 
  r_result$p %>% 
  melt() %>% 
  set_names(c('tax', 'Index', 'P_value')) %>% 
  mutate(P_value_sig = case_when(P_value > 0.05 ~ " ",
                             P_value <= 0.05 & P_value > 0.01 ~ "*",
                             P_value <= 0.01 & P_value > 0.001 ~ "**",
                             P_value <= 0.001 ~ "***",
                             TRUE ~ NA_character_))

data <- cbind(r,p) %>% select(-(4:5))

theme <- theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        text = element_text(face='bold'),
        legend.key.size = unit(14, "pt"),
        panel.grid.minor = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x=element_text(angle=45,vjust=1, hjust=1))

p=ggplot(data,aes(x=tax, y=Index))+
  geom_tile(aes(fill=r), color = 'white',alpha = 0.8) +
  geom_text(aes(label = P_value_sig), color = 'black', size = 4) +
  ggsci::scale_fill_gsea() +
  theme(legend.position="top") +
  theme +
  xlab('') +
  ylab('')