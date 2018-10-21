library(tidyverse)
library(magrittr)
library(igraph)

#load data
source("./init.R")

#most duplicated form
level1[,-1] %>% rowSums(na.rm=T) %>% {cbind(level1, "sum" = .)} -> level1
level1 %>% merge(key1, by="id", all.x=T) %>%
  dplyr::select(label, sum) %>%
  mutate(perc = round(sum/9*100,2)) %>%
  {.[order(-.$perc),]}

#most duplicated questions
level2[,-(1:6)] %>% apply(2,as.numeric) %>% rowSums(na.rm=T) %>%
{cbind(level2,"sum"=.)} -> level2
level2 %>% as.data.frame %>% mutate(perc = round(as.numeric(sum)/172*100,2)) %>%
  dplyr::select(id,desc,cat, perc) %>%
{.[order(-.$perc),]} -> a

#modifying for multilevel 

edge_list <- rbind(
  (apply(level1, 1, function(x) edges(x, 2,3)) %>% bind_rows),
  (apply(level2[,-c(2:6)], 1, function(x) edges(x,1,2)) %>% bind_rows))

write.csv(edge_list, "../data/edge_list.csv")
c(edge_list$src_node, edge_list$dest_node) %>% unique %>% t %>%
  write.csv("C:/Users/nishk/gitrepos/hsph/india-healthd-data-na/data/proc/layer_info.csv")

#how many aggregate data points are there 
a[grepl("aggregate",tolower(a$cat)),] -> b
a[!(grepl("aggregate",tolower(a$cat))),] -> c

#deep dive into phc
a %>%
  merge(edge_list[c("src_node","dest_node")], by.x = "id", by.y="src_node")%>%
  merge(edge_list[c("src_node","dest_node")], by.x = "dest_node", by.y="src_node") %>%
  set_names(c("reg","id","desc","cat","perc","level")) -> temp

temp <- temp[!(is.na(temp$level)),]
