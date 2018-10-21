#playground

library(magrittr)
library(igraph)
library(HiveR)

#creating communities in edge list 

edge_list <- read.csv("../data/proc/edge_list.csv", header=F)

names(edge_list) <- c("src_id", "src_lyr", "dest_id", "dest_lyr")

#iterate over a list of unique form id's and spits out an 
#edge list with the proprer src_id cluster
ind_lyr_cluster <- function(id, edge_list, src_lyr){
  x <- edge_list[which(edge_list$dest_id == id),"src_id"]
  if(length(x) > 1){
    mat <- matrix(1, length(x), length(x))
    rownames(mat) <- colnames(mat) <- x
    mat[upper.tri(mat)] <- 0
    mat[diag(mat)] <- 0
    graph.adjacency(mat) %>% get.edgelist() %>% as.data.frame -> temp
    
    cbind("src_id" = as.numeric(as.character(temp$V1)),
          "src_lyr" = src_lyr, 
          "dest_id" = as.numeric(as.character(temp$V2)), 
          "dest_lyr" = src_lyr) %>% return
  }
}

lyr_cluster <- function(edge_list, src_lyr_num, dest_lyr_num){
  edge_list %>%
    subset(src_lyr == src_lyr_num & dest_lyr==dest_lyr_num) %>% 
    {unique(.$dest_id)} -> unique_forms
  
  lapply(unique_forms, function(x) ind_lyr_cluster(x,edge_list, src_lyr_num)) %>% 
         {do.call(rbind,.)}
  }

lyr_cluster(edge_list, 1, 2) %>% as.data.frame -> a
lyr_cluster(edge_list ,2, 3) %>% as.data.frame -> b
edge_list %>%
  set_names(c("dest_id", "dest_lyr", "src_id", "src_lyr")) %>%
  lyr_cluster(3,2) %>% as.data.frame -> c
edge_list %>%
  set_names(c("dest_id", "dest_lyr", "src_id", "src_lyr")) %>%
  lyr_cluster(2,1) %>% as.data.frame -> d

#expanded edge list 
edge_list2 <- rbind(edge_list, a, b, c, d)

write.table(edge_list2, sep=",", file = "../data/proc/edge_list2.csv", 
            col.names = F, row.names = F)

#test space
edge_list %>%
  subset(src_lyr==1) %>%
  merge(edge_list[which(edge_list$src_lyr==2),], by.x="dest_id", by.y="src_id") %>%
  set_names(c("reg","quest","a","b","c","level","d")) %>%
  dplyr::select(quest,reg,level) -> a

#creating bipartite graphs 

test1 <- level1[level1[,"sum"] != 0,2:10] 
dimnames(test1) <- list(level1[level1[,"sum"] != 0,1], dimnames(level1)[[2]][2:10])
bg1 <- graph.incidence(test1)

bipartite_projection(bg1) %>%
{plot(.$proj1)}

plot(bg1, layout=layout.bipartite, asp=0.65, 
     vertex.color=c("orange","green")[V(bg1)$type+1])

  #testing with layout of coordinates 
coords <- layout_as_bipartite(bg1)
coords[128:136,1] <- seq(1,140, by=140/9)


V(bg1)$name %>% 
  as.data.frame %>%
  merge(key1, by.x=".",by.y="id") %>%
  {.$label} -> V(bg1)$name

plot(bg1,
     layout=coords,
     edge.arrow.size=1,
     vertex.label.family="Helvetica",
     vertex.label.color="black",
     vertex.label.cex=1,
     vertex.label.dist=rep(4, 136),
     vertex.label.degree=c(rep(-pi/2, 127), rep(pi/2, 9)), #0 is right, "pi" is left, "pi/2" is below, and "-pi/2" is above
     edge.arrow.width=1,
     edge.lty=1,
     asp=0.6) # controls how rectangular the plot is. <1 = wide, >1 = tall

#creating a tripartite network 

edge_list[complete.cases(edge_list),] %>%
  dplyr::select(src_node, dest_node) %>%
  graph_from_data_frame(directed=FALSE) -> tg

layer <-  rep(1, length(V(tg)$name))
layer[1:127] <- 2
layer[1208:1215] <- 3

V(tg)$name[1208:1215] <- paste("Level ", c(2:8,1))

layout <- layout_with_sugiyama(tg, layers=layer)

layout$layout[1208:1215,1] <- seq(1, 1100, by = 1100 / 8)
layout$layout[1:127,1]

cbind(V(tg)$name[1:127], layout$layout[1:127,1]) %>%
  as.data.frame %>%
  {.[order(.[,2]),]}

sort(table(layout$layout[1:127,1]))

plot(tg,
     layout=cbind(layer,layout$layout[,1]),
     vertex.shape=c("square","circle","square")[layer],
     vertex.size=c(0,20,25)[layer],
     vertex.label.dist=c(0.8,0,0)[layer],
     vertex.label.degree=0)

##Analysis without aggregate information 
level2[level2[,"agg"] != " 1",] -> level2_noagg
level2_noagg[,c(1,7:177)] %>%
  graph.incidence

graph.incidence(level1)
  

a <- level1[level1[,"sum"] != 0,2:10] 
dimnames(a) <- list(level1[level1[,"sum"] != 0,1], dimnames(level1)[[2]][2:10])
a <- graph.incidence(a)

b <- level2[level2[,"agg"] != " 1",]
dimnames(b) <- list(b[,"id"], dimnames(level1)[[2]][2:10])
bg1 <- graph.incidence(test1)


###CREATING A PRETTY GRAPH 

#create a form for each level if shared by multiple levels 
new_level1 <- level1[,-11]
new_level2 <- b[,c(1,7:177)]

forms <- vector()

for(i in 1:nrow(new_level1)){
  forms %<>% c(new_level1[i,new_level1[i,]==1] %>% 
    names %>% 
    substr(5,5) %>%
    {paste0(new_level1[i,"id"],"-",.)})
}

  #clear out forms that aren't in any level
forms <- forms[!(endsWith(forms, "-"))]

#find questions that are in forms 
f_to_q <- new_level2[,1,drop=F]

for(i in 1:length(forms)){
  f_to_q %<>% cbind(as.matrix(new_level2[,substr(forms[i],1,5)]) %>%
                      set_colnames(forms[i]))
}

#clean up "TRUE" and other ridiculous things
f_to_q[f_to_q == "TRUE"] <- 1
f_to_q[5890] <- 0
f_to_q[358868] <- 1
#convert to numeric and add in sums
f_to_q <- apply(f_to_q, 2, as.numeric)
f_to_q <- cbind(f_to_q, "sum" = rowSums(f_to_q[,-1]))


#convert questions to weights between forms 
final <- matrix(nrow=length(forms), ncol=length(forms))
colnames(final) <- forms
rownames(final) <- forms
final[is.na(final)] <- 0

f_mult_q <- f_to_q[as.numeric(f_to_q[,"sum"]) > 1,]
f_mult_q <- f_mult_q[,-ncol(f_mult_q)]

for(i in 1:nrow(f_mult_q)){
  f_mult_q[i,as.numeric(f_mult_q[i,]) == 1] %>%
    names() %>% combn(2) -> tmp 
  
  for(j in 1:ncol(tmp)){
    final[tmp[1,j],tmp[2,j]] <- final[tmp[1,j],tmp[2,j]] + 1
  }
}


#plot forms by 8 levels

final <- readRDS("../data/proc/final.RDS")
saveRDS(final, "../data/proc/final.RDS")


final_graph <- graph.adjacency(final, weighted = T)

final_layer <-  as.numeric(substr(V(final_graph)$name, 7,7))

substr(V(final_graph)$name,1,5) %>%
  as.data.frame() %>%
  merge(key1, by.x=".",by.y="id") %>%
  {.$label} -> V(final_graph)$name

final_layout <- layout_with_sugiyama(final_graph, layers=final_layer)

#layout$layout[1208:1215,1] <- seq(1, 1100, by = 1100 / 8)
#layout$layout[1:127,1]

plot(final_graph,
     layout=cbind(final_layer,final_layout$layout[,1]),
     vertex.shape=c("square","circle","square","circle",
                    "square","circle","square","circle")[final_layer],
     vertex.label.degree=0)


###HIVE
hive1 <- edge2HPD(edge_df = get.data.frame(final_graph))
hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")
hive3 <- mineHPD(hive2, option = "axis <- source.man.sink")
hive4 <- mineHPD(hive3, option = "remove zero edge")
