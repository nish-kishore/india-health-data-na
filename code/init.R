library(baser.nish)
library(readxl)

#read in data
#keys
key1 <- read_xlsx("../data/raw/base_data.xlsx", sheet = "CLAY1 Registerlevel KEY")

#level 1 is the "register" or individual form by administrative level
level1 <- read_xlsx("../data/raw/base_data.xlsx", sheet = "CLAY2 RegisterLevel TABLE")
names(level1)[1] <- "id"
level1 %>% as.matrix -> level1
level1[is.na(level1)] <- 0

#cleaning level1
# level1[level1[,"sum"] == 0,]
# 
# key1[which(key1$id %in% level1[level1[,"sum"] == 0,"id"]),]

#level 2 is the question information per register 
level2 <- read_xlsx("../data/raw/base_data.xlsx", sheet = "MASTER")
level2 %>% {.[,c(-1,-2,-3,-5,-7,-8)]} -> level2
names(level2)[1:6] <- c("id", "desc","notes","agg","agg_orig","cat")
level2 %>% as.matrix -> level2
level2[is.na(level2)] <- 0
#level2[,-(1:6)] %>% apply(2,as.numeric) -> level2[,-(1:6)]


#function to identify columns 
edges <- function(x, level_a, level_b){
  if(length(x[x==1]) > 0){
    x %>% {.[.==1]} %>% names %>% as.numeric %>% 
    {cbind(as.numeric(x["id"]),level_a,.,level_b)} %>% as.data.frame() %>%
      set_colnames(c("src_node","src_lyr","dest_node","dest_lyr")) %>%
      return
  }
}

