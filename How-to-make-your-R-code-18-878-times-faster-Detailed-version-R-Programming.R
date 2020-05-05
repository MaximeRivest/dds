#------------------------------------------------------------------------------#
# R programming - DDS
#------------------------------------------------------------------------------#

#---- References ----
"https://www.burns-stat.com/pages/Tutor/R_inferno.pdf"


# R Setup ----
library(tictoc)
head(df)


# Generating a dataset
df = data.frame(viewer_id = as.integer(as.factor(sample(0:199, 4800, replace = T)))-1,
                movie_id = as.integer(as.factor(round(rnorm(4800, 100, 50)))),
                value = rep(1, 4800))

#____Growing a vector, no algorithmic optimization ----

Rcalculate_vc_1 <- function(df){
  viewer_coupling <- data.frame(viewer_1 = numeric(0), viewer_2 = numeric(0))
  for(i in 1:(nrow(df)-1)){
    for(j in (i+1):nrow(df)){
      if(df[i,"movie_id"] == df[j,"movie_id"]){
        viewers <- data.frame(viewer_1 = df[i,"viewer_id"],
                              viewer_2 = df[j,"viewer_id"])
        viewer_coupling <- rbind(viewer_coupling, viewers)
      }
    }
  }
  return(viewer_coupling) 
}

tic()
df_vc_1 <- Rcalculate_vc_1 ()
toc()
# 95.577 sec elapsed

#____Pre-allocate the data.frame, no algorithmic optimization ----
Rcalculate_vc_2 <- function(df){
  viewer_coupling <- data.frame(viewer_1 = numeric(nrow(df) * 20),
                                viewer_2 = numeric(nrow(df) * 20))
  counter <- 1
  for(i in 1:(nrow(df)-1)){
    for(j in (i+1):nrow(df)){
      if(df[i,"movie_id"] == df[j,"movie_id"]){
        viewer_coupling[counter, 1] <- df[i,"viewer_id"]
        viewer_coupling[counter, 1] <- df[j,"viewer_id"]
        counter <- counter + 1
      }
    }
  }
  viewer_coupling <- viewer_coupling[1:(counter-1),]
  return(viewer_coupling) 
}
tic()
df_vc_2 <- Rcalculate_vc_2 ()
toc()
# 86.893 sec elasped


#____sort df and list ----

Rcalculate_vc_3 <- function(df){
  df <- df[order(df$viewer_id),]
  ldf <- split(df, as.factor(df$movie_id))
  viewer_coupling <- list()
  for(subdf in ldf) {
    if(nrow(subdf) > 1) {
      combn_vec <- combn(subdf$viewer_id, 2)
      for(col_i in 1:ncol(combn_vec)){
        vc_name <- paste(as.vector(combn_vec[,col_i]), collapse = "_")
        if(is.null(viewer_coupling[[vc_name]])) {
          viewer_coupling[vc_name] <- 1
        } else {
          viewer_coupling[[vc_name]] <- viewer_coupling[[vc_name]] + 1
        }
      }
    }
  }
  vnames <- unlist(strsplit(names(viewer_coupling), "_"))
  
  df_viewer_coupling <- data.frame(
    "viewer_1" = as.integer(vnames[seq(1, length(vnames), 2)]),
    "viewer_2" = as.integer(vnames[seq(2, length(vnames), 2)]),
    vc = unlist(viewer_coupling)  
  )
  df_viewer_coupling <- df_viewer_coupling[
    df_viewer_coupling$viewer_1 != df_viewer_coupling$viewer_2,]
  return(df_viewer_coupling)
}
tic()
df3 <- Rcalculate_vc_3 ()
toc()

# 3.097 sec elapsed

#____linear algebra ----

Rcalculate_vc_4 <- function(df){
  v <- as.integer(as.factor(df$viewer_id))
  m <- as.integer(as.factor(df$movie_id))
  
  mat <- matrix(0, nrow = max(v), ncol=max(m))
  
  for(i in 1:length(v)){
    mat[v[i], m[i]] <- 1
  }
  
  vc_mat <- mat %*% t(mat)
  
  viewer_coupling_df <- data.frame(
    viewer_1 = rep(1:200,200),
    viewer_2 = rep(1:200, each = 200),
    vc = as.vector(vc_mat)
  )
  
  viewer_coupling_df <- viewer_coupling_df[(viewer_coupling_df$viewer_1 > viewer_coupling_df$viewer_2) &
                                             viewer_coupling_df$vc > 0,]
  return(viewer_coupling_df)
}

#____dplyr ----

library(dplyr)
Rcalculate_vc_dplyr <- function(df){
  viewer_coupling_dp <- inner_join(df, df, by = "movie_id") %>%
    filter(viewer_id.x > viewer_id.y) %>%
    group_by(viewer_id.x, viewer_id.y) %>%
    summarise(vc = sum(value.x))
  return(viewer_coupling_dp)
}

#____Data.table ----

library(data.table)
Rcalculate_vc_dt <- function(df){
  setDTthreads(1)
  dt1 <- as.data.table(df)
  dt2 <- as.data.table(df)
  setkey(dt1, "movie_id")
  setkey(dt2, "movie_id")
  
  viewer_coupling_dt <- merge(dt1, dt2, all = T, allow.cartesian = T)
  
  viewer_coupling_dt <- viewer_coupling_dt[
    viewer_id.x > viewer_id.y,
    sum(value.x),
    by = .(viewer_id.x,viewer_id.y)
    ]
  return(viewer_coupling_dt)
}
