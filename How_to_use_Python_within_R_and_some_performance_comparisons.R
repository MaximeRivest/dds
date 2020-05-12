#------------------------------------------------------------------------------#
# R programming - DDS
#------------------------------------------------------------------------------#
# R Setup ----
library(tictoc)
# Generating a dataset
df = data.frame(viewer_id = as.integer(as.factor(sample(0:199, 2400, replace = T)))-1,
                movie_id = as.integer(as.factor(round(rnorm(2400, 100, 50)))),
                value = rep(1, 2400))

# Python setup ---
library(reticulate)
use_python("/usr/bin/python3")
r_to_py(df)

#____Growing a vector, no algorithmic optimization ----
Rcalculate_vc_1 <- function(df){
  viewer_coupling <- data.frame(viewer_1 = numeric(0), viewer_2 = numeric(0))
  for(i in 1:(nrow(df)-1)) {
    for(j in (i+1):nrow(df)){
      if(df[i,"movie_id"] == df[j,"movie_id"]) {
        viewers = data.frame(viewer_1 = df[i,"movie_id"],
                             viewer_2 = df[j,"movie_id"])
        viewer_coupling <- rbind(viewer_coupling, viewers)
      }
    }
  }
  return(viewer_coupling)
}

py_run_string("
import pandas as pd
def Py_calculate_vc_1(df):
  viewer_coupling = pd.DataFrame(columns = ['viewer_1', 'viewer_2'])
  for i in range(0,df.shape[0]-1):
    for j in range(i+1,df.shape[0]):
      if df.iloc[i,1] == df.iloc[j,1]:
        viewers = pd.DataFrame({'viewer_1': [df.iloc[i,1]],'viewer_2': [df.iloc[j,1]]})
        viewer_coupling = viewer_coupling.append(viewers)
  return viewer_coupling
")

tic()
dfrvc1 <- Rcalculate_vc_1(df)
toc()
#73.716

tic()
dfpy <- py$Py_calculate_vc_1(df)
toc()
#73.782

#____Pre-allocate the data.frame, no algorithmic optimization ----
Rcalculate_vc_2 <- function(df){
  viewer_coupling <- data.frame(viewer_1 = numeric(nrow(df)*20),
                                viewer_2 = numeric(nrow(df)*20))
  counter <- 1
  for(i in 1:(nrow(df)-1)) {
    for(j in (i+1):nrow(df)){
      if(df[i,"movie_id"] == df[j,"movie_id"]) {
        viewer_coupling[counter,1] <- df[i,"movie_id"]
        viewer_coupling[counter,2] <- df[j,"movie_id"]
        counter <- counter + 1
      }
    }
  }
  viewer_coupling <- viewer_coupling[1:counter,]
  return(viewer_coupling)
}

py_run_string("
def Py_calculate_vc_2(df):
  viewer_coupling = pd.DataFrame({'viewer_1': [0] * 20 * df.shape[0],
    'viewer_2': [0] * 20 * df.shape[0]})
  counter = 0
  for i in range(0,df.shape[0]-1):
    for j in range(i+1,df.shape[0]):
      if df.iloc[i,1] == df.iloc[j,1]:
        viewer_coupling.iloc[counter, 0] = df.iloc[i,1]
        viewer_coupling.iloc[counter, 1] = df.iloc[j,1]
        counter += 1
  return viewer_coupling.iloc[range(0,counter), [0,1]]
")

bench12 <- rbenchmark::benchmark(
  Rcalculate_vc_1(df),
  Rcalculate_vc_2(df),
  py$Py_calculate_vc_1(df),
  py$Py_calculate_vc_2(df),
  replications = 3
)
bench12_sec = dplyr::mutate(bench12, elapsed_sec = elapsed / 3)


#____sort df and list ----
Rcalculate_vc_3 <- function(df){
  df <- df[order(df$viewer_id),]
  sdf = split(df,as.factor(df$movie_id))
  viewer_coupling <- list()
  for(df_i in sdf) {
    if(length(df_i$viewer_id) > 1){
      combs <- combn(df_i$viewer_id, 2)
      for(col_i in 1:ncol(combs)){
        vc_name <- paste(as.vector(combs[,col_i]), collapse = "_")
        if(is.null(viewer_coupling[[vc_name]])){
          viewer_coupling[vc_name] <- 1
        } else {
          viewer_coupling[[vc_name]] <- viewer_coupling[[vc_name]] + 1
        }
      } 
    }
  }
  v <- unlist(strsplit(names(viewer_coupling), "_"))
  viewer_coupling_base <- data.frame(
    viewer1 = as.integer(v[seq(1, length(v), by = 2)]),
    viewer2 = as.integer(v[seq(2, length(v), by = 2)]),
    vc = unlist(viewer_coupling)
  )
  viewer_coupling_base <- viewer_coupling_base[
    viewer_coupling_base$viewer1 != viewer_coupling_base$viewer2,]
  return(viewer_coupling_base)
}

py_run_string("
import pandas as pd
from itertools import combinations 

def Py_calculate_vc_3(df):
  df = df.sort_values(['viewer_id'])
  sdf = df.groupby('movie_id')
  viewer_coupling = {}
  for df_i in sdf:
    if df_i[1].shape[0] > 1:
      combs = combinations(df_i[1]['viewer_id'],2)
      for comb_i in combs:
        current_val = viewer_coupling.get((comb_i[0], comb_i[1]), 0)
        viewer_coupling[(comb_i[0], comb_i[1])] = current_val + 1
  viewer_coupling_base = pd.DataFrame(
        {'viewer1': [i[0] for i in viewer_coupling],
        'viewer2': [i[1] for i in viewer_coupling],
        'vc': viewer_coupling.values})
  viewer_coupling_base = viewer_coupling_base[viewer_coupling_base['viewer1'] != viewer_coupling_base['viewer2']]
  return viewer_coupling_base
")

bench3 <- rbenchmark::benchmark(
  Rcalculate_vc_3(df),
  py$Py_calculate_vc_3(df),
  replications = 10
)
bench3_sec = dplyr::mutate(bench3, elapsed_sec = elapsed / 10)



#____linear algebra ----
Rcalculate_vc_4 <- function(df){
  df_la <- df
  df_la$movie_id <- as.integer(as.factor(df$movie_id))
  df_la$movie_id <- df_la$movie_id + 1
  df_la$viewer_id <- df_la$viewer_id + 1
  mla <- matrix(0, 
                nrow = max(df_la$viewer_id),
                ncol = max(df_la$movie_id))
  for(i in 1:nrow(df_la)){
    mla[df_la[i,1],df_la[i,2]] <- mla[df_la[i,1],df_la[i,2]] + 1
  }
  mla <- mla %*% t(mla)
  viewer_coupling_la <- data.frame(
    viewer1 = rep(1:200, each = 200),
    viewer2 = rep(1:200, times = 200),
    vc = as.vector(mla)
  )
  viewer_coupling_la <- viewer_coupling_la[
    (viewer_coupling_la$vc > 0) & 
      (viewer_coupling_la$viewer1 > viewer_coupling_la$viewer2),]
  return(viewer_coupling_la)
}

py_run_string("
def Py_calculate_vc_4(df):
  df_la = df
  df_la['movie_id'] = df['movie_id'].astype('category').cat.codes
  df_la['viewer_id'] = df['viewer_id'].astype('category').cat.codes
  mla = np.zeros((max(df_la['viewer_id'])+1,max(df_la['movie_id'])+1))
  for i in range(0, df_la.shape[0]) :
    mla[df_la.iloc[i,0],
        df_la.iloc[i,1]] = mla[df_la.iloc[i,0],
                               df_la.iloc[i,1]] + 1
  mla = np.matmul(mla,np.transpose(mla))
  viewer_coupling_la = pd.DataFrame(
    {'viewer1': [i for i in range(0,200)] * 200,
    'viewer2': np.repeat(np.arange(0,200),200),
    'vc': np.reshape(mla,-1)}
  )
  viewer_coupling_la = viewer_coupling_la[
    (viewer_coupling_la['vc'] > 0) & 
      (viewer_coupling_la['viewer1'] > viewer_coupling_la['viewer2'])]
  return viewer_coupling_la
              ")

bench4 <- rbenchmark::benchmark(
  Rcalculate_vc_4(df),
  py$Py_calculate_vc_4(df),
  replications = 100
)

bench4_sec <- dplyr::mutate(bench4, elapsed_sec = elapsed /100)
bench4_sec

#____Dplyr ----
library(dplyr)
calculate_vc_dplyr <- function(df){
  viewer_coupling_dp <- inner_join(df,df, by = "movie_id") %>%
    filter(viewer_id.x > viewer_id.y) %>%
    group_by(viewer_id.x, viewer_id.y) %>%
    summarise(vc = n())
  return(viewer_coupling_dp)
}

#____Data.table ----
library(data.table)
setDTthreads(1)
calculate_vc_dt <- function(df){
  df1 <- as.data.table(df)
  df2 <- as.data.table(df)
  setkey(df1, "movie_id")
  setkey(df2, "movie_id")
  
  viewer_coupling_dt <- merge(df1, df2, all=FALSE, allow.cartesian = T)
  viewer_coupling_dt <- viewer_coupling_dt[
    viewer_coupling_dt$viewer_id.x > viewer_coupling_dt$viewer_id.y,
    .N,
    by = .(viewer_id.x, viewer_id.y)]
  return(viewer_coupling_dt)
}


#____join Pandas Python ====
py_run_string("
import pandas as pd
def Py_calculate_vc_pandas(df):
  df.set_index('movie_id')
  jdf = pd.merge(left=df, 
                 right=df,
                 left_on='movie_id',
                 right_on='movie_id')
  jdf = jdf[jdf['viewer_id_x']>jdf['viewer_id_y']]
  jdf = jdf.groupby(['viewer_id_x','viewer_id_y'])[['value_x']].count()
  return jdf
")

bench_dfs <- rbenchmark::benchmark(
  calculate_vc_dplyr(df),
  calculate_vc_dt(df),
  py$Py_calculate_vc_pandas(df),
  replications = 100
)

benchdfs_sec <- dplyr::mutate(bench_dfs, elapsed_sec = elapsed /100)
benchdfs_sec
