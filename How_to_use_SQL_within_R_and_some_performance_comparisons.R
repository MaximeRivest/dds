#------------------------------------------------------------------------------#
# R programming - DDS
#------------------------------------------------------------------------------#
# R Setup ----

# Generating a dataset
df = data.frame(viewer_id = as.integer(as.factor(sample(0:199, 2400, replace = T)))-1,
                movie_id = as.integer(as.factor(round(rnorm(2400, 100, 50)))),
                value = rep(1, 2400))

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

#____SQL ----
library(RSQLite)
library(DBI)

con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(con, "df1", df)
dbWriteTable(con, "df2", df)
dbExecute(con, "create index idx_movie_id ON df1 (movie_id)")
dbExecute(con, "create index idx_movie_id2 ON df2 (movie_id)")

calculate_vc_sql <- function(df){
  res <- dbSendQuery(con,
    "
    SELECT a.viewer_id as v1, b.viewer_id as v2, count() as vc
    FROM df1 as a,
      df2 as b
    WHERE a.movie_id = b.movie_id AND a.viewer_id > b.viewer_id
    GROUP BY v1, v2
    "
  )
  df_sql <- dbFetch(res)
  dbClearResult(res)
  return(df_sql)
}

rbenchmark::benchmark(
  calculate_vc_dt(df),
  calculate_vc_dplyr(df),
  calculate_vc_sql(df),
  replications = 100
)
