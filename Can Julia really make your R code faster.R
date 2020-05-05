#------------------------------------------------------------------------------#
# R programming - DDS
#------------------------------------------------------------------------------#
# R Setup ----
# Generating a dataset
df = data.frame(viewer_id = as.integer(as.factor(sample(0:199, 2400, replace = T)))-1,
                movie_id = as.integer(as.factor(round(rnorm(2400, 100, 50)))),
                value = rep(1, 2400))

head(df)


library(JuliaCall)
julia_setup("/home/maxime/julia-1.4/bin/")
julia_library("DataFrames")
julia_assign("df", df)

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

julia_command("
function calculate_vc1(df)
  viewer_coupling = DataFrame(
          viewer_1 = Int[],
          viewer_2 = Int[])
  for i in 1:(nrow(df)-1)
    for j in (i+1):nrow(df)
      if df[i,:movie_id] == df[j,:movie_id] 
        viewers = DataFrame(viewer_1 = df[i,:movie_id],
                             viewer_2 = df[j,:movie_id])
        viewer_coupling = vcat(viewer_coupling, viewers)
      end
    end
  end
  return viewer_coupling
end      
")
julia_eval("calculate_vc1(df)")

r_and_julia_v1 <- rbenchmark::benchmark(
  Rcalculate_vc_1(df),
  julia_eval("calculate_vc1(df)"),
  replications = 5
)


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


julia_command("
function calculate_vc2(df)
  viewer_coupling = DataFrame(viewer_1 = zeros(nrow(df)*20),
                                viewer_2 = zeros(nrow(df)*20))
  counter = 1
  for i in 1:(nrow(df)-1) 
    for j in (i+1):nrow(df) 
      if df[i,:movie_id] == df[j,:movie_id] 
        viewer_coupling[counter,1] = df[i,:movie_id]
        viewer_coupling[counter,2] = df[j,:movie_id]
        counter += 1
      end
    end
  end
  viewer_coupling = viewer_coupling[1:counter,:]
  return viewer_coupling 
end
")

julia_eval("calculate_vc2(df)")


r_and_julia_v2 <- rbenchmark::benchmark(
  Rcalculate_vc_2(df),
  julia_eval("calculate_vc2(df)"),
  replications = 5
)


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

julia_library("Combinatorics")

julia_command("
function calculate_vc3(df)
  df = sort!(df, :viewer_id)
  sdf = groupby(df,:movie_id)
  viewer_coupling = Dict{Tuple{Int64,Int64},Int}()
  for df_i in sdf 
    if nrow(df_i) > 1 
      combs = combinations(df_i[!,:viewer_id], 2)
      for col_i in combs
        current_val = get(viewer_coupling, (col_i[1], col_i[2]), 0)
        viewer_coupling[(col_i[1], col_i[2])] = current_val + 1
      end
    end
  end
  v = [i[2] for i in viewer_coupling]
  viewer_coupling_base = DataFrame(
    viewer1 = [i[1][1] for i in viewer_coupling],
    viewer2 = [i[1][2] for i in viewer_coupling],
    vc = v
  )
  viewer_coupling_base = viewer_coupling_base[
    viewer_coupling_base[!,:viewer1] .!= viewer_coupling_base[!,:viewer2],:]
  return viewer_coupling_base 
end
")
julia_eval("calculate_vc3(df)")

r_and_julia_v3 <-  rbenchmark::benchmark(
  Rcalculate_vc_3(df),
  julia_eval("calculate_vc3(df)"),
  replications = 5
)




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

julia_command("
function calculate_vc4(df)
  df_la = deepcopy(df)
  categorical!(df_la, :movie_id)
  df_la[!,:movie_id] = convert.(Int,df_la[!,:movie_id].refs)
  df_la[!,:movie_id] = df_la[!,:movie_id]
  df_la[!,:viewer_id] = convert.(Int, df_la[!,:viewer_id] .+ 1)
  mla = zeros(Int,
              maximum(df_la[!,:viewer_id]),
              maximum(df_la[!,:movie_id]))
  for i in 1:nrow(df_la) 
    mla[df_la[i,1],df_la[i,2]] += 1
  end
  mla = mla * mla'
  viewer_coupling_la = DataFrame(
    viewer1 = repeat(1:200, inner = 200),
    viewer2 = repeat(1:200, outer = 200),
    vc = mla[:]
  )
  viewer_coupling_la = viewer_coupling_la[
    (viewer_coupling_la[!,:vc] .> 0) .& 
    (viewer_coupling_la[!,:viewer1] .> viewer_coupling_la[!,:viewer2]),:]
  return viewer_coupling_la
end")

r_and_julia_v4 <-  rbenchmark::benchmark(
  Rcalculate_vc_4(df),
  julia_eval("calculate_vc4(df)"),
  replications = 5
)


#____Dplyr ----
library(dplyr)
calculate_vc_dplyr <- function(df){
  viewer_coupling_dp <- inner_join(df,df, by = "movie_id") %>%
    filter(viewer_id.x > viewer_id.y) %>%
    group_by(viewer_id.x, viewer_id.y) %>%
    summarise(vc = n())
  return(viewer_coupling_dp)
}

system.time(calculate_vc_dplyr(df))

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
system.time(calculate_vc_dt(df))



julia_command("
function calculate_vc_jdf(df)
  jdf = join(df, df, on = :movie_id, makeunique = true)
  filter!(row -> row[:viewer_id] > row[:viewer_id_1], jdf)
  viewer_coupling_jdf = by(jdf, [:viewer_id, :viewer_id_1],
                           vc = :value => sum)
  return viewer_coupling_jdf
end")

julia_eval("calculate_vc_jdf(df)")


r_and_julia_vdf <-  rbenchmark::benchmark(
  Rcalculate_vc_dplyr(df),
  Rcalculate_vc_dt(df),
  julia_eval("calculate_vc_jdf(df)"),
  replications = 5
)


library(dplyr)

comparaisons <- bind_rows(list(r_and_julia_v1,
                               r_and_julia_v2,
                               r_and_julia_v3,
                               r_and_julia_v4,
                               r_and_julia_vdf))

comparaisons %>%
  mutate(relative = elapsed/min(elapsed)) %>% View()
