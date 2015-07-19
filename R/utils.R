.onLoad <- function(libname, pkgname) {
  envir <<- new.env()
}

.onUnload <- function(libpath) {
  # Force finalize() on objects
  gc();
} # .onUnload()


#' Internals
#'
#' @export
#' @importFrom stats na.omit

.District <-
  function(name, electorate, each_row, link, max_vote_length){
    candidates <- NULL
    new1 <- NULL
    vote_spread <- NULL
    votes<-list()
    for(candid in 1:length(each_row[,candidates])){
      followers<-link[[as.character(each_row[candid,candidates])]]
      votes_vec<-list()
      if(!is.null(followers)){
        ball_s<-vector(length=max_vote_length)
        for(i in 1:max_vote_length){
          ballot<-vector()
          ballot[[1]]<-as.character(each_row[candid,candidates])
          ball<-.addFollowers(i, followers, each_row)
          ballot<-na.omit(c(ballot,ball))
          ball_s[i]<-list(c(ballot))
        }
        depth_vec<-sample(1:max_vote_length, as.numeric(as.character(each_row[candid,vote_spread])), replace=T)
        depth_vec<-data.table::data.table(depth_vec)
        depth_vec<-depth_vec[,c("new1"):=.addBallot(depth_vec,ball_s)]
        votes_1<-depth_vec[,new1]
      } else {
        ballot<-vector()
        ballot[[1]]<-as.character(each_row[candid,candidates])
        votes_1<-rep(list(c(ballot)), as.numeric(as.character(each_row[candid,vote_spread])))
      }

      # votes_1<-rep(list(c(ballot)), as.numeric(as.character(each_row[candid,vote_spread])))
      votes<-c(votes,list(votes_1))
      names(votes)<-c(names(votes)[1:(length(votes)-1) ], as.character(each_row[candid,candidates]))
    }
    return(votes)
  }
NULL


#' Internals
#'
#' @export
.av.count <-function(votes, counter){
  limit<-as.integer(sum(sapply(1:length(votes),function(x) length(votes[[x]])))/2)
  count_first<-votes
  is_w<-.is.winner(count_first,limit)
  counter1<-counter+1
  while(!is_w){
    list_names<-unlist(lapply(count_first,length))
    loser<-names(list_names)[which(list_names==min(list_names))]
    weakest_votes<-count_first[[which(list_names==min(list_names))]]
    count_first<-count_first[which(list_names==min(list_names))*-1]
    #is_w<-.is.winner(count_first,limit)
    if(length(count_first)<2)break
    count_first<-.redistributedVotes(count_first,weakest_votes)
    is_w<-.is.winner(count_first,limit)
    #print(is_w)
    #unlist(lapply(count_first,length))
    counter1<-counter1+1
  }
  return(c(list(count_first),list(counter1)))
}
NULL


#' Internals
#'
#' @export
.get_results <-
  function(votes, counter){
    fptp_c<-unlist(lapply(votes,length))
    av1<-.av.count(votes, counter)
    cc1<-av1[[1]]
    counter<-av1[[2]]
    av_c<-unlist(lapply(cc1,length))
    print(av_c)
    fptp_winner<-names(fptp_c)[which(fptp_c==max(fptp_c))]
    fptp_votes<-max(fptp_c)
    av_winner<-names(av_c)[which(av_c==max(av_c))]
    av_votes<-max(av_c)
    return(c(fptp_winner, fptp_votes, av_winner, av_votes, counter))
  }
NULL


#' Internals
#'
#' @export
#'
.timediff <-
  function(start_time) {
    start_time <- as.POSIXct(start_time)
    dt <- difftime(Sys.time(), start_time, units="secs")
    # Since I only want the H:M:S, it can ignore the date...
    # be careful about time-zone issues
    format(.POSIXct(dt,tz="GMT"), "%H:%M:%S")
  }
NULL

#' Internals
#'
#' @export
.redistributedVotes <-
  function(count_first,votes){
    for(i in 1:length(votes)){
      b=F
      if(length(votes[[i]])>1){
        for(j in 2:length(votes[[i]])){
          #if(!is.na(votes[[i]][[j]])){
          name<-votes[[i]][[j]]
          for(k in 1:length(count_first)){
            if(name==names(count_first)[[k]]){
              #print(paste(i,j,k,length(votes[[i]])))
              count_first[[k]]<-c(count_first[[k]],list(c(votes[[i]])))
              b=T
              #print(paste(i,j,k,b,length(votes[[i]])))
            }
          }
          if(b) break
        }
      }
    }
    return(count_first)
  }
NULL

#' Internals
#'
#' @export
#'
.is.winner <-
  function(count_first,limit){
    is_winner<-F
    if(length(count_first)==1)is_winner<-T
    for(n in 1:length(count_first)){
      if(length(count_first[[n]])>limit) is_winner<-T
    }
    return(is_winner)
  }
NULL

#' Internals
#'
#' @export
.getFollower <-
  function(follower_probability_map, vote_count){
    follower_names<-names(follower_probability_map)
    new_f<-vector()
    idx<-sample(1:vote_count,size=1)
    #idx<-min(5,vote_count)
    if(length(follower_probability_map)!=0){
      for(f in 1:length(follower_probability_map)){
        #print(follower_probability_map[[f]])
        l_u<-follower_probability_map[[f]]
        if(idx>=l_u[[1]] && idx<=l_u[[2]]) new_f<-c(new_f, follower_names[[f]])
      }
    }else(new_f<-as.null(new_f))
    return(new_f)
  }
NULL


#' Internals
#'
#' @export
.constructFollowerProbabilityMap <-
  function(followers, each_row){
    candidates <- NULL
    vote_spread <- NULL
    follower_probability_map<-list()
    follower_probability_names<-vector()
    counter<-0
    vt<-F
    for(f in 1: length(followers)){
      for(v in 1:length(each_row[,candidates])){
        if(followers[[f]]==as.character(each_row[v,candidates])) {vt<-T}
      }
      if(vt){
        follower_probability_map<-c(follower_probability_map, list(c(counter, counter + as.numeric(as.character(each_row[each_row$candidates==followers[[f]],vote_spread])))))
        follower_probability_names<-c(follower_probability_names, followers[[f]])
        counter<-counter+as.numeric(as.character(each_row[each_row$candidates==followers[[f]],vote_spread]))+1
        follower_probability_map<-structure(follower_probability_map, .Names=follower_probability_names)
      }else{as.null(follower_probability_map)}
      vt<-F
    }
    return(c(list(follower_probability_map),list(counter)))
  }
NULL

#' Internals
#'
#' @export
.addBallot <-
  function(x, ball_s){
    r1<-ball_s[x]
    return(r1)
  }
NULL

#' Internals
#'
#' @export
.addFollowers <- function(depth, followers, each_row){
  #vs<-as.numeric(as.character(each_row[,vote_spread]))
  ball<-character(depth)
  for(i in 1: depth){
    if(length(followers)>=i){
      #.follower_probability_map<-.constructFollowerProbabilityMap(followers, vs)[[1]]
      #.vote_count<-.constructFollowerProbabilityMap(followers, vs)[[2]]
      tmp<-.constructFollowerProbabilityMap(followers, each_row)
      follower_probability_map<-tmp[[1]]
      vote_count<-tmp[[2]]
      follower<-.getFollower(follower_probability_map, vote_count)
    }else break
    if(length(follower!=0)) ball[i]<-follower
  }
  return(ball)
}
NULL



#' Read/Write Multiple csv Files at a Time
#' 
#' \code{mcsv_w} - Read and assign multiple csv files at the same time.
#' 
#' @param files csv file(s) to read.   
#' @param a.names object names to assign the csv file(s) to.  If NULL assigns 
#' the csv to the name(s) of the csv file(s) in the global environment.
#' @param l.name A character vector of names to assign to the csv files 
#' (dataframes) being read in.  Default (NULL) uses the names of the files in 
#' the directory without the file extension.
#' @param list A character vector of length one to name the list being read in.  
#' Default is \code{"L1"}.
#' @param pos where to do the removal. By default, uses the current environment. 
#' @param envir the environment to use. 
#' @param \dots data.frame object(s) to write to a file
#' @export

readWriteCSV <-
function(files, a.names = NULL, l.name = NULL, list = TRUE, pos = 1,
    envir = as.environment(pos)){
    if (is.null(a.names)){
        a.names <- unlist(lapply(files, function(x){
            v <- unlist(strsplit(x, "/|\\\\"))
            v <- v[length(v)]
            gsub(".csv", "", v)
        }))
    }
    invisible(lapply(seq_along(files), function(i) {
        assign(a.names[i], read.csv(files[i]), envir = envir)
    }))
    if (list) {
        L1 <- lapply(a.names, function(x){
            get(x)
        }) 
        names(L1) <- a.names
        if (is.null(l.name)){
            l.name <- "L1"
        }
        assign(l.name, L1, envir = envir)
    }
    assi <- paste(c(l.name, a.names), collapse=", ")
    message(paste0("objects assigned: ", assi))
    invisible(a.names)
}
NULL