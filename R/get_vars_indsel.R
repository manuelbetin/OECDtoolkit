#' @title select relevant variables based on new selection criteria
#' @description algorithm to select proper variables in
#' replacement of main variables when missing
#' @param dt_non_na a dataset containing the data non missing
#' @param type_var if policy or outcome variables
#' @return tibble with the proper selection of
#' variables
#' @author  Federica Depace

get_vars_indsel<-function(dt_non_na,type_var) {
# for each group: A, B, C
if (type_var=="outcomes"){
  myvec<-c("A","B","C","J")

  for (i in myvec) {
    for (n in 1:3) {
      name=paste0('n_rank',n, i)
     # print(name)
      assign(name, nrow(dt_non_na[with( dt_non_na,which(rank==n & Indicator==i)), ]))
    }
  }

  for (i in myvec) {
    name1=paste0('n_rank1', i)
    name2=paste0('n_rank2', i)
    name3=paste0('n_rank3', i)
    name_dt= paste0('newdf_', i)

    if (get(name1)==1) {
      assign( name_dt , dt_non_na[with( dt_non_na,which(rank==1 & Indicator==i )), ])
    } else if (get(name1) ==0 & get(name2) ==1) {
      assign( name_dt, dt_non_na[with( dt_non_na,which(rank==2 & Indicator==i )), ] )
    } else if (get(name1) ==0 & get(name2) ==0  & get(name3) ==1) {
      assign(name_dt, dt_non_na[with( dt_non_na,which(rank==3  & Indicator==i )), ])
    } else if (get(name1) ==0 & get(name2) ==0  & get(name3) ==0 & n_rank2J==1)  {
      assign(name_dt, dt_non_na[with( dt_non_na,which(rank==2  & Indicator=="J" )), ])
    } else if (get(name1) ==0 & get(name2) ==0  & get(name3) ==0 & n_rank2J==0  & n_rank3J==1) {
      assign(name_dt,dt_non_na[with( dt_non_na,which(rank==3  & Indicator=="J" )), ])
    } else if (get(name1) ==0 & get(name2) ==0  & get(name3) ==0 & n_rank2J==0  & n_rank3J==0) {
      empty<-data.frame(matrix(NA, nrow = 1, ncol =length(dt_non_na) ))
      colnames(empty)<-colnames(dt_non_na)
      assign(name_dt, empty ) #one row on NAs
    }
  }
    newdf<-rbind(newdf_A, newdf_B, newdf_C)
    newdf<-na.omit(newdf)
    newdf<-distinct(newdf)

    #need to: take care of 2 out of 3 variables! # take care of joker variables
}
if (type_var=="policy"){
  myvec<-c("A","B","C","D","E")
  for (i in myvec) {
    for (n in 1:3) {
      name=paste0('n_rank',n, i)
      # print(name)
      assign(name, nrow(dt_non_na[with( dt_non_na,which(rank==n & Indicator==i)), ]))
    }
  }
  for (i in myvec) {
    name1=paste0('n_rank1', i)
    name2=paste0('n_rank2', i)
    name3=paste0('n_rank3', i)
    name_dt= paste0('newdf_', i)

    if (get(name1)==1) {
      assign( name_dt , dt_non_na[with( dt_non_na,which(rank==1 & Indicator==i )), ])
    } else if (get(name1) ==0 & get(name2) ==1) {
      assign( name_dt, dt_non_na[with( dt_non_na,which(rank==2 & Indicator==i )), ] )
    } else if (get(name1) ==0 & get(name2) ==0  & get(name3) ==1) {
      assign(name_dt, dt_non_na[with( dt_non_na,which(rank==3  & Indicator==i )), ])
    }    else if (get(name1) ==0 & get(name2) ==0  & get(name3) ==0) {
      empty<-data.frame(matrix(NA, nrow = 1, ncol =length(dt_non_na) ))
      colnames(empty)<-colnames(dt_non_na)
      assign(name_dt, empty ) #one row on NAs
    }
  }

  # bind the non empty ones
  newdf<-rbind(newdf_A, newdf_B, newdf_C, newdf_D, newdf_E)
  newdf<-na.omit(newdf)
  newdf<-distinct(newdf)

  # try to make it of 5 variables
  if (length(newdf[,1])==4|length(newdf[,1])==3) {
      newdf_v1<-newdf %>% mutate(temp=paste(rank, Indicator))
      dt_non_na_v1<-dt_non_na %>% mutate(temp=paste(rank, Indicator))
      if (any(newdf_v1$temp!= "2 B") & # there is no B2
        any(dt_non_na_v1$temp=="2 B")){
        add_on<-dt_non_na_v1[ which(dt_non_na_v1$temp=="2 B"), ]
        newdf_v1<- rbind(newdf_v1,add_on )
  }   else if (any(newdf_v1$temp== "2 B") & any(newdf_v1$temp!= "2 A") & # there is  B2 already but not A2
            any(dt_non_na_v1$temp=="2 A")){
        add_on<-dt_non_na_v1[ which(dt_non_na_v1$temp=="2 A"), ]
        newdf_v1<- rbind(newdf_v1,add_on )
            } else if (any(newdf_v1$temp== "2 B") & any(newdf_v1$temp== "2 A") & # there is B2 already and A2
        any(dt_non_na_v1$temp=="3 A")){
          add_on<-dt_non_na_v1[ which(dt_non_na_v1$temp=="3 A"), ]
          newdf_v1<- rbind(newdf_v1,add_on )
            }
      newdf<-subset(newdf_v1, select=-c(temp))
  }
}

  return(newdf)

}


