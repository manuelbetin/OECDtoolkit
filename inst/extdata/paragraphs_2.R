#'@title Text generation
#'@description generate dynamic text using rLDCP algorithm: the file
#'take as input a dataset with 4 variables and produce
#'@author Federica De Pace, Manuel Betin, Naomi Cohen
#'@return a dataset containing the values of the indicator for each country
#'and the paragraph of text interpretating the data
#'@export a ldcp object containing the text generated
#'

#library(rLDCP)

####################################################################
###################### input file definition #######################
####################################################################
# input1 <- read.csv(file="htk_paragraphs.csv",
# sep=";", dec=".",
# header=TRUE)

####################################################################
###################### data definition #############################
####################################################################

input1=data.frame(mydt)

replace_value_na=0 #default value when missing data

non_num_vars=input1 %>% select_if(is.character)
#
colsnum=(2+length(main_vars)):(2*length(main_vars)+1)

#colsnum=2:length(main_vars)
myvars=lapply(colsnum,function(x){
  if(main_vars_direction[x-4]=="decreasing"){
    myvar <- 1-c(base::t(input1[x]))
  }else{
    myvar <- c(base::t(input1[x]))
  }
  myvar=myvar%>%replace_na(replace_value_na)
  myvar
})

rm(input1)

#provide the labels to the indicators and the categories that will be
#used in the text as well as definitions for each categories

definition=c(efficiency=" measures the economy’s capacity to align housing supply with demand, thereby limiting excessive price and rent increases, contributing to macroeconomic stability and facilitating residential mobility",
             inclusiveness=" refers to the housing sector’s capacity to deliver adequate and affordable homes across the income distribution while limiting residential segregation",
             sustainability=" assesses the housing sector's readiness for the transition to a low-carbon economy and its capacity to attenuate pressures on the ecosystem by preserving biodiversity and residents' health")


mycategory_label=tolower(category)
mycategory_definition=definition[[mycategory_label]]
myvar1_label=main_vars_label[1]

myvar2_label=main_vars_label[2]

myvar3_label=main_vars_label[3]


myvar1_label_long=main_vars_label_long[1]

myvar2_label_long=main_vars_label_long[2]

myvar3_label_long=main_vars_label_long[3]
####################################################################
###################### Data Structure definition ##################
####################################################################

input <- c()

my_method <- function (input){
  input
}

my_data <- data_structure(input,my_method)

####################################################################
###################### CP definition ###############################
####################################################################

#3 main indicators
cp_myvar1 <- cp("cp_myvar1", c("nodata","myvar1_lowtail", "myvar1_low", "myvar1_slow", "myvar1_medium", "myvar1_shigh", "myvar1_high","myvar1_uptail"))
cp_myvar2 <- cp("cp_myvar2", c("nodata","myvar2_lowtail", "myvar2_low", "myvar2_slow", "myvar2_medium", "myvar2_shigh", "myvar2_high","myvar2_uptail"))
cp_myvar3 <- cp("cp_myvar3", c("nodata","myvar3_lowtail", "myvar3_low", "myvar3_slow", "myvar3_medium", "myvar3_shigh", "myvar3_high","myvar3_uptail"))

cp_profile <- cp("cp_profile", c("Homogeneous","Heterogeneous"))
cp_Efficiency <- cp("cp_Efficiency", c("all_low","avg","all_high"))

####################################################################
###################### PMs definition ##############################
####################################################################

#########################################
## myvar1: first main indicator
g_pm_myvar1 <- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(
    triangle_mf(0, 0, 0),
    triangle_mf(0.00001 , 0.1, 0.15),
    trapezoid_mf(0.15, 0.15, 0.35, 0.35) ,
    trapezoid_mf(0.3, 0.35, 0.4, 0.45) ,
    triangle_mf(0.45, 0.5, 0.6) ,
    trapezoid_mf(0.55, 0.6, 0.65, 0.65),
    trapezoid_mf(0.65, 0.65, 0.85, 0.85),
    triangle_mf(0.85, 0.95, 1)), u)
  y }

  t_pm_myvar1 <- function(y){
    if(mycategory_label=="efficiency") {
    templates <- c(
      paste0(" ",myvar1_label," there is unfortunately no data available for targetcountry"),
      paste0(" In targetcountry, ", myvar1_label, ", which measure the overall affordability of housing, are among the lowest in OECD countries"),
      paste0(" In targetcountry, ", myvar1_label, ", which measure the overall affordability of housing, are low compared with other OECD countries"),
      paste0(" In targetcountry, ", myvar1_label, ", which measure the overall affordability of housing, are low compared with other OECD countries"),
      paste0(" In targetcountry, ", myvar1_label, ", which measure the overall affordability of housing, are close with OECD average"),
      paste0(" In targetcountry, ", myvar1_label, ", which measure the overall affordability of housing, are high compared with other OECD countries"),
      paste0(" In targetcountry, ", myvar1_label, ", which measure the overall affordability of housing, are high compared with other OECD countries"),
      paste0(" In targetcountry, ", myvar1_label, ", which measure the overall affordability of housing, are among the highest in OECD countries")
    )
  }
 else if(mycategory_label=="inclusiveness") {
    templates <- c(
      paste0(" ",myvar1_label," there is unfortunately no data available for targetcountry"),
      paste0(" targetcountry displays among the lowest ",myvar1_label, " measured as the ", myvar1_label_long ),
      paste0(" targetcountry displays relatively low ",myvar1_label, ", measured as the ", myvar1_label_long),
      paste0(" targetcountry displays relatively low ",myvar1_label,", measured as the ", myvar1_label_long),
      paste0(" targetcountry displays average ",myvar1_label,", measured as the ", myvar1_label_long),
      paste0(" targetcountry displays relatively high ",myvar1_label,", measured as the ", myvar1_label_long),
      paste0(" targetcountry displays relatively high ",myvar1_label, ", measured as the ", myvar1_label_long),
      paste0(" targetcountry displays among the highest ",myvar1_label, ", measured as the ", myvar1_label_long)
    )
 }
    else if(mycategory_label=="sustainability"){
      templates <- c(
        paste0(" ",myvar1_label," there is unfortunately no data available for targetcountry"),
        paste0(" targetcountry displays among the lowest ",myvar1_label),
        paste0(" targetcountry displays relatively low ",myvar1_label),
        paste0(" targetcountry displays relatively low ",myvar1_label),
        paste0(" targetcountry displays  average ",myvar1_label),
        paste0(" targetcountry displays relatively high ",myvar1_label),
        paste0(" targetcountry displays relatively high ",myvar1_label),
        paste0(" targetcountry displays among the highest ",myvar1_label)
      )
    }
    return(templates[which.max(y$w)])
  }




pm_myvar1 <- rLDCP::pm(y=cp_myvar1, g=g_pm_myvar1, t=t_pm_myvar1)

#########################################
## myvar2: second main indicator
g_pm_myvar2<- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(
    triangle_mf(0, 0, 0),
    triangle_mf(0.00001 , 0.1, 0.15),
    trapezoid_mf(0.15, 0.15, 0.35, 0.35) ,
    trapezoid_mf(0.3, 0.35, 0.4, 0.45) ,
    triangle_mf(0.45, 0.5, 0.6) ,
    trapezoid_mf(0.55, 0.6, 0.65, 0.65),
    trapezoid_mf(0.65, 0.65, 0.85, 0.85),
    triangle_mf(0.85, 0.95, 1)), u)
  y }

t_pm_myvar2 <- function(y){
  if(mycategory_label=="efficiency") {
    templates <- c(
      paste0(" ",myvar2_label," there is unfortunately no data available for targetcountry"),
      paste0(" targetcountry displays among the lowest ",myvar2_label, ", which would suggest appropriate supply responsiveness on average across regions and localities"),
      paste0(" targetcountry displays relatively low ",myvar2_label, ", which would suggest appropriate supply responsiveness on average across regions and localities"),
      paste0(" targetcountry displays relatively low ",myvar2_label, ", which would suggest appropriate supply responsiveness on average across regions and localities"),
      paste0(" targetcountry displays  average ", myvar2_label, ", which would suggest appropriate supply responsiveness on average across regions and localities"),
      paste0(" targetcountry displays relatively high ",myvar2_label, ", which would suggest comparatively low supply responsiveness on average across regions and localities"),
      paste0(" targetcountry displays relatively high ",myvar2_label, ", which would suggest comparatively low supply responsiveness on average across regions and localities"),
      paste0(" targetcountry displays among the highest ",myvar2_label, ", which would suggest comparatively low supply responsiveness on average across regions and localities")
    )
} else if (mycategory_label=="inclusiveness") {
    templates <- c(
      paste0(" ",myvar2_label," there is unfortunately no data available for targetcountry"),
      paste0(" it is among the countries with the lowest level of ", myvar2_label,", measured by the ", myvar2_label_long ),
      paste0(" it ranks relatively low in terms of ",myvar2_label, ", measured by the ", myvar2_label_long),
      paste0(" it ranks relatively low in terms of ",myvar2_label, ", measured by the ", myvar2_label_long),
      paste0(" it is located close to OECD average in terms of ",myvar2_label, ", measured by ", myvar2_label_long),
      paste0(" it ranks relatively high in terms of ",myvar2_label, ", measured by the ", myvar2_label_long),
      paste0(" it ranks relatively high in terms of ",myvar2_label, ", measured by the ", myvar2_label_long),
      paste0("it is among the countries with the highest level of ",myvar2_label, ", measured by the ", myvar2_label_long)
    )
  } else if (mycategory_label=="sustainability") {
      templates <- c(
        paste0(" ",myvar2_label," there is unfortunately no data available for targetcountry"),
        paste0(" it is among the countries with the lowest level of ", myvar2_label),
        paste0(" it ranks relatively low in terms of ", myvar2_label),
        paste0(" it ranks relatively low in terms of ", myvar2_label),
        paste0(" it is located close to OECD average in terms of ", myvar2_label),
        paste0(" it ranks relatively high in terms of ",myvar2_label),
        paste0(" it ranks relatively high in terms of ",myvar2_label),
        paste0("it is among the countries with the highest level of ",myvar2_label)
      )
    }
    return(templates[which.max(y$w)])
  }


pm_myvar2 <- rLDCP::pm(y=cp_myvar2, g=g_pm_myvar2, t=t_pm_myvar2)

#########################################
## myvar3: third main indicator
g_pm_myvar3<- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(
    triangle_mf(0, 0, 0),
    triangle_mf(0.00001 , 0.1, 0.15),
    trapezoid_mf(0.15, 0.15, 0.35, 0.35) ,
    trapezoid_mf(0.3, 0.35, 0.4, 0.45) ,
    triangle_mf(0.45, 0.5, 0.6) ,
    trapezoid_mf(0.55, 0.6, 0.65, 0.65),
    trapezoid_mf(0.65, 0.65, 0.85, 0.85),
    triangle_mf(0.85, 0.95, 1)), u)
  y }

t_pm_myvar3 <- function(y){
 if(mycategory_label=="efficiency") {
    templates <- c(
      paste0(" ",str_to_sentence(myvar3_label),", as measured by the ", myvar3_label_long, ", is not available for this targetcountry"),
      paste0(" ",str_to_sentence(myvar3_label),", as measured by the ", myvar3_label_long, ", is among the lowest"),
      paste0(" ",str_to_sentence(myvar3_label),", as measured by the ", myvar3_label_long, ", is relatively low by international comparison"),
      paste0(" ",str_to_sentence(myvar3_label),", as measured by the ", myvar3_label_long, ", is slighly below the average of OECD countries"),
      paste0(" ",str_to_sentence(myvar3_label),", as measured by the ", myvar3_label_long, ", is close to the average of OECD countries"),
      paste0(" ",str_to_sentence(myvar3_label),", as measured by the ", myvar3_label_long, ", is slighly above the average of OECD countries"),
      paste0(" ",str_to_sentence(myvar3_label),", as measured by the ", myvar3_label_long, ", is relatively high by international comparison"),
      paste0(" ",str_to_sentence(myvar3_label),", as measured by the ", myvar3_label_long, ", is among the highest in OECD countries")
    )
 } else if (mycategory_label=="inclusiveness" & myvar3_label=="commuting time" ) {
    templates <- c(
      paste0(" ", myvar3_label," there is unfortunately no data available for targetcountry"),
      paste0("average ", myvar3_label, " is among the lowest by international comparison" , "which suggests that very few workers live far from their workplace. This hints at comparatively efficient residential mobility, efficient worker-job matching and low levels of exclusion"),
      paste0("average ", myvar3_label, " is relatively low by international comparison", "which suggests that few workers live far from their workplace. This hints at comparatively efficient residential mobility, efficient worker-job matching and low levels of exclusion"),
      paste0("average ", myvar3_label, " is relatively low by international comparison, which suggests that few workers live far from their workplace. This hints at comparatively efficient residential mobility, efficient worker-job matching and low levels of exclusion"),
      paste0("average ", myvar3_label, " is in line with the OECD average, which suggests that some workers live far from their workplace. This hints at average levels of residential mobility, moderate worker-job matching and average levels of exclusion"),
      paste0("average ", myvar3_label, " is relatively high by international comparison, which suggests that many workers live far from their workplace. This hints at comparatively constrained residential mobility, weak worker-job matching and high levels of exclusion"),
      paste0("average ", myvar3_label, " is relatively high by international comparison, which suggests that many workers live far from their workplace. This hints at comparatively constrained residential mobility, weak worker-job matching and high levels of exclusion"),
      paste0("average ", myvar3_label, " is among the highest by international comparison, which suggests that most workers live far from their workplace. This hints at comparatively constrained residential mobility, weak worker-job matching and high levels of exclusion")
    )
 } else if (mycategory_label=="inclusiveness" & myvar3_label=="equality of access \nto culture in cities" ) {
   templates <- c(
     paste0(" ", myvar3_label," there is unfortunately no data available for targetcountry"),
     paste0("Accessibility of cultural point of interest is very unequal between people living in urban centers and people residing in commuting areas, suggesting a relatively high level of segregation and a low level of inclusiveness by international comparison"),
     paste0("Accessibility of cultural point of interest is very unequal between people living in urban centers and people residing in commuting areas, suggesting a relatively high level of segregation and a low level of inclusiveness by international comparison"),
     paste0("Accessibility of cultural point of interest is rather unequal between people living in urban centers and people residing in commuting areas, suggesting a average level of segregation and a average level of inclusiveness by international comparison"),
     paste0("Accessibility of cultural point of interest is rather unequal between people living in urban centers and people residing in commuting areas, suggesting a average level of segregation and a average level of inclusiveness by international comparison"),
     paste0("Accessibility of cultural point of interest is fairly equal between people living in urban centers and people residing in commuting areas, suggesting a relatively low level of segregation and a high level of inclusiveness by international comparison"),
     paste0("Accessibility of cultural point of interest is fairly equal between people living in urban centers and people residing in commuting areas, suggesting a relatively low level of segregation and a high level of inclusiveness by international comparison"),
     paste0("Accessibility of cultural point of interest is fairly equal between people living in urban centers and people residing in commuting areas, suggesting a relatively low level of segregation and a high level of inclusiveness by international comparison")
     )
 }else if (mycategory_label=="inclusiveness" & myvar3_label!="commuting time" & myvar3_label!="equality of access \nto culture in cities") {
   templates <- c(
     paste0(" ", myvar3_label," there is unfortunately no data available for targetcountry"),
     paste0("average ", myvar3_label, " is among the lowest by international comparison"),
     paste0("average ", myvar3_label, " is relatively low by international comparison"),
     paste0("average ", myvar3_label, " is relatively low by international comparison"),
     paste0("average ", myvar3_label, " is in line with the OECD average"),
     paste0("average ", myvar3_label, " is relatively high by international comparison"),
     paste0("average ", myvar3_label, " is relatively high by international comparison"),
     paste0("average ", myvar3_label, " is among the highest by international comparison")
   )
 }  else if (mycategory_label=="sustainability"& myvar3_label=="urban area biodiversity") {
   templates <- c(
     paste0(" ", myvar3_label," there is unfortunately no data available for targetcountry"),
     paste0(" In targetcountry, the coverage of natural and semi-natural land in urban areas is among the lowest by international comparison, indicating a high degree of urbanization across the country on average"),
     paste0(" In targetcountry, the coverage of natural and semi-natural land in urban areas is relatively low by international comparison, indicating a high degree of urbanization across the country on average"),
     paste0(" In targetcountry, the coverage of natural and semi-natural land in urban areas is relatively low by international comparison, indicating a high degree of urbanization across the country on average"),
     paste0(" In targetcountry, the coverage of natural and semi-natural land in urban areas is in line with OECD average, indicating an average degree of urbanization across the country"),
     paste0(" In targetcountry, the coverage of natural and semi-natural land in urban areas is relatively high by international comparison, indicating a modest degree of urbanization across the country on average"),
     paste0(" In targetcountry, the coverage of natural and semi-natural land in urban areas is relatively high by international comparison, indicating a modest degree of urbanization across the country on average"),
     paste0(" In targetcountry, the coverage of natural and semi-natural land in urban areas is among the highest by international comparison, indicating a modest degree of urbanization across the country on average")
   )
 }else if (mycategory_label=="sustainability"& myvar3_label=="access to \ngreen space") {
   templates <- c(
     paste0(" ", myvar3_label," there is unfortunately no data available for targetcountry"),
     paste0(" In targetcountry, the average access to green space in cities is among the lowest by international comparison"),
     paste0(" In targetcountry, the average access to green space in cities is relatively low by international comparison"),
     paste0(" In targetcountry, the average access to green space in cities is relatively low by international comparison"),
     paste0(" In targetcountry, the average access to green space in cities is in line with OECD average"),
     paste0(" In targetcountry, the average access to green space in cities is relatively high by international comparison"),
     paste0(" In targetcountry, the average access to green space in cities is relatively high by international comparison"),
     paste0(" In targetcountry, the average access to green space in cities is among the highest by international comparison")
   )
 }else if (mycategory_label=="sustainability" & myvar3_label!="urban area \nbiodiversity"& myvar3_label!="access to green space") {
   templates <- c(
     paste0(" ", myvar3_label," there is unfortunately no data available for targetcountry"),
     paste0(" targetcountry's ", myvar3_label, " is among the lowest, by international comparison"),
     paste0(" targetcountry's ", myvar3_label, " is relatively low, by international comparison"),
     paste0(" targetcountry's ", myvar3_label, " is relatively low, by international comparison"),
     paste0(" targetcountry's ", myvar3_label, " is in line with the OECD average"),
     paste0(" targetcountry's ", myvar3_label, " is relatively high, by international comparison"),
     paste0(" targetcountry's ", myvar3_label, " is relatively high, by international comparison"),
     paste0(" targetcountry's ", myvar3_label, " is relatively low, by international comparison"),
     paste0(" targetcountry's ", myvar3_label, " is among the highest, by international comparison")
   )
 }

    return(templates[which.max(y$w)])
}

pm_myvar3 <- rLDCP::pm(y=cp_myvar3, g=g_pm_myvar3, t=t_pm_myvar3)

#########################################
# global performance over the three indicators
g_pm_Efficiency<- function(u,y){
  operator <- operator(  min , max )
  y$w <- infer_rules(fuzzy_rules(

    fuzzy_rule( 0, 1, 1, 1, 0, 0, 0, 0 ,
                0, 1, 1, 1, 0, 0, 0, 0 ,
                0, 1, 1, 1, 0, 0, 0, 0 ,
                0, 1, 0, 0),
    fuzzy_rule( 0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 1),
    fuzzy_rule( 0, 0, 0, 1, 1 ,1 ,0 , 0,
                0, 0, 0, 1, 1 ,1 ,0 , 0,
                0, 0, 0, 1, 1 ,1 ,0 , 0,
                0, 0, 1, 0)
  ), operator,
  list( u[[1]]$w, u[[2]]$w, u[[3]]$w ))
  y }

t_pm_Efficiency<- function(y){
  templates <- c(
    "",#" what make overall assessment on efficiency difficult",
    " Generally speaking, its performance are among the bottom OECD ones.",
    " Overall, its performance are in line with the rest of OECD countries.",
    " targetcountry is globally among the top OECD performers."
  )
  return(templates[which.max(y$w)])
}

pm_Efficiency <- rLDCP::pm(y=cp_Efficiency, g=g_pm_Efficiency, t=t_pm_Efficiency)

#########################################
# Homogeneity of performance over the three main indicators

g_pm_profile<- function(u,y){
  operator <- operator(  min , max )
  y$w <- infer_rules(fuzzy_rules(

    fuzzy_rule( 0, 1, 1, 1, 0, 0, 0, 0,
                0, 1, 1, 1, 0, 0, 0, 0,
                0, 1, 1, 1, 0, 0, 0, 0,
                0, 1),
    fuzzy_rule( 0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 0, 0, 1, 1, 1,
                0, 1),
    fuzzy_rule( 0, 0, 0, 1, 1, 1, 0, 0,
                0, 0, 0, 1, 1, 1, 0, 0,
                0, 0, 0, 1, 1, 1, 0, 0,
                0, 1)
  ), operator,
  list( u[[1]]$w, u[[2]]$w, u[[3]]$w ))
  y }

t_pm_profile<- function(y){
  templates <- c(
    paste0(" The overall performance of targetcountry regarding ", category, " is difficult to assess based on the three main indicators"),
    paste0(" targetcountry displays similar relative performances among all three dimensions of ", category, ":")
  )
  return(templates[which.max(y$w)])
}
pm_profile <- rLDCP::pm(y=cp_profile, g=g_pm_profile, t=t_pm_profile)

####################################################################
###################### GLMP definition ##############################
####################################################################

glmp_method <- function(pm,input){
  pm$pm_myvar1  <- pm_infer(pm$pm_myvar1, input[1])
  pm$pm_myvar2  <- pm_infer(pm$pm_myvar2, input[2])
  pm$pm_myvar3  <- pm_infer(pm$pm_myvar3, input[3])
  pm$pm_Efficiency  <- pm_infer(pm$pm_Efficiency, list(pm$pm_myvar1$y,pm$pm_myvar2$y,pm$pm_myvar3$y))
  pm$pm_profile  <- pm_infer(pm$pm_profile, list(pm$pm_myvar1$y,pm$pm_myvar2$y,pm$pm_myvar3$y))
  pm
}
my_glmp <- glmp(list(pm_myvar1 = pm_myvar1,
                     pm_myvar2 = pm_myvar2,
                     pm_myvar3 = pm_myvar3,
                     pm_Efficiency = pm_Efficiency,
                     pm_profile = pm_profile),
                glmp_method)
####################################################################
###################### Report Template definition ##################
####################################################################


report_no_data=function(var1,var2,var3){
  if((var1 == 0.00000) & (var2== 0.00000) & (var3 == 0.00000)){
    no_data= "There is no data available for this country."
  }else{
    no_data=1}
  return(no_data)
}

report_tail=function(myvar, nb_indicators){ #error in the function the rank is not displaying properly
  if(str_detect(pm_report(myvar), "tail")) {
    #x = rank(myvars[[nb_indicators]])
    #v = which(all_vars==ctry)
    #ctry_x=x[[v]]
    #rank_var=(length(myvars[[nb_indicators]])+1)-ctry_x
    #myperc_var=paste0(pm_report(myvar), ". Indeed, targetcountry is ranked ", toOrdinal::toOrdinal(rank_var,language="english"), " over ", length(myvars[[nb_indicators]]), " countries", " (located in the percentile ", round(myvar[["u"]], digits=3),")")
    myperc_var=paste0(pm_report(myvar),"")
  } else {

    myperc_var=paste0(pm_report(myvar),"")
  }
  return(myperc_var)
}

report_method <- function(properties,pm){

  ## specific case for no data at all ##

  no_data=report_no_data(pm$pm_myvar1$u,pm$pm_myvar2$u,pm$pm_myvar3$u)

  ## specific case for extreme values, include ranking of the country ##
  if(no_data==1){
    myperc_var1=report_tail(pm$pm_myvar1,1)
    myperc_var2=report_tail(pm$pm_myvar2,2)
    myperc_var3=report_tail(pm$pm_myvar3,3)

    paste (
      str_to_title(mycategory_label),mycategory_definition,".",
      #pm_report(pm$pm_profile),
      #pm_report(pm$pm_Efficiency),".",
      myperc_var1,".",

      if((str_detect(myperc_var1, "low") && str_detect(myperc_var2, "high"))|(str_detect(myperc_var1, "high") && str_detect(myperc_var2, "low"))) {
      " Conversely, "
      }
        else if( (str_detect(myperc_var1, "average") && ( ( str_detect(myperc_var2, "high"))| (str_detect(myperc_var2, "low"))) ) |
                 (str_detect(myperc_var2, "average") && ( ( str_detect(myperc_var1, "high"))| (str_detect(myperc_var1, "low"))) ))  {
     "" }
         else if ( (str_detect(myperc_var1, "average") &&   str_detect(myperc_var2, "average") ) ||
                   (str_detect(myperc_var1, "high") &&   str_detect(myperc_var2, "high") ) ||
                   (str_detect(myperc_var1, "low") &&   str_detect(myperc_var2, "low") ) )   {
      " Similarly, "
      },

      myperc_var2,".",

      if( (str_detect(myperc_var2, "low") && str_detect(myperc_var3, "high")) |
          (str_detect(myperc_var2, "high") && str_detect(myperc_var3, "low")) ) {
        " On the other hand, " }
     else if ( (str_detect(myperc_var2, "low") && str_detect(myperc_var3, "low")) |
               (str_detect(myperc_var2, "high") && str_detect(myperc_var3, "high"))) {
        " Furthermore, "
        },

      myperc_var3,
      ". Factors contributing to these outcomes include [to be completed by the desk]",
      sep="")
  } #close if(no_data)
}
properties = NULL
my_report <- report_template(properties,report_method)

####################################################################
###################### LDCP definition #############################
####################################################################

my_ldcp <- ldcp(data=my_data,glmp=my_glmp,report=my_report)
