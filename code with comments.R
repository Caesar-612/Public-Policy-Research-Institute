library(tidyverse)
library(foreign)
library(labelled)
#use for get_label and labels function
library(haven)
#for crosstable
library(readstata13)
#read stata file
library(pollster)
#crosstable too
library(knitr)
library(kableExtra)
#to custimize the kable
library(sjlabelled)
#editing label
library(rlang)
#!!sym() return the labelled column in for loop

# read dta file
dta = read_dta("AVA_PRRI_Wave 2_final_wtd_banners_v3.dta")
aa <- dta%>% colnames()%>%str_subset("^Q")
dta%>%
  select(-c(aa, weight)) -> dtatry
#read sav file
sav = read_sav("AVA_PRRI_Wave 2_final_wtd.sav")
sav%>%
  merge(dtatry, by = "Respid") -> test1
dta1 = read_dta("AVS 2021 with Banners V2_coded open ends.dta")
dta2 = read_dta("AVA_PRRI_Wave 4_banners v2.dta")
dta3 = read_dta("AVA_PRRI_Wave 4.dta")
dta4 = read_dta("AVS 2021_final 9-30-21.dta")
#fixed the column to same name
colnames(dta1)[2] <- "weight"
#combine your data that have the same "Respid"
merge(dta1,dta4, by = "Respid")-> d1
merge(dta2,dta3[-6], by = "Respid")-> d2
#rename some variables inside the data to get same name
rename(d1, "race_m" = "racecat",
       "educ_m2" = "educat2",
       "region_m" = "region",
       "allcat_m" = "allcat",
       "reltrad_w4" = "reltrad2",
       "party_m" = "partycat",
       "ideo_m" = "ideocat")->d1

# IMPORTANT!! Please check your data set to see if there are republican_news similar name variable and vaccine related name variable






age_table <- function(x) {
  #If there is not group for age we can use transmute to create a column and ifelse to seperate the age into groups below
  x %>%
    select(age)%>%
    transmute(group1 = ifelse(age %in% 18:29, "B",
                              ifelse(age %in% 30:49, "C", ifelse(age %in% 50:64,
                                                                 "D", ifelse(age %in% 65:150, "E", "skip"))))) ->t1
  #combine the age group column back to data
  x <- cbind(x, as.vector(t1))
  #create group for education column
  values <- c("High school or Less", "High school or Less", "Some College",
              "College Graduate", "Post-Graduate", "skip")
  #replace the variables name with values first and second values will be High school or less then third will be some college
  x$Education <- values[x$educ]
  #Change the education group to simple letter
  x%>%
    mutate(edu_group1 = recode(Education, "High school or Less" = "F",
                               "Some College" = "G",
                               "College Graduate" = "H",
                               "Post-Graduate" = "I")) ->x
  #return the column that start with upper case Q which is the question column
  for (i in str_subset(colnames(x),"^Q")[1:5]) {
    #create cross table for age and education group
    crosstab(df = x, x= !!sym(i), y = group1, weight = weight,pct_type = "column") ->t1
    crosstab(df = x, x= !!sym(i), y= edu_group1, weight = weight, pct_type = "column") ->t2
    merge(t1, t2, key = t1[1])-> t3
  #calculate the average of each row
    All_Americans_A <- apply(t3%>%select(-i),1,sum)/8
 #create the sample size
    All_Americans_A[1] <- All_Americans_A[1]*4
 #add sample size and average into new column
    t4 <- cbind(All_Americans_A,t3)
    t4[t4 =="n"] <- "Unweighted sample size"
    #t4%>%
    #  as_tibble()-> t4
    #t4 <- t4[c(6,2,3,4,5,1),]
 #relocate the AllAmericans column after the question
 #col.name() adjust the column names for each column
 #add_header_above()create a subtitle for those column and make groups
 #caption = get_label(x[i]) called out the label inside i column and display
    t4%>%
      relocate(All_Americans_A, .after = i)%>%
      kable(caption =get_label(x[i]), digits = 0,
            col.names = c(" ", "All_Americans\nA", '18-30\nB', '30-49\nC', '50-64\nD', '65+\nE',
                          'HighSchool or Less\nF', 'Some College\nG', 'College  Gradute\nH',
                          'Post-Gradute\nI')) %>%
      kable_styling("striped", full_width = T,
                    position = "left", font_size = 8) %>%
      add_header_above(c(" " = 1, "All Americans" = 1, "Age groups" = 4, "Education" = 4))%>%
      print()
  }
}
#test the result
age_table(d1)

#build function called race_table()
race_table <- function(x){
  #str_subset(colnames(x),"^Q")[1:5] means pick the first five column that the column names are start with capital Q
  for (i in str_subset(colnames(x),"^Q")[1:5]){
    #create cross table for race, educatio, region and all american !!sym() can return the list for i
    r1 <- crosstab(df= x, x = !!sym(i), y = race_m, weight = weight, pct_type = "column")
    r2 <- crosstab(df= x, x = !!sym(i), y= educ_m2, weight = weight, pct_type = "column")
    r3 <- crosstab(df= x, x = !!sym(i), y= region_m, weight = weight, pct_type = "column")
    r4 <- crosstab(df = x, x = !!sym(i), y = allcat_m, weight =weight, pct_type = "column" )%>%select("All Americans")%>%
      rename("american" = "All Americans")
    #caculate the unweighted sample size and create a verctor
    x%>%
      group_by(race_m)%>%
      count()%>%
      table()%>%
      colnames()%>%
      as.numeric()%>%
      rev()->size1

    x%>%
      filter(race_m == 1)%>%
      group_by(educ_m2)%>%
      count()%>%
      table()%>%
      colnames()%>%
      as.numeric()%>%
      rev()->size2
    x%>%
      group_by(region_m)%>%
      count()%>%
      table()%>%
      colnames()%>%
      as.numeric()%>%rev() -> size3
    a <- c(1,size1)
    b <-  c(1,size2)
    c <-  c(1,size3)
    #addd the unweight sample size vector into the crosstable
    r1%>%
      filter(!!sym(i) != "n") %>%
      rbind(a) ->r1
    r2 %>%
      filter(!!sym(i) != "n")%>%
      rbind(b)%>%
      select(-i)->r2
    r3%>%
      filter(!!sym(i) != "n")%>%
      rbind(c)%>%
      select(-i) ->r3
    r5 <- cbind(r1,r2,r3,r4)
    #change the row name to Unweight sample size
    r5[r5 == 1] <- "Unweighted sample size"
    r5%>%
      relocate(american, .after = i)%>%
      kable(caption =get_label(x[i]), digits = 0,
            col.names = c(" ", "All_Americans\nA", 'White\nB', 'Black\nC', 'Hispanic\nD', 'Other\nE',
                          'Multiracial\nF', 'White, no 4-year college degree\nG', 'White, 4- year college degree +\nH',
                          'NorthEast\nI', 'MidWest\nJ', 'South\nK','West\nL')) %>%
      kable_styling("striped", full_width = T,
                    position = "left", font_size = 8) %>%
      add_header_above(c(" " = 1, "All Americans" = 1, "Race/Ethnicity" = 5, "White education" = 2, "Region" = 4))-> r5
    print(r5)
  }
}



religious_table <- function(x){
  for (i in str_subset(colnames(x),"^Q")[1:5]){
    r1 <- crosstab(df= x, x = !!sym(i), y = reltrad_w4, weight = weight, pct_type = "column")
    #select the column we need and save it back
    r1 <- r1[,1:13]
    #for r2 select(-i) will remove the column than we can avoid duplicate problem
    r2 <- crosstab(df = x, x = !!sym(i), y = allcat_m, weight =weight, pct_type = "column" )%>%select(-i)%>%
      rename("american" = "All Americans")
    #another way to create unweighted sample size column
    test1%>%
      group_by(reltrad_w2)%>%
      count()%>%
      data.frame()%>%
      select(n) ->size1
    #remove the surplus number
    size1 <- size1[-13,]
    a <- c(1,size1)
    r1%>%
      filter(!!sym(i) != "n") %>%
      rbind(a) ->r1

    r5 <- cbind(r1,r2)
    r5[r5 == 1] <- "Unweighted sample size"
    r5%>%
      relocate(american, .after = i)%>%
      kable(caption =get_label(x[i]), digits = 0,
            col.names =  c(" ", "All_Americans\nA", 'White\nevangelical\nProtestant\nB', 'White\nmainlineProtestant\nC', 'Black\nProtestant\nD', 'Hispanic\nProtestant\nE','Other\nnonwhite\nProtestant\nF', 'White\nCatholic\nG', 'Hispanic\nCatholic\nH', 'Mormon\nI', 'Other\nChristian\nJ', 'Jewish\nK', 'Other\nnon-Christian\nreligion\nL','Unaffiliated\nM')) %>%
      kable_styling("striped", full_width = T,
                    position = "left", font_size = 8) %>%
      add_header_above(c(" " = 1, "All Americans" = 1, "Religious Affiliation" = 12))-> r5
    print(r5)
  }
}



politics_table <- function(x){
  for (i in str_subset(colnames(x),"^Q")[1:5]){
    r1 <- crosstab(df= x, x = !!sym(i), y = party_m, weight = weight, pct_type = "column")
    #select the column we need and save it back
    r1 <- r1[,1:4]
    r2 <- crosstab(df = x, x = !!sym(i), y = republican_news, weight = weight, pct_type = "column")
    r2 <- r2[,1:5]
    r3 <- crosstab(df = x, x = !!sym(i), y = ideo_m, weight = weight, pct_type = "column")
    r3 <- r3[,1:4]
    r4 <- crosstab(df = x, x = !!sym(i), y = allcat_m, weight =weight, pct_type = "column" )%>%select(-i)%>%
      rename("american" = "All Americans")
    #caculate the unweighted sample size
    x%>%
      group_by(party_m)%>%
      count()%>%
      table()%>%
      colnames()%>%
      as.numeric()%>%
      rev()->size1

    x%>%
      group_by(republican_news)%>%
      count()%>%
      table()%>%
      colnames()%>%
      as.numeric()%>%
      rev()->size2
    x%>%
      group_by(ideo_m)%>%
      count()%>%
      table()%>%
      colnames()%>%
      as.numeric()%>%rev() -> size3
    #combine unweighted sample size into cross table
    a <- c(1,size1)
    b <-  c(1,size2)
    c <-  c(1,size3)
    r1%>%
      filter(!!sym(i) != "n") %>%
      rbind(a) ->r1
    r2 %>%
      filter(!!sym(i) != "n")%>%
      rbind(b)%>%
      select(-i)->r2
    r3%>%
      filter(!!sym(i) != "n")%>%
      rbind(c)%>%
      select(-i) ->r3
    r5 <- cbind(r1,r2,r3,r4)
    r5[r5 == 1] <- "Unweighted sample size"
    #adjust the cross table digits = 0 mean no demical number
    r5%>%
      relocate(american, .after = i)%>%
      kable(caption =get_label(x[i]), digits = 0,
            col.names =  c(" ", "All_Americans\nA", 'Republican\nB', 'Independent\nC', 'Democrat\nD', 'Fox News Republican\nE',
                           'Conservative TV (OANN, NewsMax) Republican\nF', 'No TV news Republican\nG', 'Mainstream news Republican\nH',
                           'Conservative\nI', 'Moderate\nJ', 'Liberal\nK')) %>%
      kable_styling("striped", full_width = T,
                    position = "left", font_size = 8) %>%
      add_header_above(c(" " = 1, "All Americans" = 1, "Party Affiliation, in survey" = 3, "Republicans (only) by trusted TV news sourc" = 4, "Political Ideolog" = 3))-> r5
    print(r5)
  }
}


vaccine_table <- function(x){
  for (i in str_subset(colnames(x),"^Q")[1:5]){
    r1 <- crosstab(df= x, x = !!sym(i), y = vaccine_m, weight = weight, pct_type = "column")
    r1 <- r1[,1:6]
    r2 <- crosstab(df = x, x = !!sym(i), y = hesitant_m, weight = weight, pct_type = "column")
    r2 <- r2[,1:4]
    r3 <- crosstab(df = x, x = !!sym(i), y = allcat_m, weight =weight, pct_type = "column" )%>%select(-i)%>%
      rename("american" = "All Americans")
    #another way to create unweighted sample size column
    x%>%
      group_by(vaccine_m)%>%
      count()%>%
      data.frame()%>%
      select(n) ->size1
    size1 <- size1[-6,]

    x%>%
      group_by(hesitant_m)%>%
      count()%>%
      data.frame()%>%
      select(n) ->size2
    size2 <- size2[-4,]

    a <- c(1,size1)
    b <-  c(1,size2)

    r1%>%
      filter(!!sym(i) != "n") %>%
      rbind(a) ->r1
    r2 %>%
      filter(!!sym(i) != "n")%>%
      rbind(b)%>%
      select(-i)->r2

    r4 <- cbind(r1,r2,r3)
    r4[r4 == 1] <- "Unweighted sample size"
    r4%>%
      relocate(american, .after = i)%>%
      kable(caption =get_label(x[i]), digits = 0,
            col.names =  c(" ", "All_Americans\nA", 'Have received at least one dose\nB', 'Will get as soon as possible\nC', 'Wait and see\nD', 'Only if required\nE', 'Will not get\nF', 'Have or will get\nG', 'Wait and see or only if required\nH', 'Will not get\nI')) %>%
      kable_styling("striped", full_width = T,
                    position = "left", font_size = 8) %>%
      add_header_above(c(" " = 1, "All Americans" = 1, "Vaccine status" = 5, "3-way vaccine status" = 3))-> r5
    print(r5)
  }
}
