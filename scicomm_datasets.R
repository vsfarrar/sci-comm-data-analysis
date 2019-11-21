#Datasets for Arik NPB 101D Science Communication project


#sc = joined sci-comm data 
sc<-read.csv(file="~/Desktop/data/scicomm_data_joined.csv")  

#mutate to create question groupings by theme 
sc<-sc %>%
  replace(is.na(.), 0) %>% #change to 0 for ease of math 
  mutate(scicommscore_begin = rowSums(.[23:45]), 
         scicommscore_final = rowSums(.[66:88]), 
         scicommscore_change = scicommscore_final - scicommscore_begin) %>% #add question groupings
mutate(communitysense_begin = rowSums(select_(.,"impact_begin", "impact2_begin", "convey_begin", "importscicomm_begin")), 
       communitysense_final = rowSums(select_(.,"impact_final", "impact2_final", "convey_final", "importscicomm_final")),
         confidence_begin = rowSums(select_(.,"explanation_begin", "explanation2_begin", "selfconf_begin","tellstory_begin")), 
         confidence_final = rowSums(select_(.,"explanation_final", "explanation2_final", "selfconf_final","tellstory_final")),
         identity_begin = rowSums(select_(.,"commpeer_begin", "commlay_begin", "idscicomm_begin", "commpeer2_begin", "commlay2_begin","idphysiocomm_begin")),
         identity_final = rowSums(select_(.,"commpeer_final", "commlay_final", "idscicomm_final", "commpeer2_final", "commlay2_final","idphysiocomm_final")),
         toolsable_begin = rowSums(select_(.,"tools_begin", "accurateinfo_begin", "assessaccuracy_begin")),
         toolsable_final = rowSums(select_(.,"tools_final", "accurateinfo_final", "assessaccuracy_final")))

sc[sc == 0] <- NA  #return 0 to NA

#further mutates to create a change score for the question groupings
sc<- sc %>%
  mutate(communitysense_change = communitysense_final - communitysense_begin, 
         confidence_change = confidence_final - confidence_begin,
         identity_change = identity_final - identity_begin,
         toolsable_change = toolsable_final - toolsable_begin)

#scid = long form of dataset with scicomm data only

scid<-sc %>% 
  select(id, group,scientist_begin:tellstory_begin, scientist_final:tellstory_final, scicommscore_begin:toolsable_change)%>%
  gather(question, value, scientist_begin:toolsable_change)%>%
  separate(col = question, into = c("question", "survey"), sep = "_") 
#minor clean up 
scid$survey<-as.factor(scid$survey) #as factor
scid$question<-as.factor(scid$question)

#populate percent column for qgroupings in scid. 
#This is the percent of the total possible score for each question grouping. 

scid <- scid %>%
mutate(perc = case_when(    #create percentages for each question grouping to increase interpretability
  question == "communitysense" ~ (value/40),
  question == "identity"  ~ (value/60), 
  question == "toolsable" ~ (value/30),
  question == "confidence" ~ (value/40))) %>%
  mutate(perc = perc*100)


#GROUPS DEFINED FOR PROJECT

#science identity question groupings by theme 
qgroupings<-c("communitysense", "confidence", "identity", "toolsable") #qgrouping levels for filtering
