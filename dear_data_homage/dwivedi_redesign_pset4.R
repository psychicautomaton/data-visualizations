#set working directory
setwd('/Users/umadwivedi/Documents/Projects/YALE/senior/s1/PLSC 349 — data visualization/pset_4')
#import libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
hrbrthemes::import_roboto_condensed()


#read in data gleaned from dear_data image
dear_emo = tibble(read.csv('dear_data_scraped.csv'))
e = c("anxious_not_ok", 
      "stressed", 
      "confused",
      "blurry_clouded_ok",
      "ok",
      "unproductive",
      "nostalgic",
      "productive_motivated",
      "relaxed",
      "silly",
      "excited")

act_labels = c("waiting",
               "walking",
               "couch time",
               "events",
               "spa",
               "planning",
               "shopping",
               "lunch and dinner",
               "morning prep",
               "meetings",
               "dear data work",
               "talking about work",
               "projects",
               "email")

#factor categorical variables
dear_emo <- dear_emo %>%
  mutate(emotion = factor(emotion, levels = e),
         activity_gen_cat = as_factor(activity_gen_cat),
         activity_spec = factor(activity_spec, 
                                levels=c("waiting",
                                         "walking",
                                         "couch_evening",
                                         "events",
                                         "spa",
                                         "planning",
                                         "shopping",
                                         "lunch_dinner",
                                         "morning_prep",
                                         "meetings",
                                         "dear_data",
                                         "talking",
                                         "projects",
                                         "emails")),
         company = factor(company, levels=c("alone", "cowork", "bf", "friend")),
         company_2 = as_factor(company_2),
         weather = factor(weather, levels=c("ok", "not_ok")))
dear_emo <- dear_emo %>%
  mutate(emotion = reorder())
#save levels
emotions = levels(dear_emo$emotion)
activities = levels(dear_emo$activity_spec)
cats = levels(dear_emo$activity_gen_cat)

#initialize empty matrix
counts = data.frame(matrix(0, nrow = length(activities), ncol = length(emotions)))
colnames(counts) = emotions

#init empty list
totals = list()

#init empty cols
dear_emo$count = rep(0, nrow(dear_emo))
dear_emo$total = rep(0, nrow(dear_emo))
dear_emo$emo_per_act = rep(0, nrow(dear_emo))


#calc counts of each emotion / activity + total counts
#for each activity
for (i in c(1:length(activities))) {
  lev = lapply(list(activities), '[[', i)
  dat <- dear_emo %>% filter(activity_spec == lev)
  print(nrow(dat))
  totals[i] = nrow(dat)
  new = list(table(dat$emotion))
  for (j in c(1:length(new[[1]]))) {
    em = names(new[[1]])[j]
    counts[i,em] = new[[1]][em]

  }
  
}
#clean env
rm(i,em,j,dat,lev,new)

#rename matrix rows for clarity
rownames(counts) <- activities



#save count of relevant emotion, total of relevant activity, and % of each 
#activity comprised by each emotion for each row
for (i in c(1:nrow(dear_emo))){
  for (j in c(1:length(emotions))){
    for (k in c(1:length(activities))){
        if(dear_emo$activity_spec[i] == activities[k] & dear_emo$emotion[i] == emotions[j]){
         
           dear_emo$count[i] = as.integer(counts[k, j][[1]])
           dear_emo$total[i] = as.integer(totals[k])
           dear_emo$emo_per_act[i] = (as.integer(dear_emo$count[i]) / as.integer(dear_emo$total[i]))

        }
      }
    }
  }

#clean env
rm(i,j,k)


emo_labels = c("anxious/not ok",
               "stressed",
               "confused",
               "blurry/clouded but ok",
               "ok",
               "unproductive",
               "nostalgic",
               "productive/motivated",
               "relaxed",
               "silly",
               "excited")

act_labels = c("waiting",
               "walking",
               "couch time",
               "events",
               "spa",
               "planning",
               "shopping",
               "lunch and dinner",
               "morning prep",
               "meetings",
               "dear data work",
               "talking about work",
               "projects",
               "email")


weather.labs = c("Good Weather", "Bad Weather", "Any Weather", "Any Weather") 
names(weather.labs) = c("ok", "not_ok", "NA", "(all)")
company.labs = c("Alone", "With Coworkers/Clients", "With Boyfriend", "With Friends", "With Everyone", "With Everyone") 
names(company.labs) = c("alone", "cowork", "bf", "friend", "NA", "(all)")



#plot
p <- ggplot(dear_emo, 
           aes(x=as.numeric(emotion), y=activity_spec, fill=(emo_per_act))) + 
    geom_tile(color = "white",
              lwd = 1,
              linetype = 1) + 
    facet_grid(weather ~ company, 
               scales="free",  
               margins=TRUE, 
               labeller = labeller(weather = weather.labs, 
                                   company = company.labs),
               drop=TRUE) +
    theme_ft_rc() +
    scale_fill_gradient(low = "#00d4ff",
                      high = "#e43792",
                      guide = "colorbar") +
    scale_x_continuous(breaks = 1:length(emotions),
                     labels = emo_labels,
                     sec.axis = sec_axis(~.,
                                         breaks = 1:length(emotions),
                                         labels = emo_labels))+
    scale_y_discrete(labels = act_labels) +
    theme(axis.text.x.top = element_text(hjust=0),
          axis.text.x = element_text(angle = 90,
                                     margin = margin(t = .1, unit = "cm"),
                                     hjust = 1),
          plot.title=element_text(size=rel(3)),
          plot.subtitle = element_text(size=rel(1.8), color="orchid3"),
          axis.title.x = element_text(size=rel(2.25), margin=margin(t = .5, unit="cm"), color="deeppink3"),
          axis.title.y = element_text(size=rel(2.25), margin=margin(t = 1, unit="cm"), color="deeppink3"),
          strip.text = element_text(color = "lightskyblue2", size = rel(1.25)),
          strip.text.y = element_text(angle = 360),
          legend.title = element_text(size=rel(1.25), color = "white")) +
    labs(title = "Association between Activity and Emotional State",
         subtitle = " by company for and weather during each activity",
         x = "Emotional State (by Company Kept)",
         y = "Activity (by Weather)",
         fill = "Strength of Association\n(% of Activity in Emotional State)") #+
    #guides(col=guide_legend("Strength of Association (% of Activity in Emotional State)"))
    
    

#save
ggsave(filename = "dwivedi_dear_data_scraped.png", plot = p, width=25, height=18)

