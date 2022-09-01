### THIS SCRIPT ALLOWS YOU TO SIMULATE THE DATA FLOW OF THE SHINY APP
### IT IS USEFUL FOR TESTING NEW ELEMENTS


# Load functions

setwd("/home/mhira/shiny_apps/MHIRA-usage-report/") # make sure you are in the app folder, else source files will not be found

source("graphql_functions/getToken.R")
source("graphql_functions/getAssessments.R")

#Setting

token = getToken(Username = "shiny", Password = "Password@1")

assessments = getAssessments(token = token, url = url)


df = assessments %>% 
  mutate(
    createdAt = lubridate::as_datetime(createdAt),
    updatedAt = lubridate::as_datetime(updatedAt),
    dateTime = createdAt,
    status = factor(status, levels = c("COMPLETED", "CANCELLED", "OPEN_FOR_COMPLETION")))  %>%
  arrange(dateTime) %>%
  mutate(questInAssessment = map(assessments$questionnaires, nrow) %>% unlist) %>%
  group_by(status) %>% 
  mutate(questCount = cumsum(questInAssessment))


ggplot(df, aes(x = dateTime, y = questCount, group = status, linetype = status, colour = status)) + 
  geom_line(lwd = 1.3) + 
  geom_point(size = 2) + 
  ylab("cummulative number of questionnaires") + 
  scale_colour_brewer(palette = "Set1", direction = -1) + 
  theme_light() 
  

questCount = df %>% 
              unnest(questionnaires) %>% 
              group_by(status) %>%
              count(abbreviation) %>%
              ungroup %>% 
              arrange(desc(status), abbreviation,  n) %>%
              group_by(abbreviation) %>%
              mutate(pos = cumsum(n) - 0.5 * n) 


ggplot(q, aes(x = abbreviation, y = n, colour = status, fill = status)) +
  geom_bar(stat = "identity", alpha = 0.3) + 
  geom_text(aes(y = pos, label = n, group = status)) +
  scale_colour_brewer(palette = "Set1", direction = -1) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  ylab("count") + 
  xlab("Instrument") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
  
  
 




  
