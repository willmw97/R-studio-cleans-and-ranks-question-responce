
library(dplyr)
library(tidyr)
library(tidyverse)

health <- read.csv(file.choose())
health_stack <- health %>% 
  gather("Question", "Response", -1) %>%
mutate(Participant = X) %>%

mutate(Needs_Reverse = recode(Response,
                              `Somewhat Agree`= "Yes",
                              `Somewhat Disagree`= "No")) %>%

mutate(TempRecodeValue = recode(Response,
                                  `Strongly Agree` = 5,
                                  `Somewhat Agree` = 4,
                                  `Neither Agree nor Disagree` = 3,
                                  `Somewhat Disagree` = 2,
                                  `Strongly Disagree` = 1,
                                  .default = 0
                                  ))  %>%
  
mutate(TempReverseValue = recode(as.character(TempRecodeValue),
                                    `5` = 1,
                                    `4` = 2,
                                    `3`= 3,
                                    `2`= 4,
                                    `1` = 5,
                                    .default = 0)) %>%
mutate(Coded_Value = ifelse(Needs_Reverse == "Yes",
                               TempReverseValue,
                               TempRecodeValue
                              )) %>%
mutate(Question_Type = substr(Question, 1,2)) %>%
group_by(Question_Type, Participant) %>%
  summarise(
    Total_Value = sum(Coded_Value)
  )%>%
  spread(Question_Type, Total_Value) -> unstack_health 

write.csv(unstack_health, file = "final_health.csv",row.names=FALSE, na="")
  

  



