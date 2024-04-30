library(stringr)
library(tibble)
library(readr)
library(dplyr)

##first write a function that can allow us to read the text in and transform into a data frame quickly
ReadDebate <- function(FolderName) {
  files <- list.files(FolderName, full.names = TRUE)
  df_list <- list()
  id <- numeric(0)  # Initialize id vector with no elements
  
  for (i in seq_along(files)) {
    speech <- strsplit(read_file(files[i]), 
                       "\n(?=El\\spresidente\\sdiputado)|\n(?=El\\svicepresidente\\sdiputado)|\n(?=El\\ssecretario\\sdiputado)|\n(?=El\\sdiputado)|\n(?=La\\ssecretaria\\sdiputada)|\n(?=La\\sdiputada)|\n(?=La\\spresidenta\\sdiputada)|\n(?=La\\svicepresidenta\\sdiputada)",
                       perl = TRUE)[[1]]
    
    if (length(speech) > 0) {
      df_list[[i]] <- tibble(content = speech)
      id <- c(id, rep(i, times = length(speech)))  # Assign id based on the number of speeches
    } else {
      df_list[[i]] <- NULL
    }
  }
  
  completeDF <- bind_rows(df_list)
  completeDF$id <- id
  
  return(completeDF)
}

Debate_Semester1<-ReadDebate(FolderName = "CongressDebate_semester1")


####------------------------------------------------------------------------------------------------------------------------------------------
##First we extract the TitleName:
Debate_Semester1$TitleName<-str_extract(Debate_Semester1$content,
                                        "^(El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada|La diputada|La presidenta diputada|La vicepresidenta diputada)\\s.*?(?:\\([^:]+\\))?:\\s")

##A total of 22 is mis-specified as they start with one of the following but it is part of the previous speech
##El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada
##|La diputada|La presidenta diputada|La vicepresidenta diputada
##For this reason, we inspect this 22 speech as below:
which(is.na(Debate_Semester1$TitleName))

##They are part of the moderation of the president or the secretary
#Therefore, we can directly ignore them since the speech of the president and the secretary
##will not be included in our analysis
Debate_Semester1[290,1]
Debate_Semester1[295,1]
Debate_Semester1[308,1]
Debate_Semester1[321,1]
Debate_Semester1[346,1]
Debate_Semester1[353,1]
Debate_Semester1[362,1]
Debate_Semester1[373,1]
Debate_Semester1[573,1]
Debate_Semester1[614,1]
Debate_Semester1[633,1]
Debate_Semester1[660,1]
Debate_Semester1[699,1]
Debate_Semester1[740,1]
Debate_Semester1[745,1]
Debate_Semester1[1586,1]
Debate_Semester1[3003,1]
Debate_Semester1[3258,1]
Debate_Semester1[3259,1]
Debate_Semester1[4986,1]
Debate_Semester1[5277,1]
Debate_Semester1[5277,1]
Debate_Semester1[7328,1]

Debate_Semester1<-na.omit(Debate_Semester1)



##Next, we will extract the title from Title Name and put it into a separate column:
##Now we will extract the title from the TitleName:
Debate_Semester1$Title<-str_extract(Debate_Semester1$TitleName,
                                    "^((El\\spresidente\\sdiputado)|(El\\svicepresidente\\sdiputado)|(El\\ssecretario\\sdiputado)|(El\\sdiputado)|(La\\ssecretaria\\sdiputada)|(La\\sdiputada)|(La\\spresidenta\\sdiputada)|(La\\svicepresidenta\\sdiputada))")

##Then, we will delete the title and make the name as a separate column:
Debate_Semester1$Name<-str_extract(Debate_Semester1$TitleName,
                                   "(?<=El\\spresidente\\sdiputado|El\\svicepresidente\\sdiputado|El\\ssecretario\\sdiputado|El\\sdiputado|La\\ssecretaria\\sdiputada|La\\sdiputada|La\\spresidenta\\sdiputada|La\\svicepresidenta\\sdiputada)[^(:]+")
Debate_Semester1$Name<-trimws(Debate_Semester1$Name)

##Then, we delete the TitleName so that we can have the speech in a separate column:
Debate_Semester1$Speech<-str_replace(Debate_Semester1$content,
                                     "^(El presidente diputado|El vicepresidente diputado|El secretario diputado|El diputado|La secretaria diputada|La diputada|La presidenta diputada|La vicepresideta diputada)\\s.*?(?:\\([^:]+\\))?:\\s",
                                     "")

##Next, we take only the id, Title, Name, and Speech Columns 
##and we will keep only the El diputado and La diputada:
Debate_Semester1<-Debate_Semester1 %>%
  select(id, Title, Name, Speech) %>%
  filter(Title=="El diputado" | Title=="La diputada")



####-----------------------------------------------------------------------------------------
##Read the table with date and id in:
Date_ID<-read.csv("semester1 id_date.csv", nrows = 26)

##Change the column name to be able to merge two data:
colnames(Date_ID)[1]<-"id"

##Merge two data to have the date for our debate data:
Debate_Semester1<-merge(x=Debate_Semester1,
                        y=Date_ID,
                        by.x = "id",
                        by.y = "id")

##Now, we can take out the id column since this is only for the merging:
Debate_Semester1<-Debate_Semester1 %>%
  select(-id)

##Remove the Date_ID data after the merging to clear up space in the environment:
rm(Date_ID)


NamesOfCongressperson<-Debate_Semester1 %>%
  group_by(Name) %>%
  summarise(count=n())
