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


####------------------------------------------------------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------------------------------------------------------
Debate_Semester1<-ReadDebate(FolderName = "CongressDebate_semester1")

##First we extract the TitleName:
Debate_Semester1$TitleName<-str_extract(Debate_Semester1$content,
                                        "^(El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada|La diputada|La presidenta diputada|La vicepresidenta diputada)\\s.*?(?:\\([^:]+\\))?:\\s")

##A total of 22 is mis-specified as they start with one of the following but it is part of the previous speech
##El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada
##|La diputada|La presidenta diputada|La vicepresidenta diputada
##For this reason, we inspect this 22 speech as below:
which(is.na(Debate_Semester1$TitleName))

##By looking at the previous lines, we will understand from which speaker the speech is coming from
##The results show that they are part of the moderation of the president or the secretary
#Therefore, we can directly ignore them since the speech of the president and the secretary
##will not be included in our analysis
Debate_Semester1[(291-1),"content"]
Debate_Semester1[(296-1),"content"]
Debate_Semester1[(309-1),"content"]
Debate_Semester1[(322-1),"content"]
Debate_Semester1[(347-1),"content"]
Debate_Semester1[(354-1),"content"]
Debate_Semester1[(363-1),"content"]
Debate_Semester1[(374-1),"content"]
Debate_Semester1[(574-1),"content"]
Debate_Semester1[(615-1),"content"]
Debate_Semester1[(634-1),"content"]
Debate_Semester1[(661-1),"content"]
Debate_Semester1[(700-1),"content"]
Debate_Semester1[(741-1),"content"]
Debate_Semester1[(746-1),"content"]
Debate_Semester1[(1587-1),"content"]
Debate_Semester1[(3004-1),"content"]
Debate_Semester1[(3259-1),"content"]
Debate_Semester1[(3260-1),"content"]
Debate_Semester1[(4987-1),"content"]
Debate_Semester1[(5278-1),"content"]
Debate_Semester1[(7329-1),"content"]

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
##Read the table with date and id in for debate semester 1:
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




####-----------------------------------------------------------------------------------------
####-----------------------------------------------------------------------------------------
####-----------------------------------------------------------------------------------------
##Use the same ReadDebate Function to read the files in:
Debate_Semester2<-ReadDebate(FolderName = "CongressDebate_semester2")

##Start the cleaning of the text:
##First we extract the TitleName:
Debate_Semester2$TitleName<-str_extract(Debate_Semester2$content,
                                        "^(El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada|La diputada|La presidenta diputada|La vicepresidenta diputada)\\s.*?(?:\\([^:]+\\))?:\\s")

##To see if there are any misspecification of cutting the speech into rows:
which(is.na(Debate_Semester2$TitleName))

##A total of 23 are mis-specified as they start with one of the following but it is part of the previous speech
##El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada
##|La diputada|La presidenta diputada|La vicepresidenta diputada
##Therefore, we closely inspect this 23 speech:

##By looking at the previous speech, we will know from which speaker the speech is coming from
##the below 17 are part of the moderation from the chair or the secretary
#Therefore, we can directly ignore
Debate_Semester2[(2378-1),"content"] 
Debate_Semester2[(2763-1),"content"]
Debate_Semester2[(3751-1),"content"]
Debate_Semester2[(4536-1),"content"]
Debate_Semester2[(5961-1),"content"]
Debate_Semester2[(5964-1),"content"]
Debate_Semester2[(5967-1),"content"]
Debate_Semester2[(7364-1),"content"]
Debate_Semester2[(7720-1),"content"]
Debate_Semester2[(7791-1),"content"]
Debate_Semester2[(8270-1),"content"]
Debate_Semester2[(8634-1),"content"]
Debate_Semester2[(8680-1),"content"]
Debate_Semester2[(8918-1),"content"]
Debate_Semester2[(12413-1),"content"]
Debate_Semester2[(13599-1),"content"]
Debate_Semester2[(13627-1),"content"]

##The below 6 are part of the debate from the previous speech of a congressman
Debate_Semester2[(4853-1),"content"] ##by El diputado Pablo Amilcar Sandoval Ballesteros
Debate_Semester2[(9726-1),"content"] ##by El diputado José Gerardo Rodolfo Fernández Noroña
Debate_Semester2[(9737-1),"content"] ##by El diputado Emmanuel Reyes Carmona
Debate_Semester2[(9901-1),"content"] ##by La diputada Mariana Gómez del Campo Gurza
Debate_Semester2[(11892-1),"content"] ##La diputada Mariana Gómez del Campo Gurza
Debate_Semester2[(13694-1),"content"] ##El diputado Leonel Godoy Rangel

##For these 6 texts, we are going to give the title name above to each of them:
Debate_Semester2[4853,"TitleName"]<-"El diputado Pablo Amilcar Sandoval Ballesteros:"
Debate_Semester2[9726,"TitleName"]<-"El diputado José Gerardo Rodolfo Fernández Noroña:"
Debate_Semester2[9737,"TitleName"]<-"El diputado Emmanuel Reyes Carmona:"
Debate_Semester2[9901,"TitleName"]<-"La diputada Mariana Gómez del Campo Gurza:"
Debate_Semester2[11892,"TitleName"]<-"La diputada Mariana Gómez del Campo Gurza:"
Debate_Semester2[13694,"TitleName"]<-"El diputado Leonel Godoy Rangel:"

##We will omit the rest since the other 17 texts are part of the moderation 
##from the chair or the secretary
##Since the speech from the chair and secretary only serve as moderating function
##we will filter them out and not include in our analysis
##For this reason, we can omit the 17 texts directly here:
Debate_Semester2<-na.omit(Debate_Semester2)

##Next, we will extract the title from Title Name and put it into a separate column:
##Now we will extract the title from the TitleName:
Debate_Semester2$Title<-str_extract(Debate_Semester2$TitleName,
                                    "^((El\\spresidente\\sdiputado)|(El\\svicepresidente\\sdiputado)|(El\\ssecretario\\sdiputado)|(El\\sdiputado)|(La\\ssecretaria\\sdiputada)|(La\\sdiputada)|(La\\spresidenta\\sdiputada)|(La\\svicepresidenta\\sdiputada))")

##Then, we will delete the title and make the name as a separate column:
Debate_Semester2$Name<-str_extract(Debate_Semester2$TitleName,
                                   "(?<=El\\spresidente\\sdiputado|El\\svicepresidente\\sdiputado|El\\ssecretario\\sdiputado|El\\sdiputado|La\\ssecretaria\\sdiputada|La\\sdiputada|La\\spresidenta\\sdiputada|La\\svicepresidenta\\sdiputada)[^(:]+")
Debate_Semester2$Name<-trimws(Debate_Semester2$Name)

##Then, we delete the TitleName so that we can have the speech in a separate column:
Debate_Semester2$Speech<-str_replace(Debate_Semester2$content,
                                     "^(El presidente diputado|El vicepresidente diputado|El secretario diputado|El diputado|La secretaria diputada|La diputada|La presidenta diputada|La vicepresideta diputada)\\s.*?(?:\\([^:]+\\))?:\\s",
                                     "")

##Next, we take only the id, Title, Name, and Speech Columns 
##and we will keep only the El diputado and La diputada:
Debate_Semester2<-Debate_Semester2 %>%
  select(id, Title, Name, Speech) %>%
  filter(Title=="El diputado" | Title=="La diputada")

####-----------------------------------------------------------------------------------------
##Read the table with date and id in for debate semester 2:
Date_ID2<-read.csv("semester2 id_date.csv", nrows = 26)

##Change the column name to be able to merge two data:
colnames(Date_ID2)[1]<-"id"

##Merge two data to have the date for our debate data:
Debate_Semester2<-merge(x=Debate_Semester2,
                        y=Date_ID2,
                        by.x = "id",
                        by.y = "id")

##Now, we can take out the id column since this is only for the merging:
Debate_Semester2<-Debate_Semester2 %>%
  select(-id)

##Remove the Date_ID data after the merging to clear up space in the environment:
rm(Date_ID2)

####------------------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------------------
##Bind the debate from semester 1 and semester 2 together:
Debate_Semester1_2<-rbind(Debate_Semester1, Debate_Semester2)

NameOfCongressPerson<-Debate_Semester1_2 %>%
  group_by(Name) %>%
  summarise(count=n())
