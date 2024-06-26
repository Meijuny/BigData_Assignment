library(stringr)
library(tibble)
library(readr)
library(dplyr)
library(fuzzyjoin)

##First copy the below folder from the "BigData_Assignment" Zip
##"CongressDebate_semester1", "CongressDebate_semester2", "CongressDebate_semester3", "CongressDebate_semester4"
#and the below CSV files from the "BigData_Assignment" Zip into the working directory 
##"candidates.csv", "electoral_results.csv", 
##"semester1 id_date.csv", "semester2 id_date.csv", "semester3 id_date.csv", "semester4 id_date.csv"

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

##The below 6 are part of the debate from the speech of a congressman
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


####-----------------------------------------------------------------------------------------
####-----------------------------------------------------------------------------------------
####-----------------------------------------------------------------------------------------
##Use the same ReadDebate Function to read the files in:
Debate_Semester3<-ReadDebate(FolderName = "CongressDebate_semester3")

##First we extract the TitleName:
Debate_Semester3$TitleName<-str_extract(Debate_Semester3$content,
                                        "^(El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada|La diputada|La presidenta diputada|La vicepresidenta diputada)\\s.*?(?:\\([^:]+\\))?:\\s")

##To see if there are any misspecification of cutting the speech into rows:
which(is.na(Debate_Semester3$TitleName))

##A total of 36 are mis-specified as they start with one of the following but it is part of the previous speech
##El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada
##|La diputada|La presidenta diputada|La vicepresidenta diputada
##Therefore, we closely inspect this 36 speech:

##By looking at the previous speech, we will know from which speaker the speech is coming from
##the below 31 are part of the moderation from the chair or the secretary
#Therefore, we can directly ignore
Debate_Semester3[(986-1),"content"]
Debate_Semester3[(1013-1),"content"]
Debate_Semester3[(1046-1),"content"]
Debate_Semester3[(1047-1),"content"]
Debate_Semester3[(1048-1),"content"]
Debate_Semester3[(1049-1),"content"]
Debate_Semester3[(1050-1),"content"]
Debate_Semester3[(1088-1),"content"]
Debate_Semester3[(3033-1),"content"]
Debate_Semester3[(3151-1),"content"]
Debate_Semester3[(3200-1),"content"]
Debate_Semester3[(3207-1),"content"]
Debate_Semester3[(3220-1),"content"]
Debate_Semester3[(3616-1),"content"]
Debate_Semester3[(5999-1),"content"]
Debate_Semester3[(6805-1),"content"]
Debate_Semester3[(6824-1),"content"]
Debate_Semester3[(7594-1),"content"]
Debate_Semester3[(7595-1),"content"]
Debate_Semester3[(7602-1),"content"]
Debate_Semester3[(7603-1),"content"]
Debate_Semester3[(7604-1),"content"]
Debate_Semester3[(7645-1),"content"]
Debate_Semester3[(11203-1),"content"]
Debate_Semester3[(11768-1),"content"]
Debate_Semester3[(11926-1),"content"]
Debate_Semester3[(12047-1),"content"]
Debate_Semester3[(13714-1),"content"]
Debate_Semester3[(13863-1),"content"]
Debate_Semester3[(14188-1),"content"]
Debate_Semester3[(15056-1),"content"]

##The below 5 are part of the debate from the speech of a congressman
Debate_Semester3[(539-1),"content"] ##by El diputado José Gerardo Rodolfo Fernández Noroña
Debate_Semester3[(978-1),"content"] ##by El diputado Pedro Vázquez González
Debate_Semester3[(2914-1),"content"] ##by El diputado José Elías Lixa Abimerhi
Debate_Semester3[(4821-1),"content"] ##by El diputado Klaus Uwe Ritter Ocampo
Debate_Semester3[(8164-1),"content"] ##by La diputada Flora Tania Cruz Santos

##For these 5 texts, we are going to give the title name above to each of them:
Debate_Semester3[539,"TitleName"]<-"El diputado José Gerardo Rodolfo Fernández Noroña:"
Debate_Semester3[978,"TitleName"]<-"El diputado Pedro Vázquez González:"
Debate_Semester3[2914,"TitleName"]<-"El diputado José Elías Lixa Abimerhi:"
Debate_Semester3[4821,"TitleName"]<-"El diputado Klaus Uwe Ritter Ocampo:"
Debate_Semester3[8164,"TitleName"]<-"La diputada Flora Tania Cruz Santos:"

##We will omit the rest since the other 31 texts are part of the moderation 
##from the chair or the secretary
##Since the speech from the chair and secretary only serve as moderating function
##we will filter them out and not include in our analysis
##For this reason, we can omit the 31 texts directly here:
Debate_Semester3<-na.omit(Debate_Semester3)

##Next, we will extract the title from Title Name and put it into a separate column:
##Now we will extract the title from the TitleName:
Debate_Semester3$Title<-str_extract(Debate_Semester3$TitleName,
                                    "^((El\\spresidente\\sdiputado)|(El\\svicepresidente\\sdiputado)|(El\\ssecretario\\sdiputado)|(El\\sdiputado)|(La\\ssecretaria\\sdiputada)|(La\\sdiputada)|(La\\spresidenta\\sdiputada)|(La\\svicepresidenta\\sdiputada))")

##Then, we will delete the title from TitleName and make the name as a separate column:
Debate_Semester3$Name<-str_extract(Debate_Semester3$TitleName,
                                   "(?<=El\\spresidente\\sdiputado|El\\svicepresidente\\sdiputado|El\\ssecretario\\sdiputado|El\\sdiputado|La\\ssecretaria\\sdiputada|La\\sdiputada|La\\spresidenta\\sdiputada|La\\svicepresidenta\\sdiputada)[^(:]+")
Debate_Semester3$Name<-trimws(Debate_Semester3$Name)

##Then, we delete the TitleName so that we can have the speech in a separate column:
Debate_Semester3$Speech<-str_replace(Debate_Semester3$content,
                                     "^(El presidente diputado|El vicepresidente diputado|El secretario diputado|El diputado|La secretaria diputada|La diputada|La presidenta diputada|La vicepresideta diputada)\\s.*?(?:\\([^:]+\\))?:\\s",
                                     "")

##Next, we take only the id, Title, Name, and Speech Columns 
##and we will keep only the El diputado and La diputada:
Debate_Semester3<-Debate_Semester3 %>%
  select(id, Title, Name, Speech) %>%
  filter(Title=="El diputado" | Title=="La diputada")

####-----------------------------------------------------------------------------------------
##Read the table with date and id in for debate semester 2:
Date_ID3<-read.csv("semester3 id_date.csv", nrows = 30)

##Change the column name to be able to merge two data:
colnames(Date_ID3)[1]<-"id"

##Merge two data to have the date for our debate data:
Debate_Semester3<-merge(x=Debate_Semester3,
                        y=Date_ID3,
                        by.x = "id",
                        by.y = "id")

##Now, we can take out the id column since this is only for the merging:
Debate_Semester3<-Debate_Semester3 %>%
  select(-id)

##Remove the Date_ID data after the merging to clear up space in the environment:
rm(Date_ID3)



####-----------------------------------------------------------------------------------------
####-----------------------------------------------------------------------------------------
####-----------------------------------------------------------------------------------------
##Use the same ReadDebate Function to read the files in:
Debate_Semester4<-ReadDebate(FolderName = "CongressDebate_semester4")

##First we extract the TitleName:
Debate_Semester4$TitleName<-str_extract(Debate_Semester4$content,
                                        "^(El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada|La diputada|La presidenta diputada|La vicepresidenta diputada)\\s.*?(?:\\([^:]+\\))?:\\s")

##To see if there are any misspecification of cutting the speech into rows:
which(is.na(Debate_Semester4$TitleName))

##A total of 20 are mis-specified as they start with one of the following but it is part of the previous speech
##El presidente diputado|El diputado|El vicepresidente diputado|El secretario diputado|La secretaria diputada
##|La diputada|La presidenta diputada|La vicepresidenta diputada
##Therefore, we closely inspect this 20 speech:

##By looking at the previous speech, we will know from which speaker the speech is coming from
##the below 19 are part of the moderation from the chair or the secretary
#Therefore, we can directly ignore
Debate_Semester4[(623-1),"content"]
Debate_Semester4[(624-1),"content"]
Debate_Semester4[(701-1),"content"]
Debate_Semester4[(732-1),"content"]
Debate_Semester4[(1142-1),"content"]
Debate_Semester4[(1630-1),"content"]
Debate_Semester4[(1679-1),"content"]
Debate_Semester4[(1682-1),"content"]
Debate_Semester4[(1746-1),"content"]
Debate_Semester4[(2227-1),"content"]
Debate_Semester4[(5965-1),"content"]
Debate_Semester4[(6832-1),"content"]
Debate_Semester4[(6867-1),"content"]
Debate_Semester4[(8745-1),"content"]
Debate_Semester4[(9627-1),"content"]
Debate_Semester4[(9931-1),"content"]
Debate_Semester4[(9944-1),"content"]
Debate_Semester4[(9945-1),"content"]
Debate_Semester4[(12748-1),"content"]

##The below 1 are part of the debate from the speech of a congresswoman
Debate_Semester4[(10550-1),"content"] ##by La diputada Lilia Aguilar Gil

##For these 1 text, we are going to give the title name above to it:
Debate_Semester4[10550,"TitleName"]<-"La diputada Lilia Aguilar Gil:"

##We will omit the rest since the other 19 texts are part of the moderation 
##from the chair or the secretary
##Since the speech from the chair and secretary only serve as moderating function
##we will filter them out and not include in our analysis
##For this reason, we can omit the 19 texts directly here:
Debate_Semester4<-na.omit(Debate_Semester4)

##Next, we will extract the title from Title Name and put it into a separate column:
##Now we will extract the title from the TitleName:
Debate_Semester4$Title<-str_extract(Debate_Semester4$TitleName,
                                    "^((El\\spresidente\\sdiputado)|(El\\svicepresidente\\sdiputado)|(El\\ssecretario\\sdiputado)|(El\\sdiputado)|(La\\ssecretaria\\sdiputada)|(La\\sdiputada)|(La\\spresidenta\\sdiputada)|(La\\svicepresidenta\\sdiputada))")

##Then, we will delete the title from TitleName and make the name as a separate column:
Debate_Semester4$Name<-str_extract(Debate_Semester4$TitleName,
                                   "(?<=El\\spresidente\\sdiputado|El\\svicepresidente\\sdiputado|El\\ssecretario\\sdiputado|El\\sdiputado|La\\ssecretaria\\sdiputada|La\\sdiputada|La\\spresidenta\\sdiputada|La\\svicepresidenta\\sdiputada)[^(:]+")
Debate_Semester4$Name<-trimws(Debate_Semester4$Name)

##Then, we delete the TitleName so that we can have the speech in a separate column:
Debate_Semester4$Speech<-str_replace(Debate_Semester4$content,
                                     "^(El presidente diputado|El vicepresidente diputado|El secretario diputado|El diputado|La secretaria diputada|La diputada|La presidenta diputada|La vicepresideta diputada)\\s.*?(?:\\([^:]+\\))?:\\s",
                                     "")

##Next, we take only the id, Title, Name, and Speech Columns 
##and we will keep only the El diputado and La diputada:
Debate_Semester4<-Debate_Semester4 %>%
  select(id, Title, Name, Speech) %>%
  filter(Title=="El diputado" | Title=="La diputada")

####-----------------------------------------------------------------------------------------
##Read the table with date and id in for debate semester 2:
Date_ID4<-read.csv("semester4 id_date.csv", nrows = 25)

##Change the column name to be able to merge two data:
colnames(Date_ID4)[1]<-"id"

##Merge two data to have the date for our debate data:
Debate_Semester4<-merge(x=Debate_Semester4,
                        y=Date_ID4,
                        by.x = "id",
                        by.y = "id")

##Now, we can take out the id column since this is only for the merging:
Debate_Semester4<-Debate_Semester4 %>%
  select(-id)

##Remove the Date_ID data after the merging to clear up space in the environment:
rm(Date_ID4)


####------------------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------------------
##Bind the debate from semester 1 and semester 2 together:
Debate_All<-rbind(Debate_Semester1, Debate_Semester2, Debate_Semester3, Debate_Semester4)

##remove the Debates for each semester from the environment:
rm(Debate_Semester1, Debate_Semester2, Debate_Semester3, Debate_Semester4)

##Remove the accents from the names
Debate_All$Name<-iconv(Debate_All$Name, from="UTF-8",to="ASCII//TRANSLIT")

##Remove the accents from the speech
Debate_All$Speech<-iconv(Debate_All$Speech, from="UTF-8",to="ASCII//TRANSLIT")



####------------------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------------------
##The following steps are to merge the Debate_All data with the candidate data which contains 
##the district each congressman/congresswomen represent

##Since the names of congressman/congresswoman will be used to merge the two datasets,
##We will do the preparation for this 'name' identifier for merging as followings:

##First, we need to transform names of congressman/congresswoman in the Debate_All data
##to lower case
Debate_All<-Debate_All %>%
  mutate(Name=tolower(Name))

##As Mexicans have a very long name, such as "jose gerardo rodolfo fernandez norona", 
##We decided to make them in alphabetical order for an easier merging later (e.g. fernandez gerardo jose norona rodolfo)

##First we create a function that can make each name order alphabetically 
normalize_name <- function(name) {
  sapply(name, function(x) {paste(sort(unlist(strsplit(x, " "))), collapse=" ")})
  }

Debate_All$NormalizedName<-normalize_name(Debate_All$Name)

##Read the candidate data in:
candidates <- read.csv("./candidates.csv") 

##We will group parties into three coalitions: 
##1. government coalition
##2.(Main) opposition coalition
##3.(Other) opposition coalition named MC as below
##The rest of the parties will not have representative in the congress
##Therefore, it is not relevant to our research question and thus, we give them NA
candidates <- candidates %>%
  mutate(
    party = case_when(
      party %in% c("PAN", "PRI", "PRD", "PAN-PRI-PRD", "PAN-PRI", "PAN-PRD", "PRI-PRD", "MC") ~ "opposition",
      party %in% c("PVEM", "PT", "MORENA", "PES", "PVEM-PT-MORENA", "PVEM-PT", "PVEM-MORENA", "PT-MORENA") ~ "government",
      TRUE ~ NA_character_  # Assign NA to all other values
    )
  )

##To be able to merge with Debate_All data with the names of congressman/congresswomen, 
##we make all the names to lower case:
candidates <- candidates %>%
  mutate(candidate = tolower(candidate))

##Then, we also order each name alphabetically:
candidates$NormalizedName<- normalize_name(candidates$candidate)

##As the same congressman/congresswoman can be registered to multiple parties, such as 'PAN', 'PAN-PRI',
##but after categorizing the parties into coalition, we will have a lot of duplicate values, such as:
##Noel Mata Atlino were registered for multiple parties, but now he belongs to the same coalition 'opposition'

#We need to get rid of these duplicate values as below:
candidates <- candidates %>%
  distinct(candidate, party, .keep_all = TRUE) 


# remove rows where 'candidate' or 'party' is just an empty space or blank
candidates <- candidates %>%
  filter(candidate != " " & candidate != "", party != " " & party != "")

###Now, we can merge the Debate_All data and candidate data through the identifier 'NormalizedName'
##We will use stringdist_left_join function here as this will also match with the names with typo recorded during the debate,
##such as 'Espinoza' instead of 'Espinosa'
Debate_Candidate<-stringdist_left_join(Debate_All, candidates, by = "NormalizedName", method = "jw", max_dist = 0.1)

##We select the columns we need:
## select the columns of interest
Debate_Candidate<-Debate_Candidate %>% 
  select(NormalizedName.x, party, state, district, Speech, Date) %>% 
  rename("name" = "NormalizedName.x")

##For those who does has NA in the column of party or district or state,
##they are from proportional representative, which means that they are not representing any district
##Therefore, we will not include them in our analysis since it's not relevant to our research question:
Debate_Candidate<-na.omit(Debate_Candidate)

##Next, we need to create the identifier for each district:
Debate_Candidate<-Debate_Candidate %>% 
  mutate(state_district=paste(state, district, sep = "-"))  

##Validation!!!!!
##We first construct a list with district and the corresponding congressman/congresswoman
##For the district with two representatives, one of the representative is coming from proportional representative list
##This means that this congressman/congresswoman is not really representing this district
##Therefore, we need delete them from our data.

##The list with district and the corresponding congressman/congresswoman is as follows:
District_Representative<-Debate_Candidate %>%
  group_by(state_district, name) %>%
  summarise(count=n())

##The names of the congressman/congresswoman that was from the proportional representation are:
##chavez hector ruiz, acevedo diaz edna gisel, banez julieta mejia, arturo cruz gonzalez luis
##aguilar angelo kevin pina, gamboa miner pablo, chertorivski salomon woldenberg, castro cynthia iliana lope
##We will delete them from our data:
Debate_Candidate<-Debate_Candidate %>%
  filter(name!="chavez hector ruiz" & name!="acevedo diaz edna gisel" & name!="ibanez julieta mejia" & 
           name!="arturo cruz gonzalez luis" & 
           name!="gamboa miner pablo" & name!="chertorivski salomon woldenberg" & name!="chertorivsky salomon woldenberg" &  
           name!="castro cynthia iliana lopez" & name!="castro galarza yesenia" & name!="flores gerardo pena")


####-----------------------------------------------------------------------------------------
##To merge with this Debate_Candidate data with the election results:
##First, we read the election results in:
electoral_results <- read.csv("./electoral_results.csv")

##We use the column 'state_district' as identifier to merge Debate_Candidate data and electoral_results data
Debate_Candidate_Election<-left_join(Debate_Candidate, electoral_results,
                                     by="state_district")

##Next, we select the columns we need:
Debate_Candidate_Election<-Debate_Candidate_Election %>%
  select(name, party, state_district, Speech, Date, VOTE_SHARE_RULING)

##Clear the environment to have just the Debate_Candidate_Election Data
rm(list=c("candidates","Debate_All","Debate_Candidate","District_Representative",
          "electoral_results","normalize_name","ReadDebate"))


####--------------------------------------------------------------------------------------------------------------------------------
##Set up doc_id for the LDA:
Debate_Candidate_Election<-Debate_Candidate_Election %>%
  rownames_to_column()
colnames(Debate_Candidate_Election)[1]<-"doc_id"

##Change the column name "speech" to "text" for R to recognize when making the DTM
colnames(Debate_Candidate_Election)[5]<-"text"
