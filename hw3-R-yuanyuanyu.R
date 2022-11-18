getwd()
setwd('D:/R????/HW_3')  # set my directory
df <- read.csv('HW_3/exams.csv', sep=',') #Import dataset to R
View(df)

#return the dataset structure
str(df)  
#return the first five rows of the exam database
head(df,5) 
#return the last five rows of the exam database
tail(df,5)  

#Look at the column names
colnames(df)  
# Rename columns which name contains dot
colnames(df)<-c('gender','race_ethnicity','parental_level_of_education',
                 'lunch','test_preparation_course','math_score',
                 'reading_score','writing_score')  

#Change the type of the gender column from character type (chr) to factor type.
df$gender<-as.character.factor(df$gender)
#Change the type of the test.preparation.course column from character type (chr) to factor type.
df$test_preparation_course<-as.character.factor(df$test_preparation_course)

#Add a new column "average_score" to the dataset with the average scores for the three exams "math.score", "reading.score" and "writing.score".
df$mean<-(df$math_score+df$reading_score+df$writing_score)/3
colnames(df)[9]<-c('average_score')

#Sort the exam dataframe on 'average_score'column
df_sorted<-df[order(df$average_score),]
View(df_sorted)

#Remove the "lunch" column from the exam dataset.
df[,!names(df)%in%'lunch']
df[,-grep('lunch',names(df))]
df[,-4]

#returns subset of students who have completed a test preparation course
subset(df,subset = test_preparation_course == 'completed')

new_df2<-new_df%>%
  group_by(age)%>%
  summarise(mean_n_score=mean(mean_n_score))
new_df2
as.numeric(new_df2[,c('age')])
new_df3<-apply(new_df2,2,as.numeric)
new_df3<-data.frame(new_df3)
ggplot(new_df3,aes(x=age))+geom_histogram(fill='lightblue',color='black')
ggplot(new_df3,aes(x=age,y=mean_n_score))+geom_histogram(fill='lightblue',color='black')


#test1
completion_counts <- df %>%
  group_by(test.preparation.course) %>%  # group by gender
  count()
completion_counts
completion_counts <- completion_counts %>%  
  mutate(label = paste((n/sum(completion_counts$n)*100), '%', sep = ''))  
dp<-ggplot(completion_counts, aes(x='', y=n, fill=test.preparation.course))+
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color="white") +
  coord_polar("y", start=0) +
  ggtitle("completion ratio")+
  theme_void()
print(dp)  #completed 33.5%, none 66.5%
