
con <- DBI::dbConnect(odbc::odbc(),
                      driver = "SQL Server",
                      server = "DESKTOP-3IEJBD7\\SQLEXPRESS",
                      databases = "COLLEGE",
                      trusted_connection = "True")
                   

####
df_classrooms<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."Classrooms"')
df_courses<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."Courses"')
df_departments<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."departments"')
df_students<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."Students"')
df_teachers<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."Teachers"')

#######q1

df_1 <- inner_join(df_classrooms,df_courses,by="CourseId")
df_2 <- inner_join(df_1,df_departments,by=c("DepartmentID") )
final_q1 <- df_2  %>%
  group_by(DepartmentName) %>% 
  summarise(stu_num = n_distinct(StudentId))
final_q1


#####q2
df_q2 = right_join(df_classrooms,df_students,by="StudentId")
df_q2 = right_join(df_q2,df_courses,by="CourseId")
df_q2 = filter(df_q2, DepartmentID=="1")

final_q2 <- df_q2  %>%
  group_by(CourseName) %>% 
  summarise(stu_num = n_distinct(StudentId)) 
final_q2
print(paste0("The total number of students:", sum(final_q2$stu_num)))

####q3

df_q3 = right_join(df_classrooms,df_students,by="StudentId")
df_q3 = right_join(df_q3,df_courses,by="CourseId")
df_q3 = filter(df_q3, DepartmentID=="2")

final_q3 <- df_q3  %>%
  group_by(CourseName) %>% 
  summarise(stu_num = n_distinct(StudentId)) 

small<- length(which(final_q3$stu_num[]<22))
big<- length(which(final_q3$stu_num[]>=22))

####q4
df_q4 <-df_students %>% group_by(Gender)
count(df_students,Gender)
###q5

df_1q5<- left_join(df_students,df_classrooms, by='StudentId')
df_1q5<-left_join(df_1q5,df_courses,by='CourseId')
df_3 <-df_1q5 %>%
  group_by(CourseId,Gender) %>% 
  summarise(stu_num = n_distinct(StudentId))
df_4 <-df_1q5 %>%
  group_by(CourseId) %>% 
  summarise(tot_stu_num = n()) 
df_5 <-merge(df_3,df_4, by='CourseId',all=TRUE)
df_5 <-df_5 %>%
  mutate(percentage=df_5$stu_num/df_5$tot_stu_num*100)
final_q5 <- df_5 %>% filter(percentage>70)
final_q5 <-left_join(final_q5,df_courses,by='CourseId')

select (final_q5,CourseId,CourseName,Gender,percentage)
####q6
df_1q6<- left_join(df_courses,df_departments ,by='DepartmentID')
df_1q6<-left_join(df_1q6,df_classrooms,by="CourseId")
df_3q6<-df_1q6 %>%
  filter((degree>80| (degree==100)))
final_q6=df_3q6 %>% group_by(DepartmentID,DepartmentName)%>%
  summarise(num_stu=n_distinct(StudentId))

###q7
df_1q7<- left_join(df_courses,df_departments ,by='DepartmentID')
df_1q7<-left_join(df_1q7,df_classrooms,by="CourseId")
df_3q7<-df_1q7 %>%
  filter((degree<60| (degree==100)))
final_q7=df_3q7 %>% group_by(DepartmentID,DepartmentName)%>%
  summarise(num_stu=n_distinct(StudentId))


###q8
df_1q8<- inner_join(df_classrooms,df_courses ,by="CourseId")
df_1q8_te<- inner_join(df_1q8,df_teachers, by="TeacherId")
df_1q8_te$tname = paste(df_1q8_te$FirstName.x,df_1q8_te$LastName.x)

final_1q8 <-df_1q8 %>% group_by(TeacherId) %>%
 summarise(avg_degrees=mean(degree,na.rm=TRUE))
final_1q8<- final_1q8 %>% arrange(desc(avg_degrees)) 
final_1q8<- final_1q8 %>% select(tname,avg_degrees)


####q9

df_1q9 = inner_join(df_courses, df_teachers, by='TeacherId')
df_1q9 = inner_join(df_1q9, df_classrooms, by='CourseId')
df_1q9 = inner_join(df_1q9,df_students,by='StudentId')
df_1q9 = inner_join(df_1q9, df_departments, by= 'DepartmentID')

df_1q9 = select (df_1q9 ,DepartmentID,DepartmentName,
         CourseId,CourseName,TeacherId, 
         FirstName.x,LastName.x,StudentId)
final = df_1q9 %>% 
  group_by(DepartmentID,DepartmentName,CourseId,
           CourseName,TeacherId,FirstName.x,LastName.x)%>%
summarise(num_stu=n_distinct(StudentId))
final

###q10


df_1q10 <- left_join (df_students, df_classrooms, by=c("StudentId"="StudentId"))
df_1q10 <- left_join(df_1q10, df_courses, by="CourseId" )
df_avg_dep1 <- df_1q10 %>% filter(DepartmentID==1)%>% 
                group_by(StudentId) %>% summarise(Eng_tziyun=mean(degree,na.rm = TRUE))
df_avg_dep2 <- df_1q10 %>% filter(DepartmentID==2)%>% 
  group_by(StudentId) %>% summarise(sci_tziyun=mean(degree,na.rm = TRUE))
df_avg_dep3 <-df_1q10 %>% filter(DepartmentID==3)%>% 
  group_by(StudentId) %>% summarise(arts_tziyun=mean(degree,na.rm = TRUE)) 
df_avg_dep4 <-df_1q10 %>% filter(DepartmentID==4)%>% 
  group_by(StudentId) %>% summarise(sport_tziyun=mean(degree,na.rm = TRUE))
df_memutza_klali <-df_1q10 %>% group_by(StudentId) %>% 
  summarise(memutza_klali=mean(degree, na.rm = TRUE))

final_1q10 <-df_1q10 %>%group_by(StudentId,FirstName,LastName)%>%
 summarise(sah_course=mean(n_distinct(CourseId, na.rm = TRUE)))

final_1q10 <-left_join(final_1q10,df_avg_dep1,by="StudentId")
final_1q10 <-left_join(final_1q10,df_avg_dep2,by="StudentId")
final_1q10 <-left_join(final_1q10,df_avg_dep3,by="StudentId")
final_1q10 <-left_join(final_1q10,df_avg_dep4,by="StudentId")
final_1q10 <-left_join(final_1q10,df_memutza_klali,by="StudentId")


