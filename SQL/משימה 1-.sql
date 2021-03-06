
 ---2
 ---
  --a--a.	מנהל המכללה ביקש לדעת כמה סטודנטים יש לפי יחידה (מחלקה).
SELECT departments.DepartmentName, COUNT(students.studentID) as mispar_stu
FROM dbo.students
INNER JOIN dbo.classrooms ON students.StudentID = classrooms.StudentID
INNER JOIN dbo.courses ON classrooms.CourseID = courses.CourseID
INNER JOIN dbo.departments ON courses.DepartmentID = departments.DepartmentID
GROUP BY departments.DepartmentName

  ---
  ---b- כמה סטודנטים יש ללפי כל קורס שהמורה לאנגלית מעביר וסה"כ התלמידים בכל הקורסים שלו
  ---c-	המרכז למדעים רוצה להבין כמה כיתות קטנות (מתחת ל-22) וכמה גדולות צריך עבור קורסים ביחידה (מחלקה) שלו.

  select count( [StudentId]) As Count_Student,       
   	  c.[CourseName]from [dbo].[Courses] c 
	inner join [dbo].[Departments] d on c.[DepartmentID] = d.[DepartmentId]
	inner join [dbo].[Classrooms] cl on cl.[CourseId] = c.[CourseId]Where [DepartmentName] = 'English'
	group by c.[CourseName]
	 union all
	select count( [StudentId]) As Count_Student,'Sum Student'
	from [dbo].[Courses] c 
	inner join [dbo].[Departments] d on c.[DepartmentID] = d.[DepartmentId]
	inner join [dbo].[Classrooms] cl on cl.[CourseId] = c.[CourseId]
	Where [DepartmentName] = 'English'
	 select count(ClassType) ClassNumber,ClassType from(select count(cl.[StudentId]) as a1,       co.[CourseName],	  
	  case 	      WHEN count(cl.[StudentId]) >= 22 THEN 'Big Class'		    else 'Smal Class'    
	      end as ClassType from [dbo].[Classrooms] cl inner
	   join  [dbo].[Courses] co on cl.[CourseId] = co.[CourseId]where co.[DepartmentID] = 2
	   group by co.[CourseName]) A group by ClassType

---d 	סטודנטית שהיא פעילה פמיניסטית טוענת שהמכללה מעדיפה לקבל יותר גברים מאשר נשים. תבדקו האם הטענה מוצדקת (מבחינת כמותית, לא סטטיסטית).
	select gender, count (gender) as mis_stu
	from [dbo].[Students]
	 where gender is not null
	group by gender

	--יש יותר נשים מאשר גברים

--e.	באיזה קורסים אחוז הגברים / הנשים הינה מעל 70%?



		
		select Cours_Name,(count_studet*1.0/all_student*1.0)*100.0 As studet_percent
from
(
  select count(*) As count_studet,
         [CourseId] As CourseId,
	  (select [CourseName] 
          from [dbo].[Courses] 
          where [dbo].[Courses].[CourseId] = b.[CourseId]) As Cours_Name,
         gender As gender, 
         (select count(*) 
          from [dbo].[Classrooms] 
          where [dbo].[Classrooms].[CourseId] = b.[CourseId]) as all_student
  from [dbo].[Classrooms] B
  inner join [dbo].[Students] D 
  on B.[StudentId] = D.[StudentId]
  group by [CourseId],gender 
) S
Where (count_studet*1.0/all_student*1.0)*100.0 > 70.0
order by CourseId




--f.	בכל אחד מהיחידות (מחלקות), כמה סטודנטים (מספר ואחוזים) עברו עם ציון מעל 80?
select student_tziun_80,DepartmentId,
(select count (*) from [dbo].[Classrooms],[dbo].[Courses] where
[dbo].[Classrooms].courseid=[dbo].[Courses].courseid and DepartmentID=a.departmentid )as all_stu_dep,
(select Departmentname from [dbo].[departments] where DepartmentId=a.DepartmentId) as Dep_name
into tziun_over_80
from (
select count (*) As student_tziun_80,
c.DepartmentID As DepartmentID from [dbo].[Classrooms]  d
inner join [dbo].[Courses]  c on d.courseid=c.courseid
 where degree >80.0
 group by c.departmentid) a

 select dep_name,student_tziun_80, all_stu_dep,(student_tziun_80*1.0/all_stu_dep*1.0)*100.0
 as stu_percent from tziun_over_80




		  
--g.	בכל אחד מהיחידות (מחלקות), כמה סטודנטים (מספר ואחוזים) לא עברו (ציון מתחת ל-60) ?
select student_tziun_60,DepartmentId,
(select count (*) from [dbo].[Classrooms],[dbo].[Courses] where
[dbo].[Classrooms].courseid=[dbo].[Courses].courseid and DepartmentID=a.departmentid )as all_stu_dep,
(select Departmentname from [dbo].[departments] where DepartmentId=a.DepartmentId) as Dep_name
into tziun_under_60
from (
select count (*) As student_tziun_60,
c.DepartmentID As DepartmentID from [dbo].[Classrooms]  d
inner join [dbo].[Courses]  c on d.courseid=c.courseid
 where degree <60.0
 group by c.departmentid) a

 select dep_name,student_tziun_60, all_stu_dep,(student_tziun_60*1.0/all_stu_dep*1.0)*100.0
 as stu_percent from tziun_under_60

 


--h.	תדרגו את המורים לפי ממוצע הציון של הסטודנטים מהגבוהה לנמוך.

SELECT a.courseid, a.studentid,a.degree ,b.teacherid
into #avg
from [dbo].[Classrooms] as a
inner join [dbo].[Courses] as  b
on a.courseid=b.courseid


select  degree ,teacherid
,avg (degree) over (partition by teacherid) as avg_degree
from #avg
group by  degree ,teacherid
order by avg_degree desc

----VIEW---
a.	המראה את הקורסים, היחידות (מחלקות) עליהם משויכים, המרצה בכל קורס ומספר התלמידים רשומים בקורס

CREATE VIEW limudim_11 AS
select d.[DepartmentName],c. [CourseName],a.[FirstName],a.[LastName],
       (select count([StudentId]) from [dbo].[Classrooms] where [CourseId] = c.[CourseId] ) SudentCount
from [dbo].[Courses] c
inner join [dbo].[Departments] d
on c.[DepartmentID] = d.[DepartmentId]
inner join [dbo].[Teachers] a
on c.[TeacherId] = a.[TeacherId]


SELECT *
from limudim_11


----b. המראה את התלמידים, מס' הקורסים שהם לוקחים, הממוצע של הציונים לפי יחידה (מחלקה) והממוצע הכוללת שלהם.
CREATE VIEW limudim12_v AS
select StudentId,
       FirstName,
	LastName,
	DepartmentName,
	(select count(*) from [dbo].[Classrooms] where [StudentId] = A.[StudentId]) as NumberOfCourse ,
	AVG(degree)  as Avg_per_Department,
      (select AVG(isnull(degree,0.0)*1.0)  from [dbo].[Classrooms] 
	  where [StudentId] = A.StudentId group by [StudentId]) as Avg_all_Course
from
(select s.[StudentId] as StudentId,
       s.[FirstName] as FirstName,
	   s.[LastName] as LastName,
	   C.[CourseId] as CourseId,
	   C.[degree] as degree,
       (select [DepartmentName] from [dbo].[Courses] 
	   inner join  [dbo].[Departments] on [dbo].[Courses].[DepartmentID] = [dbo].[Departments].[DepartmentID] 
	   and [dbo].[Courses].[CourseId] = C.[CourseId]) as DepartmentName 
from [dbo].[Students] s
left outer join [dbo].[Classrooms] C
on s.[StudentId] = C.[StudentId]
) A
group by StudentId,FirstName,LastName,DepartmentName