#library
library(dplyr)
#Data : Income Data by States : thu nhap theo quoc gia 2002-2015
mydata=read.csv("sampledata.csv")
str(mydata)
head(mydata)
typeof(mydata)
#Example 1 : Selecting Random N Rows
#sample_n: chon hang ngau nhien tu khung du lieu, thamn  so thu 2 cho biet so luong can chon 
sample_n(mydata,3)
#Example 2 : Selecting Random Fraction of Rows
##sample_frac : tra ve ngau nhien n hang , o day la 10%
sample_frac(mydata,0.1)
#Example 3 : Remove Duplicate Rows based on all the variables (Complete Row): loai bo cac hang trung lap 
#distinct
x1 = distinct(mydata)# trong du lieu nay k co hang trung lap 
#Example 4 : Remove Duplicate Rows based on a variable: loai bo ca hang trung lap dua vao 1 bien 
## .keep_all: retain all other variables in the output data frame.: giu lai tat ca cac bien trong khung du lieu dau ra
x2 = distinct(mydata, Index, .keep_all= TRUE)# giu lai cac dong dau tien cua chu cais theo index, con lai trung bo het, neu de FALE no chi lay moi cot index ,bo cachubi trung
#Example 5 : Remove Duplicates Rows based on multiple variables: loai bo cac hang trung lap dua tren nhiu bien
## su dung 2 vien dinh tinh Index, Y2010
x2 = distinct(mydata, Index, Y2010, .keep_all= TRUE)# neu la FALSE no chi lay co index vs Y2010
##--------------------------select( ) Function----------------------------------: only desired variables.: su dung cho cac bien minh mun
#data: phai la data frame
#Example 6 : Selecting Variables (or Columns)
mydata2 = select(mydata, Index, State:Y2008)
#Example 7 : Dropping Variables: bo bien , banng viec them dau tru trc no minus sign
mydata = select(mydata, -Index, -State)# co the viet lai nhu sau
mydata = select(mydata, -c(Index,State))
#Example 8 : Selecting or Dropping Variables starts with 'Y'
## starts_with() function : chom cac bien batdau bang chu cai 
mydata3 = select(mydata, starts_with("Y"))
##Adding a negative sign before starts_with() implies dropping the variables starts with 'Y'
mydata33 = select(mydata, -starts_with("Y"))
#Example 9 : Selecting Variables contain 'I' in their names : chon  cac bien co chu I o trong
##contain
mydata4 = select(mydata, contains("I"))
#Example 10 : Reorder Variables: sap xep lai cac bien: theo bang chuw cai theo ten bien tren cung ,state moi toi index
mydata5 = select(mydata, State, everything())
#-------------------------------rename( ) Function: doi ten bien ----------------------------------------
##data= data frame
#Example 11 : Rename Variables
mydata6 = rename(mydata, Index1=Index)
#-------------------------------filter( ) Function: loc theo dieu kien--------------------------------------- 
#data: data frame
#Example 12 : Filter Rows : theo dong 
mydata7 = filter(mydata, Index == "A")
#Example 13 : Multiple Selection : chon A vs C trong cot Index1
mydata7 = filter(mydata6, Index1 %in% c("A", "C"))
#Example 14 : 'AND' Condition in Selection Criteria: dieu kien va tieu chi lua chon 
mydata8 = filter(mydata6, Index1 %in% c("A", "C") & Y2002 >= 1300000 )
#Example 15 : 'OR' Condition in Selection Criteria
mydata9 = filter(mydata6, Index1 %in% c("A", "C") | Y2002 >= 1300000)
#Example 16 : NOT Condition 
mydata10 = filter(mydata6, !Index1 %in% c("A", "C"))
#Example 17 : CONTAINS Condition: "grepl function"  tim kiem Ar trong ten cua cot state
mydata10 = filter(mydata6, grepl("Ar", State))
#-------------------------------------- summarise( ) Function---------------------------------------------
#Example 18 : Summarize selected variables
summarise(mydata, Y2015_mean = mean(Y2015), Y2015_med=median(Y2015))
#Example 19 : Summarize Multiple Variables
summarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median))
#Example 20 : Summarize with Custom Functions
summarise_at(mydata, vars(Y2011, Y2012),
             funs(n(), missing = sum(is.na(.)), mean(., na.rm = TRUE), median(.,na.rm = TRUE)))
#Example 21 : Summarize all Numeric Variables: tom tat co dieu kien
summarise_if(mydata, is.numeric, funs(n(),mean,median))
##Alternative Method : pp thay the
##First, store data for all the numeric variables : dau tien luu tru du lieu cho cac bien so
numdata = mydata[sapply(mydata,is.numeric)]
##Second, the summarise_all function calculates summary statistics for all the columns in a data frame : tinh toan thong ke cac cot khung du lieu 
summarise_all(numdata, funs(n(),mean,median))
#Example 22 : Summarize Factor Variable 
##We are checking the number of levels/categories and count of missing observations in a categorical (factor) variable. 
summarise_all(mydata["Index"], funs(nlevels(.), nmiss=sum(is.na(.))))
#-------------------------------------------arrange() function :sort data------------------------------
##To sort a variable in descending order, use desc(x). sap xep theo thu tu giam dan
#Example 23 : Sort Data by Multiple Variables : sap xeptheo nheu bien tang dan 
arrange(mydata, Index, Y2011)
#Suppose you need to sort one variable by descending order and other variable by ascending oder.  giam dan theo order
arrange(mydata, desc(Index), Y2011)
#--------------------------------------Pipe Operator %>%------------------------------------------
#-----------------------------------------group_by() function :--------------------------------------
#Example 24 : Summarise Data by Categorical Variable 
t = summarise_at(group_by(mydata, Index), vars(Y2011, Y2012), funs(n(), mean(., na.rm = TRUE))) #OR
t = mydata %>% group_by(Index) %>%
  summarise_at(vars(Y2011:Y2015), funs(n(), mean(., na.rm = TRUE)))
#------------------------------------Compute within groups-------------------------------------------------
#Example 25 : Filter Data within a Categorical Variable : loc du lieu bien phan loai

t = mydata %>% 
  filter(Index %in% c("A", "C","I")) %>% 
  group_by(Index) %>%
  do(head( . , 2))
#Example 26 : Selecting 3rd Maximum Value by Categorical Variable: con gia tri toi da theo bien phan loai
t = mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2015))) %>%  slice(3)
##slice() function is used to select rows by position. 
## use min_rank() function that calculates rank
t = mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  filter(min_rank(desc(Y2015)) == 3)
#Example 27 : Summarize, Group and Sort Together  
t = mydata %>%
  group_by(Index)%>%
  summarise(Mean_2014 = mean(Y2014, na.rm=TRUE),
            Mean_2015 = mean(Y2015, na.rm=TRUE)) %>%
  arrange(desc(Mean_2015))
#-------------------------------------mutate() function :-----------------------------------
#Example 28 : Create a new variable 
mydata1 = mutate(mydata, change=Y2015/Y2014)
#Example 29 : Multiply all the variables by 1000 
mydata11 = mutate_all(mydata, funs("new" = .* 1000))
#Example 30 : Calculate Rank for Variables
mydata12 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(.)))
##min_rank() assigns 1 to the smallest value and high number to the largest value. In case, you need to assign rank 1 to the largest value of a variable, use min_rank(desc(.))
mydata13 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(desc(.))))
#Example 31 : Select State that generated highest income among the variable 'Index'
out = mydata %>% group_by(Index) %>% filter(min_rank(desc(Y2015)) == 1) %>%
  select(Index, State, Y2015)
#Example 32 : Cumulative Income of 'Index' variable 
out2 = mydata %>% group_by(Index) %>% mutate(Total=cumsum(Y2015)) %>%
  select(Index, Y2015, Total)
#------------------------------join() function :-------------------------------------------------
#Example 33 : Common rows in both the tables
df1 = data.frame(ID = c(1, 2, 3, 4, 5),
                 w = c('a', 'b', 'c', 'd', 'e'),
                 x = c(1, 1, 0, 0, 1),
                 y=rnorm(5),
                 z=letters[1:5])

df2 = data.frame(ID = c(1, 7, 3, 6, 8),
                 a = c('z', 'b', 'k', 'd', 'l'),
                 b = c(1, 2, 3, 0, 4),
                 c =rnorm(5),
                 d =letters[2:6])
df3 = inner_join(df1, df2, by = "ID")
##If the primary key does not have same name in both the tables, try the following way: neu tu khoa chinh k co
inner_join(df1, df2, by = c("ID"="ID1"))
#Example 34 : Applying LEFT JOIN : giu nguyen cot trtai, cot phai trung cai nao ghep vo
left_join(df1, df2, by = "ID")
#-------------------------------Combine Data Vertically-----------------------------------------
#Example 35 : Applying INTERSECT
mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]
intersect(first, second)
#Example 36 : Applying UNION : cho hang trung lap ghep lai, xoa di
x=data.frame(ID = 1:6, ID1= 1:6)
y=data.frame(ID = 1:6,  ID1 = 1:6)
union(x,y)
union_all(x,y)
#Example 37 : Rows appear in one table but not in other table 
setdiff(first, second)
#Example 38 : IF ELSE Statement
df <- c(-10,2, NA)
if_else(df < 0, "negative", "positive", missing = "missing value")
##Create a new variable with IF_ELSE
df =data.frame(x = c(1,5,6,NA))
df %>% mutate(newvar=if_else(x<5, x+1, x+2,0))
##Nested IF ELSE
mydf =data.frame(x = c(1:5,NA))
mydf %>% mutate(newvar= if_else(is.na(x),"I am missing",
                                if_else(x==1,"I am one",
                                        if_else(x==2,"I am two",
                                                if_else(x==3,"I am three","Others")))))
##SQL-Style CASE WHEN Statement
mydf %>% mutate(flag = case_when(is.na(x) ~ "I am missing",
                                 x == 1 ~ "I am one",
                                 x == 2 ~ "I am two",
                                 x == 3 ~ "I am three",
                                 TRUE ~ "Others"))
#Example 39 :  Apply ROW WISE Operation  
df = mydata %>%
  rowwise() %>% mutate(Max= max(Y2012,Y2013,Y2014,Y2015)) %>%
  select(Y2012:Y2015,Max)
#Example  40 : Combine Data Frames
df1=data.frame(ID = 1:6,  x=letters[1:6])
df2=data.frame(ID = 7:12, x=letters[7:12])
xy = bind_rows(df1,df2)
xy = rbind(df1,df2)
xy = bind_cols(x,y)#OR
xy = cbind(x,y)
#Example 41 : Calculate Percentile Values
##The quantile() function is used to determine Nth percentile value. In this example, we are computing percentile values by variable Index
mydata %>% group_by(Index) %>%
  summarise(Pecentile_25=quantile(Y2015, probs=0.25),
            Pecentile_50=quantile(Y2015, probs=0.5),
            Pecentile_75=quantile(Y2015, probs=0.75),
            Pecentile_99=quantile(Y2015, probs=0.99))
##The ntile() function is used to divide the data into N bins. 
x= data.frame(N= 1:10)
x = mutate(x, pos = ntile(x$N,5))
#Example 42 : Automate Model Building# xay dung mo hinh tu dong tung cap do
by_cyl <- group_by(mtcars, cyl)# mat ten xe
models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
summarise(models, rsq = summary(mod)$r.squared)
models %>% do(data.frame(
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
)
#--------------------------------if() Family of Functions-----------------------
#Example 43 : Select only numeric columns 
##select_if(): tra ve cac cot , giu lai cac bien so 
mydata2 = select_if(mydata, is.numeric) #OR
mydata3 = select_if(mydata, is.factor) # giu lai cac chu
#Example 44 : Number of levels in factor variables 
summarise_if(mydata, is.factor, funs(nlevels(.)))
#Example 45 : Multiply by 1000 to numeric variables # nhaan 1000 voi cac bien so 
mydata11 = mutate_if(mydata, is.numeric, funs("new" = .* 1000))
#Example 46 : Convert value to NA: thaycho " " thanh NA
k <- c("a", "b", "", "d")
na_if(k, "")




