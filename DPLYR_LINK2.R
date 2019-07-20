library(dplyr)
mydata = read.csv("sampledata.csv")
mydata

# EX1: The sample_n function selects random rows from a data frame (or table). The second parameter of the function tells R the number of rows to select.
sample_n(mydata,3)

# EX2: The sample_frac function returns randomly N% of rows. In the example below, it returns randomly 10% of rows.(chon dong theo % tong so dong vd có 52 dong thì chon 10% là 5 dong)
sample_frac(mydata,0.1)

# EX3: The distinct function is used to eliminate duplicates. (dung de xoa dong bi trung dua tren tat ca cac cot)
X1 = distinct(mydata) #data nay k co dong bi trung

# EX4: The .keep_all function is used to retain all other variables in the output data frame.
X2 = distinct(mydata,Index,.keep_all = T) 
X2 #nghia la se xoa nhung cai bi trung theo Index va giu nguyen cac cot con lai, nen output chi co 1A 1C,...
head(X2)

# EX5: Remove Duplicates Rows based on multiple variables (Xoa theo nhieu cot)
X2 = distinct(mydata,Index,Y2010,.keep_all = T)
X2 #vi theo 2 dk thi Index trung nhung ma Y2010 thi k trung nen k bi xoa


# EX6: Selecting Variables (or Columns)
mydata2 = select(mydata,Index,State:Y2008)
head(mydata2)

# EX7:  Dropping Variables. The minus sign ( - ) before a variable tells R to drop the variable.
mydata1 = select(mydata, -Index, -State) #cach 1
head(mydata1)
mydata1 = select(mydata, -c(Index, State)) #cach 2

# EX8: Selecting or Dropping Variables starts with 'Y'
select(mydata,starts_with('Y')) #The starts_with() function is used to select variables starts with an alphabet.

## Adding a negative sign before starts_with() implies dropping the variables starts with 'Y'
mydata33 = select(mydata, -starts_with("Y"))
head(mydata33)

# Ex9: Selecting Variables contain 'I' in their names 
select(mydata,contains('S')) #nghia la chon theo ten cot, cot co chu s 

# EX10: Reorder Variables (sap xep lai cot)
mydata5 = select(mydata,State,everything()) #nghia la chon xep cot State trc roi tat ca cac cot con lai
head(mydata5)

# EX11: Rename Variables
mydata6 = rename(mydata,Index1 = Index) # rename(data , new_name = old_name)
head(mydata6)

# EX12: Filter Rows 
filter(mydata,Index == 'A')

# EX13: The %in% operator can be used to select multiple items. In the following program, we are telling R to select rows against 'A' and 'C' in column 'Index'.
filter(mydata,Index %in% c('A','C')) #filter 2 chu A va C trong cot Index

# EX14: 'AND' Condition in Selection Criteria
## we are picking data for 'A' and 'C' in the column 'Index' and income greater than 1.3 million in Year 2002.
filter(mydata,Index %in% c('A','C') & Y2002 >= 1300000) 


# EX15: 'OR' Condition in Selection Criteria
filter(mydata,Index %in% c('A','C') | Y2002 >= 1300000) #mot trong 2 dat dk la dc

# EX16: NOT Condition. The "!" sign is used to reverse the logical condition.(dao nguoc)
filter(mydata,!Index %in% c('A','C')) #khong lay A vs C 

# EX17: The grepl function is used to search for pattern matching. In the following code, we are looking for records wherein column state contains 'Ar' in their name.
filter(mydata,grepl('Ar',State))

# EX18: Summarize selected variables. we are calculating mean and median for the variable Y2015.
summarise(mydata,Y2015_mean = mean(Y2015),Y2015_med = median(Y2015)) #thuong dung chung vs group_by

# EX19: Summarize Multiple Variables
summarise_at(mydata,vars(Y2005,Y2006),funs(n(),mean,median)) #The summarise_at function allows us to select multiple variables by their names.

# EX20: Summarize with Custom Functions
##The dot (.) bieu thi cho moi bien dc chi dinh trong thong so thu 2 (trong ham summarise thi thong so t2 là vars) 
summarise_at(mydata, vars(Y2011, Y2012),
             funs(n(),missing = sum(is.na(.)),mean(.,na.rm = T),median(.,na.rm = T)))


set.seed(222)
mydata_2 <- data.frame(X1=sample(1:100,100), X2=runif(100)) #runif la chay ngau nhien tuy so quan sat minh dua vao(max=1,min=0)
summarise_at(mydata_2,vars(X1,X2), function(x) var(x - mean(x)))

# EX21: Summarize all Numeric Variables 
##The summarise_if function allows you to summarise conditionally.
summarise_if(mydata,is.numeric,funs(mean,median)) #neu mydata la so thi dung ham mean va median

# EX22: Summarize Factor Variable 
summarise_all(mydata['Index'], funs(nlevels(.), nmiss=sum(is.na(.)))) #xem Index co bn level va co bn NA
str(mydata)

#------arrange() function-------#
# The default sorting order of arrange() function is ascending
# To sort a variable in descending order, use desc(x). 
# EX23:  Sort Data by Multiple Variables
arrange(mydata,Index,Y2011)

## Suppose you need to sort one variable by descending order and other variable by ascending oder. 
arrange(mydata,desc(Index),Y2011)
arrange(mydata,Y2011)


# EX24: Summarise Data by Categorical Variable 
t = summarise_at(group_by(mydata, Index), vars(Y2011, Y2012), funs(n(), mean(., na.rm = TRUE)))
t

t = mydata %>%
  group_by(Index) %>%
  summarise_at(vars(Y2011,Y2012),funs(n(),mean(.)))
t

#--------do() function----------#
# Use : Compute within groups 
# Note : The dot (.) is required to refer to a data frame.
# EX25:  Filter Data within a Categorical Variable. Suppose you need to pull top 2 rows from 'A', 'C' and 'I' categories of variable Index. 
t1 = mydata %>% 
  filter(Index %in% c("A", "C","I")) %>% 
  group_by(Index) %>%
  do(head(., 2)) #ket qua giong nhu cu co dieu dc filter theo A C I
t1
?do #tinh toan bat ky 

# EX26: Selecting 3rd Maximum Value by Categorical Variable
## The slice() function is used to select rows by position. 
t2 = mydata %>% 
  select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2015))) %>%  #xep giam dan theo A C I
  slice(3) # chon dong theo index da dc filter
t2


#--------Window Functions---------#
# dplyr uses window functions that are used to subset data within a group. 
# It returns a vector of values. 
# We could use min_rank() function that calculates rank in the preceding example
t3 = mydata %>% 
  select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  filter(min_rank(desc(Y2015)) == 3)
t3

# EX27: Summarize, Group and Sort Together
t4 = mydata %>%
  group_by(Index)%>%
  summarise(Mean_2014 = mean(Y2014, na.rm=TRUE),
            Mean_2015 = mean(Y2015, na.rm=TRUE)) %>%
  arrange(desc(Mean_2015))
t4

# EX28: Create a new variable 
mydata4 = mutate(mydata, change=Y2015/Y2014)
mydata4

# EX29:  Multiply all the variables by 1000 
mydata11 = mutate_all(mydata,funs(new = .*1000))

# EX30: Calculate Rank for Variables
mydata12 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(.)))
# By default, min_rank() assigns 1 to the smallest value and high number to the largest value. 
# In case, you need to assign rank 1 to the largest value of a variable, use min_rank(desc(.))
# nghia la neu muon gan 1 cho so lon nhat thi se la min_rank(desc(.))
mydata13 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(desc(.))))

# EX31: Select State that generated highest income among the variable 'Index' 
output = mydata %>%
  group_by(Index) %>%
  filter(min_rank(desc(Y2015))==1) %>%
  select(Index,State,Y2015)

# EX32: The cumsum function calculates cumulative sum of a variable.(cumulative: luy tich)
# With mutate function, we insert a new variable called 'Total' which contains values of cumulative income of variable Index
out2 = mydata %>% 
  group_by(Index) %>% 
  mutate(Total=cumsum(Y2015)) %>% #cong don income 2015 theo Index A rieng C riêng.....
  select(Index, Y2015, Total)
  

#--------join() function--------#
# Use : Join two datasets 
# Syntax : 
  ## inner_join(x, y, by = )
  ## left_join(x, y, by = )
  ## right_join(x, y, by = )
  ## full_join(x, y, by = )
  ## semi_join(x, y, by = )
  ## anti_join(x, y, by = )
# x, y - datasets (or tables) to merge / join
# by - common variable (primary key) to join by.

# EX33: 
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

# INNER JOIN returns rows when there is a match in both tables. 
# In this example, we are merging df1 and df2 with ID as common variable (primary key). 
inner_join(df1,df2,by = 'ID')

# If the primary key does not have same name in both the tables, try the following way:
inner_join(df1,df2,by = c('ID'='ID1'))

# EX34: LEFT JOIN : It returns all rows from the left table, even if there are no matches in the right table
left_join(df1,df2) #lay bang ben left lam chuan r nhap bang ben right vao
# cai nao ben right k co thi se NA



#-------Combine Data Vertically--------# (combine theo chieu doc)

# intersect(x, y) Rows that appear in both x and y.

# union(x, y) Rows that appear in either or both x and y.

# setdiff(x, y) Rows that appear in x but not y.

# EX35: Applying INTERSECT
# Prepare Sample Data for Demonstration
mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]
# INTERSECT selects unique rows that are common to both the data frames.
out = intersect(first, second) #dong xuat hien trong ca x va y

# EX36: Applying UNION 
# UNION trinh bay tat ca cac dong cua 2 bang va xoa nhung dong bi trung. 
# By using union_all function, cho phep dong bi trung trong ket qua combine.
x=data.frame(ID = 1:6, ID1= 1:6)
y=data.frame(ID = 6:11,  ID1 = 1:6)
union(x,y)
union_all(x,y)

# EX37: Rows appear in one table but not in other table
setdiff(first,second) #nhung dong chi xuat hien o 1 ban, ban con lai k co

# EX38: IF ELSE Statement
# if_else(condition, true, false, missing = NULL)
df <- c(10,-2, NA)
if_else(df < 0, "negative", "positive", missing = "missing value")

# Create a new variable with IF_ELSE
# If a value is less than 5, add it to 1 and if it is greater than or equal to 5, add it to 2. Otherwise 0.
df =data.frame(x = c(1,5,6,NA))
df %>% 
  mutate(newvar=if_else(x<5, x+1, x+2,0))


#-----Nested IF ELSE-------# (nhieu IF ELSE)
mydf =data.frame(a = c(1:5,NA))
mydf %>% 
  mutate(newvar= if_else(is.na(a),"I am missing",
                         if_else(a==1,"I am one",
                                 if_else(a==2,"I am two",
                                 if_else(a==3,"I am three","Others")))))

#------SQL-Style CASE WHEN Statement-------#
#  case_when() function to write nested if-else queries.
# In case_when(), you can use variables directly within case_when() wrapper. TRUE refers to ELSE statement. 
mydf %>% 
  mutate(flag = case_when(is.na(a) ~ "I am missing",
                                 a == 1 ~ "I am one",
                                 a == 2 ~ "I am two",
                                 a == 3 ~ "I am three",
                                 TRUE ~ "Others"))
# quan trong f de is.na() la dieu kien dau cuae nest if_else


# EX 39:  Apply ROW WISE Operation  
# Suppose you want to find maximum value in each row of variables 2012, 2013, 2014, 2015. 
# The rowwise() function allows you to apply functions to rows.
df = mydata %>%
  rowwise() %>% #nghia la tim max cua tung dong, con bt la max cua cot
  mutate(Max= max(Y2012,Y2013,Y2014,Y2015)) %>%
  select(Y2012:Y2015,Max)
str(df)

# Ex40: Combine Data Frames 
df1=data.frame(ID = 1:6,  x=letters[1:6])
df2=data.frame(ID = 7:12, x=letters[7:12])

# The bind_rows() function combine two datasets with rows.
bind_rows(df1,df2) #nghia la noi dong xuong duoi
## or rbind (base)
rbind(df1,df2)

# The bind_cols() function combine two datasets with columns.
bind_cols(df1,df2) #ghep thanh 4 cot
## or cbind (base)
cbind(df1,df2)

# EX41: The quantile() function is used to determine Nth percentile value. 
#In this example, we are computing percentile values by variable Index. 
mydata5 = mydata %>% 
  group_by(Index) %>%
  summarise(Pecentile_25=quantile(Y2015, probs=0.25),
            Pecentile_50=quantile(Y2015, probs=0.5),
            Pecentile_75=quantile(Y2015, probs=0.75),
            Pecentile_99=quantile(Y2015, probs=0.99))

# The ntile() function is used to divide the data into N bins. 
x = data.frame(N= 1:10)
x = mutate(x, pos = ntile(x$N,5)) #chia thanh 5 bins


# EX42:  In this example, we are building linear regression model for each level of a categorical variable. 
# There are 3 levels in variable cyl of dataset mtcars.
str(mtcars)
mtcars = as.factor(mtcars$cyl)
length(unique(mtcars$cyl))
by_cyl <- group_by(mtcars, cyl)
models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
summarise(models, rsq = summary(mod)$r.squared)
models %>% do(data.frame(
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
)



#---------------if() Family of Functions------------#
# The select_if() function returns only those columns where logical condition is TRUE. 
# The is.numeric refers to retain only numeric variables
# EX43:
mydata22 = select_if(mydata, is.numeric) #select chon cot nao la numeric
mydata3 = select_if(mydata, is.factor) #chon cot nao la factor

# EX44: Like select_if() function, summarise_if() function lets you to summarise only for variables where logical condition holds. 
summarise_if(mydata, is.factor, funs(nlevels(.)))

# EX45: Multiply by 1000 to numeric variables
mydata11 = mutate_if(mydata, is.numeric, funs("new" = .* 1000))

# EX46: Convert value to NA. using na_if()
k <- c("a", "b", "", "d")
na_if(k, "") #nghia la neu trong k co " " thi no la NA



