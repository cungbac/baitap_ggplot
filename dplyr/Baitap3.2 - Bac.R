
#Library
library(dplyr)

#Load data
mydata = read.csv('sampledata.csv')
mydata
str(mydata)

#Exp 1 - Selecting random n rows
sample_n(mydata,10)

#Exp 2 - Selecting random fraction of rows (fraction = phan so/ty le)
sample_frac(mydata, 0.1)

#Exp3 - Remove duplicated rows based on all variables (complete row)
x1 = distinct(mydata)

#Exp4 - Remove duplicated rows based on variables
x2 = distinct(mydata,Index,.keep_all = TRUE)
## .keep_all = TRUE: giu lai cac variables khac, lay dong dau tien cua variable duoc chon

#Exp5 - Remove duplicated rows based on multiple variables
x2 = distinct(mydata,Index,Y2010,.keep_all = T)

#Exp6 - Select variables (columns)
mydata2 = select(mydata,Index,State:Y2008)

#Exp7 - Dropping variables - loai tru variables
mydata2 = select(mydata,-Index,-State)

#Exp8 - Selecting or Dropping variables starts with "..."
mydata3 = select(mydata,starts_with("Y"))
mydata3 = select(mydata,-starts_with("Y"))
mydata3 = select(mydata,contains("200"))
mydata3 = select(mydata,ends_with("x"))
mydata3 = select(mydata,everything())
mydata3 = select(mydata,one_of(c("Y2002","Y2004")))


#Exp9 - Selecting Variables contain 'I' in their names 
mydata4 = select(mydata,contains("i"))

#Exp10 - Reorder variables 
mydata5 = select(mydata,State,everything())

#Exp11 - Rename variables
mydata6 = rename(mydata,NewIndex = Index)

#Exp12 - Filter rows
mydata7 = filter(mydata,Index == "A")

#Exp13- Multiple selection criteria %in%- filter nhieu dieu kien
mydata8 = filter(mydata,Index %in% c("A","C"))

#Exp14 - AND condition in slection criteria
mydata9 = filter(mydata,Index %in% c("A","C")&Y2002 >= 1300000)

#Exp15 - OR condition in slection criteria
mydata10 = filter(mydata,Index %in% c("A","C")|Y2002 >= 1300000)

#Exp16 - NOT condition
mydata11 = filter(mydata,!Index %in% c("A","C"))

#Exp17 - Contains condition
mydata12 = filter(mydata,grepl("Ar",State))

#Exp18 - Summarize selected variables
summarise(mydata,Y2015_mean = mean(Y2015),Y2015_med = median(Y2015))

#Exp19 - Summarise multiple variables - summarise at
summarise_at(mydata,vars(c(Y2005:Y2007)),funs(n(),mean,median))

#Exp20 - Summarise with custom functions
summarise_at(mydata,vars(Y2011,Y2012),funs(n(),missing = sum(is.na(.)),mean(.,na.rm = T),median(.,na.rm = T)))

summarise_at(mydata,vars(Y2011,Y2012),funs(n(),missing = sum(is.na(.)),mean,median))

#Exp21 - summarise all numeric variables
summarise_if(mydata,is.numeric,funs(n(),mean,median))

numdata = mydata[sapply(mydata,is.numeric)]
summarise_all(numdata,funs(n(),mean,median))

#Exp22- summarise factor variables
summarise_all(mydata["Index"],funs(nlevels(.),nmiss = sum(is.na(.))))

summarise_all(mydata["Index"],funs(nlevels,sum(is.na(.))))

#Exp23 - sort data by multiple variable
arrange(mydata,Y2002,Y2011)
arrange(mydata,desc(Index),Y2002)

#Exp24 - summarise data by categorical variable
summarise_at(group_by(mydata,Index),vars(Y2011,Y2012),funs(n(),mean(.,na.rm = T)))

mydata %>%
  group_by(Index)%>%
  summarise_at(vars(Y2011,Y2012),funs(n(),mean(.,na.rm = T)))

#Exp25 - Filter Data within a Categorical Variable
mydata %>%
  filter(Index %in% c("A","C", "I"))%>%
  group_by(Index) %>%
  do(head(.,10))

mydata %>%
  filter(Index %in% c("A","C", "I"))%>%
  group_by(Index) %>%
  do(head(.,2))

mydata %>%
  filter(Index %in% c("A","C", "I"))%>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2002)))

mydata %>%
  filter(Index %in% c("A","C", "I"))%>%
  group_by(Index) %>%
  arrange(desc(Y2002))

#Exp26 - Selecting 3rd Maximum Value by Categorical Variable
mydata %>%
  filter(Index %in% c("A","C","I")) %>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2015))) %>%
  slice(3)

mydata %>%
  filter(Index %in% c("A","C","I")) %>%
  group_by(Index) %>%
  arrange(desc(Y2015)) %>%
  slice(3)

mydata %>% 
  select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  filter(min_rank(desc(Y2015)) == 3)

#Exp27 - Summarize, Group and Sort Together

mydata %>%
  group_by(Index) %>%
  summarise(Mean_2014 = mean(Y2014,na.rm = T),
            Mean_2015 = mean(Y2015,na.rm = T)) %>%
  arrange(desc(Mean_2015))

#Exp28 - Create a new variable
mydata1 = mutate(mydata,change = Y2015/Y2014)

#Exp29 - Multiply all the variables by 1000 
mydata11 = mutate_all(mydata,funs("new" = .*1000))

#Exp30 - Calculate Rank for Variables
mydata12 = mutate_at(mydata,vars(Y2008:Y2010),funs(Rank = min_rank(desc(.))))

mydata12 = mutate_at(mydata,vars(Y2008:Y2010),funs(Rank = min_rank(.)))

#Exp31 - Select State that generated highest income among the variable 'Index' 

mydata %>%
  group_by(Index) %>%
  select(Index,State,Y2015) %>%
  mutate_at(vars(Y2015),funs(Rank = min_rank(desc(.)))) %>%
  filter(Rank == 1)

mydata %>% 
  group_by(Index) %>% 
  filter(min_rank(desc(Y2015)) == 1) %>%
  select(Index, State, Y2015)









                     
                     