# Library data
library(readxl)
library(dplyr)

# Load Data
mydata= read_excel("sampledata.xlsx")

# Ex1: Selecting Random N rows
sample_n(mydata, 3)

#Ex2: Selecting Random Fraction of Rows
sample_frac(mydata, 0.1)

#Ex3: Remove duplicate rows based on all variables
x1= distinct(mydata)

#Ex4: Remove duplicate rows based on a variable
x2= distinct(mydata, Index,.keep_all = TRUE)

#Ex5: Remove duplicate rows based on multiple variables
x2= distinct(mydata, Index, Y2010,.keep_all = TRUE)

#Ex6: Selecting Variables
mydata2= select(mydata, Index, State:Y2008)

#Ex7: Dropping Variables
mydata= select(mydata, -Index, -State)

#EX8: Selecting or dropping variables starts with "Y"
mydata3= select(mydata, starts_with("Y"))
mydata33= select(mydata, - starts_with("Y"))

#Ex9: Selecting variables contain "I" in their name
mydata4= select(mydata, contains("I"))

#EX10: Reorder variables
mydata5= select(mydata,State, everything())

#Ex11: Rename Variables
mydata6= rename(mydata, Index1= Index)

#Ex12: Filter rows
mydata7= filter(mydata, Index=="A")
mydata7

#Ex13: Multiple selection criterial
mydata7= filter(mydata6, Index1 %in% c("A","C"))

#Ex14: AND condition in selection criteria
mydata8= filter(mydata6, Index1 %in% c("A","C") & Y2002 >= 1300000)

#Ex15: OR condition in selection criteria
mydata9= filter(mydata6, Index1 %in% c("A","C") | Y2002 >= 1300000)

#Ex16: NOT condition
mydata10= filter(mydata6, !Index1 %in% c("A","c"))

#Ex17: CONTAINS condition
mydata10= filter(mydata6, grepl("Ar", State))

#Ex18: Summarize selected veriables
summarise(mydata, Y2015_mean= mean(Y2015), Y2015_med= median(Y2015))

#Ex19: Summarize Multiple variables
summarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median))

#Ex20: summarize with Custom Functions
summarise_at(mydata, vars(Y2011, Y2012), funs(n(), missing= sum(is.na(.)), mean(.,na.rm=TRUE)))

#Ex21: Summarize all numeric variables
summarise_if(mydata, is.numeric, funs(n(), mean, median))

numdata= mydata[sapply(mydata, is.numeric)]
summarise_all(numdata, funs(n(), mean, median))
 
#Ex22: Summarize factor variable
summarise_all(mydata["Index"], funs(nlevels(.), nmiss= sum(is.na(.))))

#Ex23: sort data by multiple var
arrange(mydata, Index, Y2011)
arrange(mydata, desc(Index), Y2011)

#Ex24: Summarize data by categorical var
t= summarise_at(group_by(mydata, Index), vars(Y2011,Y2012), funs(n(), mean(., na.rm=TRUE)))

t= mydata %>%
  group_by(Index)%>%
  summarize_at(vars(Y2011:Y2015), funs(n(), mean(., na.rm = TRUE)))

#Ex25: Filter data within a categorical var
t= mydata %>%
  filter(Index %in% c("A","C","I"))%>%
           group_by(Index)%>%
           do(head(.,2))

t
#Ex26 Select 3rd max value
t= mydata%>%
  select(Index, Y2015)%>%
  filter(Index %in% c("A","C","I")) %>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2015)))%>%
  slice(3)
t
#EX27: summarize, group, sort together
t=mydata %>%
  group_by(Index) %>%
  summarise(Mean_2014= mean(Y2014, na.rm= TRUE),
            Mean_2015= mean(Y2015, na.rm= TRUE))%>%
  arrange(desc(Mean_2015))
t

#Ex 28: Create a new var
mydata1= mutate(mydata, change= Y2015/Y2014)

#Ex29: multiply all the vars by 100
mydata11= mutate_all(mydata, funs("new"=.*1000))
mydata11

#Ex30: calculate rank for var
mydata12= mutate_all(mydata3,c(Y2008, Y2009, Y2010),funs(rank= min_rank(.)))

#Ex 31:
out= mydata %>%
  group_by(Index)%>%
  filter(min_rank(desc(Y2015))==1) %>%
  select(Index, State, Y2015)

out
#Ex32:
out2= mydata %>%
  group_by(Index) %>%
  mutate(Total= cumsum(Y2015))%>%
  select(Index, Y2015, Total)
out2

#Ex33: common rows
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

df3= inner_join(df1, df2, by="ID")
df3
#Ex34: left join
left_join(df1, df2, by= "ID")

#Ex35:
mtcars$model= row.names(mtcars)
first= mtcars[1:20,]
second= mtcars[10:32,]
intersect(first, second)

#Ex36:
x=data.frame(ID = 1:6, ID1= 1:6)
y=data.frame(ID = 1:6,  ID1 = 1:6)
union(x,y)
union_all(x,y)

#Ex37:
setdiff(first, second)

#Ex38: if else
df <- c(-10,2, NA)
if_else(df < 0, "negative", "positive", missing = "missing value")

df =data.frame(x = c(1,5,6,NA))
df %>% mutate(newvar=if_else(x<5, x+1, x+2,0))

mydf =data.frame(x = c(1:5,NA))
mydf %>% mutate(newvar= if_else(is.na(x),"I am missing",
                                if_else(x==1,"I am one",
                                        if_else(x==2,"I am two",
                                                if_else(x==3,"I am three","Others")))))

#Ex39:
df = mydata %>%
  rowwise() %>% mutate(Max= max(Y2012,Y2013,Y2014,Y2015)) %>%
  select(Y2012:Y2015,Max)

df

#Ex40: 
df1=data.frame(ID = 1:6,  x=letters[1:6])
df2=data.frame(ID = 7:12, x=letters[7:12])
xy = rbind(df1,df2)
cbind(x,y)


#Ex41:
mydata %>% group_by(Index) %>%
  summarise(Pecentile_25=quantile(Y2015, probs=0.25),
            Pecentile_50=quantile(Y2015, probs=0.5),
            Pecentile_75=quantile(Y2015, probs=0.75),
            Pecentile_99=quantile(Y2015, probs=0.99))


x= data.frame(N= 1:10)
x = mutate(x, pos = ntile(x$N,5))
x

#Ex42:
length(unique(mtcars$cyl))
by_cyl <- group_by(mtcars, cyl)
models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
summarise(models, rsq = summary(mod)$r.squared)
models %>% do(data.frame(
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
)

#Ex43:
mydata2 = select_if(mydata, is.numeric)

#Ex44: 
summarise_if(mydata, is.factor, funs(nlevels(.)))

#Ex45: 
mydata11 = mutate_if(mydata, is.numeric, funs("new" = .* 1000))

#Ex46:
k <- c("a", "b", "", "d")
na_if(k, "")





