library(dplyr)
library(readxl)
data=read.csv("sampledata.csv")
#Ex1: Select random n rows
sample_n(data,4)
#chon ngau nhien 4 h?ng va all cot
#EX2: Select row theo ty le
sample_frac(data,0.9)
#EX3: Xoa cac hang trung lap
x1=distinct(data)
#xoa cac hang trung lap neu khong co n se de nguyen du lieu
#EX4:Giu tat ca cac gia tri khac nhau trong cac cot 
?.keep_all
x2=distinct(data,Index, .keep_all = TRUE)
#EX5: Remove Duplicates Rows based on multiple variables
#xoa hang trung lap dua tren nhieu bien
distinct(data, Index, Y2010, .keep_all= TRUE)
# y cua no la cot 2010 co hang trung thi n se xoa hang thu hai giu 
#hang thu nhat
#EX6: 
select(data,State:Y2008)
#EX7: Bo cot
select(data,-State,-Y2008)
#hoac
select(data,-c("State","Y2008"))
#EX8: chon hoac bo cot theo bien bat dau tu cai gi do
select(data,starts_with("Y"))
select(data,-starts_with("Y"))
#EX9: chon ten co bien I trong ten
select(data,contains("I"))
#EX10: Sap xep cac bien
select(data, State, everything())
#sap xep cac bien lai theo State la bat dau 
#EX11: rename variables
rename(data, "Index1"="Index")
#EX12: Filter
filter(data,Index=="A")
#EX13: filter theo index
#%in% index nay co chua A va C se khac index =A,C
filter(data, Index %in% c("A", "C"))
filter(data, Index == c("A", "C"))
data %>%
  filter(Index %in% c("A","C"))
#data %>%
#filter(Index %in% c("A","C")) cach nay khong dc dung cach tren
#EX14: And filter voi nhieu muc
filter(data, Index %in% c("A", "C") & Y2002 >= 1300000 )
#EX15: vua filter cai nay hoac cai kia
filter(data, Index %in% c("A", "C") | Y2002 >= 1300000)
#EX16: filter cai nay khac cai kia
filter(data, !Index %in% c("A", "C"))


#filter khac A, C
#EX:17
filter(data, grepl("Ar", State))
# chon data voi chuc nang grepl Satate ten bat dau tu Ar
#EX18: Summarize selected variables
data%>%
  summarise(meanY2015=mean(Y2015), medianY2015=median(Y2015))
#EX19:su dung funs de co nhieu chuc nang
data%>%
  summarise_at(vars(Y2005,Y2006),funs(n(),mean,median))
#EX20:  Summarize with Custom Functions
summarise_at(data, vars(Y2011, Y2012),
             funs(n(), missing = sum(is.na(.)),
                  mean(., na.rm = TRUE), median(.,na.rm = TRUE)))
data%>%
  summarise_at(vars(Y2011, Y2012),funs(
    n(),mis=sum(is.na(x)),mean,median))
#EX21: Summarize all Numeric Variables 
#ham summise_if tom tat dieu kien
summarise_if(data,is.numeric, funs(n(),mean,median))
#is.numneric la loai du lieu ma summarise dang tinh
#data famre thi la double
summarise_if(data,is.numeric, funs(n(),mean,median))
#cach viet tu tu ra
data[sapply(data,is.numeric)]
summarise_all(numdata, funs(n(),mean,median))
#EX22:  Summarize Factor Variable
summarise_all(data["Index"],
              funs(nlevels(.), nmiss=sum(is.na(.))))
#cach kiem missing values
#arrange(data_frame, variable(s)_to_sort)
#or
#data_frame %>% arrange(variable(s)_to_sort)
#cach de sap xep dara frame
#EX23: arrange du lieu theo nhieu bien
arrange(data,desc(Index),Y2010)
arrange(data, desc(Index), Y2011)
arrange(data,Index,Y2011)
#cai nay ra 2 cai tren khong ra
data%>%
  arrange(Y2011)
data%>%
  arrange(desc(Y2011))
#EX24: We are calculating count and
#mean of variables Y2011 and Y2012 by variable Index. 
data%>%
  group_by(Index)%>%
  summarise_at(vars(Y2011:Y2012),funs(n(),mean,median))
#EX25: 
data %>% 
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  do(a=head(., 2))#chi chon head 2 cua moi bien
#dung do()like: filtering for rows, selecting specific columns,
#re-ordering rows, adding new columns,
#summarizing data and computing arbitrary operations.
#co the dat ten hoac khong
#EX26: 
data %>%
  select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2015))) %>% 
  slice(3)
#sap xep slice la chon gia tri thu 3 cua moi cai giong nhau
#chon hang theo vi tri
data %>% 
  select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  filter(min_rank(desc(Y2015)) == 3)
#neu khong su dung slice thi su dung min_rank la chon hang cua moi bien
#EX27: 
data%>%
  group_by(Index)%>%
  summarise_at(vars(Y2014,Y2015),funs(mean))%>%
  arrange(desc(Y2015))

data %>%
  group_by(Index)%>%
  summarise(Mean_2014 = mean(Y2014, na.rm=TRUE),
            Mean_2015 = mean(Y2015, na.rm=TRUE)) %>%
  arrange(desc(Mean_2015))
#EX28:Create a new variable 
mutate(data, change=Y2015/Y2014)
data%>%
  mutate(change=Y2015/Y2014)
#EX29: 
mutate_all(data, funs("new" = .* 1000))
data%>%
  mutate_all(funs("new"=.*1000))
#mutate het tat ca thi dung all
#nhan tat ca cac bien voi 1000 va chen new
#EX30: Calculate Rank for Variables
mutate_at(data, vars(Y2008:Y2010), funs(Rank=min_rank(.)))
data%>%
 mutate_at(vars(Y2008:Y2010),funs(Rank=min_rank(.)))
