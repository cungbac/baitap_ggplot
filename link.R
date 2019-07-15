# Import libraries
library(dplyr)
library(readr)
library(nycflights13)

# Import data
iris = iris
mydata = read.csv('sampledata.csv')
flights = flights
# Data Manipulation 

## Manipulate Cases: 

### sample 

sample_n(mydata,3) # lấy ngẫu nhiên n dòng
sample_frac(mydata, 0.2) # lấy ngẫu nhiên theo tỷ lệ 

### Distinct function 

distinct(iris) # xóa các dòng có giá trị trùng nhau
str(iris)

### Slice the same select: 

slice(mydata,1:3)
slice_select = mydata %>%
  select(Index, Y2015) %>%
  filter(Index %in% c("A", "C", "I")) %>%
  group_by(Index) %>%
  do(arrange(., desc(Y2015))) %>%
  slice(3)

### Filter %in%, !, "|" or, grepl

ft = filter(mydata, Index == "I")
ft1 = filter(mydata, Index %in% c("A", "C") & Y2002 >=1600000)
ft2 = filter(mydata, !Index %in% c("A", "C") ) # ! nghịch đảo logic
ft4 = filter(mydata, Index %in% c("A", "C")|Y2002 >= 1600000)
ft3 = filter(mydata, grepl("Ar", State)) # grepl : used to search pattern matching

### Arrange 

mydata %>%
  arrange(Index, Y2004)
mydata %>% 
  arrange(desc(Index), Y2004) # desc: sắp xếp các dòng từ lớn đến nhỏ

# Manipulate Variaples

## Extract Variaples

### select 

sel = select(mydata, Index, State:Y2008)

select(flights, year:day) # : có nghĩa là từ year đến day

# --- started_with ()
select(iris, starts_with("Sepal")) # Chọn các biến mà tên biến đó có chứa ký tự

# --- Contain ()
select(iris, contains(".")) #Chọn các biến mà tên biến đó có chứa ký tự

# --- ends_with()
select(iris, ends_with("Length")) # Chọn các biến mà tên biến đó kết thúc với một chuỗi ký tự

# --- Everything()
select(iris, everything()) # chọn tất cả các biến

# --- matches()

select(iris, matches(".t.")) # Chọn biến mà tên biến đó khớp với điền kiện cho trước

# --- num_range
select(iris, num_range("x", 1:5)) # Chọn các biên tên x1, x2, x3, x4, x5

# --- select_if
select_if(mydata, is.numeric)
select_if(mydata, is.factor)

## Make new variables

### Mutate() 
# --- mutate
mutate(mydata, Y = Y2015/Y2014)

# --- mutate_all 
mutate_all(mydata, funs("new" = .* 1000))

# --- mutate_at 
mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(.)))

# --- transmutate 
transmute(iris, sepal = Sepal.Length + Sepal.Width) # tạo ra biến mới và loại biến cũ

### rename Varibles

rename(mydata, Index23=Index)

# Summarize multiple 
summarise(mydata, Y2015_mean = mean(Y2015), Y2015_med=median(Y2015))

# --- summarise_at
summarise_at(mydata, vars(Y2005, Y2006), funs(n(), mean, median)) # cho ta áp dụng nhiều Var,Function cùng lúc
# --- Function in summarise:
# -- first() : giá trị đầu; last: giá trị cuối
# -- n(): số lượng quan sát n vector; n_distinct: số lượng giá trị khác nhau
# -- min; max; mean; median; var; sd.

# Combine table 

## Combine Variables

### join() function
df1 = data.frame(ID = c(1, 2, 3, 4, 5),
                 w = c('a', 'b', 'c', 'd', 'e'),
                 x = c(1, 1, 0, 0, 1),
                 y=rnorm(5))

df2 = data.frame(ID = c(1, 7, 3, 6, 8),
                 a = c('z', 'b', 'k', 'd', 'l'),
                 b = c(1, 2, 3, 0, 4),
                 c =rnorm(5))
# --- inner_join
df3 = inner_join(df1, df2, by = "ID") #Giữ các giá trị có cả ở a & b

# --- left_join
left_join(df1, df2, by = "ID") # Ghép các dòng có cùng giá trị từ b đến a.

# --- right_join 
right_join(df1,df2, by = "ID") # Ghép các dòng có cùng giá trị từ a đến b

# --- full_join
full_join(df1, df2, by = "ID") # Giữ tất cả các giá trị ở a & b

## Extract Rows

### semi_join
semi_join(df1, df2, by = "ID") #Giữ lại các dòng xuất hiện trong b

### anti_join
anti_join(df1, df2, by = "ID") #Giư lại các dòng không có trong b

## Combine cases
df1 = data.frame(ID = 1:6, x = letters[1:6])
df2 = data.frame(ID = 7:12, x = letters[7:12])
df3 = data.frame(ID = 11:14,x=letters[11:14],y=letters[11:14])
### bind_row() & bind_cols() 

a = bind_rows(df1, df3) # kết hợp nối đuôi
a1 = rbind(df1, df2) # không kết hợp được nếu không cùng level

b = bind_cols(df1, df2)
b1 = cbind(df1, df2)

### Intersect 
y = data.frame(x1 = c("A", "B", "C"), x2 = 1:3)
z = data.frame(x1 = c("B", "C", "D"), x2 = c(2,3,4))

int = intersect(y,z) # Các dòng có ở cả y & z

### union()
uni = union(y, z) #Dòng có trong y hoặc z

### setdiff()
set = setdiff(y, z) #Dòng có trong y nhưng không có trong z

# Group Cases

group_by(iris, Species) #Nhóm các biến thành các hàng có cùng giá trị của Species

gr = mydata %>%
  group_by(Index) %>%
  summarise_at(vars(Y2011:Y2015), funs(n(), mean(., na.rm = T)))




