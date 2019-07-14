

# Import data 
mydata = read.csv("sampledata.csv")
iris = iris
flights = flights

# MANIPULATE CASES 

## Extract Cases 

### filter

Jan1 = filter(flights, month == 1, day == 1)
----- " >, >=, <,=, ==, !=(không bằng), &, ! "

sqrt(2)^2 == 2
----- " ta đang so sánh căn bậc 2^2 có = 2 ? )
near(sqrt(2)^2,2)
---- " thực tế căn bậc 2^2 chỉ xắp xir2 chứ k = )

#--# Multiple argument to filter 
----# y & !x; x & !y; x & y....
filter(flights, !(arr_delay > 120 | dep_delay >120))

filter(flights, arr_delay <= 120, dep_delay <= 120)

### distinct()
iris %>%
  distinct(Species)
---# loại bỏ các hàng trùng lập trên 1 biến

### slice()
iris %>%
  slice(10:15)

### Sample_frac()
sample_frac(mydata, 0.2)
### sample_n
sample_n(mydata, 3)
---# cả 2 đều mang tính chất lấy dl dòng theo tỷ lệ hoặc số tuyệt đối.
  
## Arrange cases 
arrange(mydata, Index, Y2011)
---# sắp xếp các dòng từ nhỏ đến lớn
arrange(mydata, desc(Index), Y2011 )
---# sắp xếp các dòng từ lớn đén nhỏ 

## Extract varibles 
  
### select()

mydata2 = select(mydata, Index, State:Y2008)

mydata = select(mydata, -c(Index, State))
---# argument to select
starts_with() # Starts with a prefix " khớp với những tên bắt đầu"
select(mydata, starts_with("Y2004"))

ends_with()   # Ends with a prefix " khớp với những tên kết thúc "abc" "

contains()    # Contains a literal string " khớp với những tên chứa "abc"
select(flights, contains("TIME"))
matches()     # Matches a regular expression " Chọn biến mà tên biến đó khớp với điền kiện cho trước"
num_range()   # Numerical range like x01, x02, x03.
one_of()      # Variables in character vector. " Chọn các biến mà tên biến đó nằm trong 1 nhóm các tên biến"
everything()  # All variables.

mydata3 = select(mydata, contains("I"))

## Make new variable 

### Mutate() # kết hợp biến mới tính vào data luôn

mutate = mutate(mydata, change = Y2015/Y2014)

mutate_all = mutate_all(mydata, funs("_new" = .* 1000))

mutate_at = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(.)))
---# "By default, min_rank() assigns 1 to the smallest value and high number to the largest value. In case, you need to assign rank 1 to the largest value of a variable, use min_rank(desc(.))"
mutate_at1 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(desc(.))))

mutate_each = mutate_each(iris, funs(min_rank()))
---# áp dụng hàm cho mỗi biến 

transmute = transmute(iris, sepal = Sepal.Length + Sepal.Width)
---# tính toán tạo biến mới và loại bỏ biến cũ.

  ### Rename() 
  # >> rename() syntax : rename(data , new_name = old_name), data : Data Frame
  rename = rename(mydata, Index1=Index)
  































































