# Quiz 1
a = read.csv("./quiz1_data/hw1_data.csv")
# Extract the subset of rows of the data frame where Ozone values are above 31
# and Temp values are above 90. What is the mean of Solar.R in this subset?
#ot = !is.na(a$Ozone >=32)
ot = !is.na(a$Ozone >=32) & a$Temp >90
ans18 = a[ot,c("Ozone","Wind")]
message("ans18= ",ans18)

#What is the mean of "Temp" when "Month" is equal to 6? 

ans19 = mean(a[a$Month == 6,"Temp"])
message("ans19= ",ans19)

#What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?

ans20 =  max(a[a$Month == 5,"Ozone"][!is.na(a[a$Month == 5,"Ozone"])])
message("ans20= ",ans20)