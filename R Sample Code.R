array(data=customer_churn$TotalCharges, dim=c(10))-> array_total_charges
array_total_charges
array(data=customer_churn$MonthlyCharges, dim=c(5))-> array_monthly_charges
array_monthly_charges

customer_churn[50:60,c(2,3)]->c_random1
customer_churn[1:15,"Salary"]->array_monthly_charges
array_monthly_charg

array(data=(placementdataset[1:15,"salary"], dim=c(15,5))-> array2 

placementdataset<-read.csv("C:/Jay'sFolderPlusTelliant/IntellipatDocuments/Placement_Data_Full_Class.csv")
placementdataset

num1<-placementdataset[1:15,"salary"]
num2<-placementdataset[1:5,"mba_p"]
num3<-placementdataset[1:10,"etest_p"]

pharma<-read.csv("C:/Jay'sFolderPlusTelliant/IntellipatDocuments/Pharmacovigilance_audit_Data.csv")

pharma[,c(5,2)]->Pharma_subset
Pharma_subset
pharma[,c(6,3)]->Pharma_subset
Pharma_subset
pharma[,c(4,5,2)]->Pharma_subset
Pharma_subset
pharma[100:200,c(1,3,6)]
mean(customer_churn$tenure)
mean(customer_churn$MonthlyCharges)
mean(customer_churn$TotalCharges)

mean(placementdataset$ssc_p)
mean(placementdataset$degree_p)

citytemp<-read.csv("C:/Jay'sFolderPlusTelliant/IntellipatDocuments/city_temperature.csv")
class(citytemp$Region)
--use Class function to find the type of the object
Character data type
class(citytemp$AvgTemperature)


countPlaced =0
countScience = 0
countCommerce = 0

for(i in 1:nrow(placementdataset)){
  if(placementdataset$status[i]=="Placed"){
  countPlaced=countPlaced+1
  }
  if(placementdataset$hsc_s[i]=="Science"){
  countScience=countScience+1
  }
  if(placementdataset$hsc_s[i]=="Commerce"){
  countCommerce=countCommerce+1
  }
}

countPlaced
countScience
countCommerce


counthsc80 = 0

for(i in 1:nrow(placementdataset)){
  if(placementdataset$hsc_p[i]>=80){
    counthsc80=counthsc80+1
  }

}

counthsc80

countmba75 = 0

for(i in 1:nrow(placementdataset)){
  if(placementdataset$mba_p[i]>75){
    countmba75=countmba75+1
  }
  
}

countmba75



if(customer_churn$PaymentMethod[28]=="Credit card (automatic)"){
print (" Auto Credit card ")
} else if (customer_churn$PaymentMethod[28]=="Mailed check") {
  print (" mailed check")
} else if (customer_churn$PaymentMethod[28]=="Electronic check") {
  print ("Electronic check")
} else { print ("Unknow payment")}


if(placementdataset$status[95]=="Placed"){
  print (" The student is place")
} else if (placementdataset$status[95]=="Not Placed") {
  print (" The student is not placed")
} else if (placementdataset$status[95]=="NA") {
  print (" not Available")
} else { print ("I=Unknow status")}



val <- switch( 
  4, 
  "Geeks1", 
  "Geeks2", 
  "Geeks3", 
  "Geeks8", 
  "Geeks5", 
  "Geeks6"
) 
print(val) 



placementdataset$degree_p[67]

switch (as.character(placementdataset$hsc_s[67]),
        "Science"=placementdataset$degree_p[67]+5, 
        "Commerce"=placementdataset$degree_p[67]+3) -> placementdataset$degree_p



placementdataset$degree_p[67]



countCentral=0
countScience = 0
i=1
while(i<=nrow(placementdataset)){
  if(placementdataset$hsc_b[i]=="Central"){
    countCentral=countCentral+1
  }
  if(placementdataset$hsc_s[i]=="Science" & placementdataset$hsc_p[i]>75)
 {
    countScience=countScience+1
  }
  
  
    i=i+1
}

print ("Central Board students count")
countCentral
print ("Science students percent > 75")
countScience

newdata <- subset(pharma, Gender == 'F'& Age < 25 & Issues == 'unclear dose')
newdata

newdata <- subset(pharma, Gender == 'M'& Age < 25 & Issues == 'Medication history documenting error')
newdata <- subset(pharma,  Age == 8 & Issues == 'Medication history documenting error')


nrow(placementdataset)
ncol(placementdataset)

newdata <- subset(placementdataset, degree_t == 'Sci&Tech' & status == 'Placed')
nrow(subset(placementdataset, degree_t == 'Sci&Tech' & status == 'Placed'))

nrow(newdata <- subset(placementdataset, salary > 300000 ))

class(placementdataset$sl_no)    
class(placementdataset$ssc_b)
class(placementdataset$degree_p)

newdata <- subset(customer_churn, gender == 'Female' & SeniorCitizen == 1)

newdata <- 
subset(customer_churn, PaymentMethod == 'Mailed check' & Contract == 'One year')
newdata
subset(customer_churn, PaymentMethod == 'Bank transfer (automatic)' 
       | PaymentMethod == 'Credit card (automatic)')

newdata <-subset(customer_churn, gender == 'Male' &  SeniorCitizen == 1 & Partner == 'Yes'
       & InternetService == 'DSL' & PhoneService == 'Yes' )

table(customer_churn$OnlineBackup)

head(placementdataset$salary,6)
head(placementdataset$salary,6) +5000 ->head(placementdataset$salary,6)


placementdataset[1:6,15] + 5000 -> placementdataset[1:6,15] 

c(1, "two", 3, TRUE, FALSE) -> set1

class(set1)
set1

c( TRUE, FALSE) -> set2
class(set2)


placementdataset<-read.csv("C:/Jay'sFolderPlusTelliant/IntellipatDocuments/Placement_Data_Full_Class.csv")

placementdataset$etest_p[1-6]+5000 -> placementdataset$etest_p[1-6]

placementdataset[1:10,11] +7.5-> placementdataset[1:10,11]
placementdataset[1:10,11]

(head(placementdataset,6)$salary) 

(head(placementdataset,6)$salary) + 5000 = placementdataset()

(head(placementdataset,3)$mba_p) - 5


tail(placementdataset,6)$ssc_p



nrow(placementdataset)

tail(placementdataset,8)$salary
((tail(placementdataset,8)$salary) +12000) -> placementdataset[208:215,15]
tail(placementdataset,8)$salary

sample(pharma$LocationID,5)

sample(pharma$Issues,10)

str(customer_churn)

as.numeric(),as.character() , as.vector(), as.matrix(), as.data.frame)

customer_churn<-read.csv("C:/Jay'sFolderPlusTelliant/IntellipatDocuments/customer_churn.csv")


as.character(customer_churn$InternetService)

class(customer_churn$InternetService)

customer_churn$InternetService

str(customer_churn)

as.integer(customer_churn$TotalCharges)

is.vector(customer_churn$InternetService)
as.character(customer_churn$InternetService) -> customer_churn$InternetService
is.character(customer_churn$InternetService)
is.integer(customer_churn$InternetService)

str(placementdataset)
str(pharma)
str(citytemp)

alpha <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)

matrix(data=alpha, nrow=4, ncol=4, byrow=T) ->mat_alpha
mat_alpha

data1 <- c(1:64)
matrix(data=data1, nrow =8, ncol=4, byrow=T)


list(1,"nirvana",TRUE) -> mixbag

list ("DSL","Fibre Optic", "Cable BroadBand", "Wireless", 30:40, TRUE, FALSE, TRUE,TRUE,TRUE,FALSE)->
internet_service

a <- "this is a 
long string over two lines"

list1 <- list("Month To Month", "One year", "Two Year", "Three Year")
list2 <- list(1,2,3,4,5)

contract_list <- list(list1, list2) 

contract_list[[1]][[2]]

list1 <- list("Month To Month", "One year", "Two Year", "Three Year")
list2 <- list(1,2,3,4,5)
list3 <- list(TRUE,FALSE, TRUE, TRUE)

contract_list <- list(list1, list2, list3) 
contract_list[[2]][[1]]
contract_list[[1]][[2]]  

contract_list[[3]][[3]]

v_specialization <- c("Marketing", "Finance", "techSupport")


class (v_specialization)

range(customer_churn$InternetService)
customer_churn [,c(3,6,9)]

customer_10_20 <-customer_churn[,10:20]

customer_random_rows <-customer_churn[c(65, 765,3726,7000),]

customer_churn$tenure>60 -> c_tenure

subset(customer_churn,c_tenure=T)->c_tenure
head(c_tenure)

customer_churn$MonthlyCharges[5] + 5 -> customer_churn


if (customer_churn$tenure[1] > customer_churn$tenure[10]) {
  print("1st customer tenure is greater than 10th customer")
} else {print("1st custome teunre is LT 10th customer")}

customer_churn<-read.csv("C:/Jay'sFolderPlusTelliant/IntellipatDocuments/customer_churn.csv")



#Get the count of those customers who have subscribed to both "TechSupport" & "StreamingTV"
#& customer_churn$StreamingTV == 'Yes')

count=0
i=1
while(i<=nrow(customer_churn)){
  if(customer_churn$TechSupport[i]=="Yes" & customer_churn$StreamingTV[i] == 'Yes'){
    count=count+1
  }
  i=i+1
}

count
#Extract those customers whose 'InternetService' is either 'DSL' or 'Fiber optic' & store
#the result in 'Internet_dsl_fiber

Internet_dsl_fiber <- subset(customer_churn, InternetService == 'DSL' | InternetService == 'Fiber optic')

Internet_dsl_fiber$InternetService


customer_churn<-read.csv("C:/Jay'sFolderPlusTelliant/IntellipatDocuments/customer_churn.csv")

head(customer_churn$PhoneService,4)

tail(customer_churn$TotalCharges,1)

mean(customer_churn$tenure)
min(customer_churn$tenure)
max(customer_churn$tenure)

sample(pharma$LocationID,5)

sample(customer_churn$TotalCharges,10)
table(customer_churn$PaymentMethod)
table(customer_churn$Contract)

#12th cell of 'Contract' column
if (customer_churn$Contract[12] == "monthtomonth") {
  print("The contract is on a month to month basis")
} else if (customer_churn$Contract[12] == "One year") {
  print ("The contracrt is one year")
} else if (customer_churn$Contract[12] == "Two year") {
  print ("The contract is two year ")
} else { print ("None")}

if(customer_churn$PaymentMethod[28]=="Credit card (automatic)"){
  print (" Auto Credit card ")
} else if (customer_churn$PaymentMethod[28]=="Mailed check") {
  print (" mailed check")
} else if (customer_churn$PaymentMethod[28]=="Electronic check") {
  print ("Electronic check")
} else { print ("Unknow payment")}

switch (as.character(placementdataset$hsc_s[67]),
        "Science"=placementdataset$degree_p[67]+5, 
        "Commerce"=placementdataset$degree_p[67]+3) -> placementdataset$degree_p


switch (as.character(customer_churn$gender[6]),
        "Male"=customer_churn$MonthlyCharges[6]*.8, 
        "Female"=customer_churn$MonthlyCharges[6])*.5 -> customer_churn$MonthlyCharges[6]

#Use for loop to get the count of customers whose 'InternetService' is 'DSL'

count = 0
for(i in 1:nrow(customer_churn)){
  if(customer_churn$InternetService[i] == "DSL"){
    count=count+1
  }
}
count

#Use while to find the number of customers whose tenure is exactly '2' months
i = 1
count = 0
while (i<=nrow(customer_churn)){
  if (customer_churn$tenure[i] == 2) {
    count = count +1
  }
 i = i+1 
}

print(as.character(count))

#A character vector named 'fruits' with these values: 'Apple', 'Guava', 'Banana', 'Mango'


#Create a list named 'jumbo' which comprises of:
#  a. A character vector comprising of alphabets from A to D
b. A numeric vector comprising of numbers from 55 to 60
c. A logical vector comprising of just these two values: True, False
i. Now, access the third value from the first element of the list
ii. Access the 2nd value from the 2nd element of the list
iii. Access the 1st value from the 3rd element of the list 

char1 <- c("A", "B", "C", "D")
numeric <- c(55:60)
logical1 <-(TRUE,FALSE)
jumbo <- (char1,numeric,logical1)
jumbo (1,3)
# Create a matrix named 'four_trouble', with the numbers 1 to 16. The matrix should have 4 rows
#& 4 columns
#a. Arrange the elements by row 
four_trouble <- c(1:16)
matrix(data=c(1:16), nrow =4, ncol=4, byrow=T) -> four_trouble

#Create an array named 'sky_maze' with the numbers 1 to 32. The array should comprise of two
#4*4 matrices 

num1<-c(1:32)
array(data=c(num1), dim=c(4,4,2))-> array1
array1

library(dplyr)

select(customer_churn,1,4,7,12) ->c_bunch
head(customer_churn[,c(1,4,7)])


select(customer_churn,5:10) ->c_5_10
head(customer_churn[,c(5:10)])


select(customer_churn,gender) ->c_gender
customer_churn$gender

select(customer_churn,gender,Partner,tenure) ->c_gpt

customer_churn(, c(2,4,6))

library(dplyr)


subset(customer_churn, tenure >= 20 | tenure < 10)


vview()
table(customer_churn$gender)

sample_n(customer_churn,10)->random_10
head(random_10)


sample_n(customer_churn,0.1)->random_10percent
head(random_10percent,10)

sample_n(customer_churn,0.5)->random_50percent
head(random_50percent,10)

count(customer_churn,gender)

summarise(customer_churn,mean_tenure=mean(tenure))
##   mean_tenure
## 1    32.37115
summarise(customer_churn,min_tenure=min(tenure))
##   min_tenure
## 1          0
summarise(customer_churn,max_tenure=max(tenure))
##   max_tenure
## 1         72

summarise(group_by(customer_churn,InternetService),mean_tenure=mean(tenure))
## # A tibble: 3 x 2
##   InternetService mean_tenure
##   <fct>                 <dbl>
## 1 DSL                    32.8
## 2 Fiber optic            32.9
## 3 No                     30.5
________________________________________
summarise(group_by(customer_churn,Partner),mean_MonthlyCharges=mean(MonthlyCharges))
## # A tibble: 2 x 2
##   Partner mean_MonthlyCharges
##   <fct>                 <dbl>
## 1 No                     61.9
## 2 Yes                    67.8

summarise(customer_churn,mean_TC=mean(TotalCharges,na.rm=T))
##   mean_TC
## 1  2283.3


customer_churn %>% select(1:5) -> c_15
head(c_15)
##   customerID gender SeniorCitizen Partner Dependents
## 1 7590-VHVEG Female             0     Yes         No
## 2 5575-GNVDE   Male             0      No         No
## 3 3668-QPYBK   Male             0      No         No
## 4 7795-CFOCW   Male             0      No         No
## 5 9237-HQITU Female             0      No         No
## 6 9305-CDSKC Female             0      No         No
________________________________________
customer_churn %>% select(1:5) %>% filter( gender =="Male") -> c_15_male
head( c_15_male)
##   customerID gender SeniorCitizen Partner Dependents
## 1 5575-GNVDE   Male             0      No         No
## 2 3668-QPYBK   Male             0      No         No
## 3 7795-CFOCW   Male             0      No         No
## 4 1452-KIOVK   Male             0      No        Yes
## 5 6388-TABGU   Male             0      No        Yes
## 6 9763-GRSKD   Male             0     Yes        Yes
________________________________________
customer_churn %>% filter(InternetService=="DSL") %>% group_by(gender) %>% summarise(mean_mc=mean(MonthlyCharges))
## # A tibble: 2 x 2
##   gender mean_mc
##   <fct>    <dbl>
## 1 Female    58.6
## 2 Male      57.6
________________________________________
customer_churn %>% select(1,2,"MonthlyCharges","PaymentMethod") %>% filter(MonthlyCharges>100 & gender =="Male") -> c_male_payment
________________________________________
customer_churn %>% group_by(PaymentMethod) %>% summarise(mean_tenure=mean(tenure))
## # A tibble: 4 x 2
##   PaymentMethod             mean_tenure
##   <fct>                           <dbl>
## 1 Bank transfer (automatic)        43.7
## 2 Credit card (automatic)          43.3
## 3 Electronic check                 25.2
## 4 Mailed check                     21.8
________________________________________
customer_churn %>% group_by(PaymentMethod) %>% summarise(mean_tenure=mean(tenure)) %>% arrange(desc(PaymentMethod))
## # A tibble: 4 x 2
##   PaymentMethod             mean_tenure
##   <fct>                           <dbl>
## 1 Mailed check                     21.8
## 2 Electronic check                 25.2
## 3 Credit card (automatic)          43.3
## 4 Bank transfer (automatic)        43.7
________________________________________
customer_churn %>% select(1,2,10:21) %>% filter(Contract=="One year" | Contract=="Two year") %>% arrange(Contract) -> c_contract
head(c_contract)
##   customerID gender      OnlineSecurity        OnlineBackup
## 1 5575-GNVDE   Male                 Yes                  No
## 2 7795-CFOCW   Male                 Yes                  No
## 3 6388-TABGU   Male                 Yes                 Yes
## 4 8091-TTVAX   Male                  No                  No
## 5 8191-XWSZG Female No internet service No internet service
## 6 1680-VDCWW   Male No internet service No internet service
##      DeviceProtection         TechSupport         StreamingTV
## 1                 Yes                  No                  No
## 2                 Yes                 Yes                  No
## 3                  No                  No                  No
## 4                 Yes                  No                 Yes
## 5 No internet service No internet service No internet service
## 6 No internet service No internet service No internet service
##       StreamingMovies Contract PaperlessBilling             PaymentMethod
## 1                  No One year               No              Mailed check
## 2                  No One year               No Bank transfer (automatic)
## 3                  No One year               No Bank transfer (automatic)
## 4                 Yes One year               No   Credit card (automatic)
## 5 No internet service One year               No              Mailed check
## 6 No internet service One year               No Bank transfer (automatic)
##   MonthlyCharges TotalCharges Churn
## 1          56.95      1889.50    No
## 2          42.30      1840.75    No
## 3          56.15      3487.95    No
## 4         100.35      5681.10    No
## 5          20.65      1022.95    No
## 6          19.80       202.25    No
________________________________________
customer_churn %>% filter(PaperlessBilling=="No") %>% group_by(TechSupport) %>%summarise(mean_tenure=mean(tenure))
## # A tibble: 3 x 2
##   TechSupport         mean_tenure
##   <fct>                     <dbl>
## 1 No                         25.1
## 2 No internet service        30.3
## 3 Yes                        44.2
________________________________________


sqldf("select * from customer_churn where gender='Male' and Contract='One year'") -> c_male_one
head(c_male_one)
##   customerID gender SeniorCitizen Partner Dependents tenure PhoneService
## 1 5575-GNVDE   Male             0      No         No     34          Yes
## 2 7795-CFOCW   Male             0      No         No     45           No
## 3 6388-TABGU   Male             0      No        Yes     62          Yes
## 4 8091-TTVAX   Male             0     Yes         No     58          Yes
## 5 1680-VDCWW   Male             0     Yes         No     12          Yes
## 6 8865-TNMNX   Male             0     Yes        Yes     10          Yes
##      MultipleLines InternetService      OnlineSecurity        OnlineBackup
## 1               No             DSL                 Yes                  No
## 2 No phone service             DSL                 Yes                  No
## 3               No             DSL                 Yes                 Yes
## 4              Yes     Fiber optic                  No                  No
## 5               No              No No internet service No internet service
## 6               No             DSL                  No                 Yes
##      DeviceProtection         TechSupport         StreamingTV
## 1                 Yes                  No                  No
## 2                 Yes                 Yes                  No
## 3                  No                  No                  No
## 4                 Yes                  No                 Yes
## 5 No internet service No internet service No internet service
## 6                  No                  No                  No
##       StreamingMovies Contract PaperlessBilling             PaymentMethod
## 1                  No One year               No              Mailed check
## 2                  No One year               No Bank transfer (automatic)
## 3                  No One year               No Bank transfer (automatic)
## 4                 Yes One year               No   Credit card (automatic)
## 5 No internet service One year               No Bank transfer (automatic)
## 6                  No One year               No              Mailed check
##   MonthlyCharges TotalCharges Churn
## 1          56.95      1889.50    No
## 2          42.30      1840.75    No
## 3          56.15      3487.95    No
## 4         100.35      5681.10    No
## 5          19.80       202.25    No
## 6          49.55       475.70    No
table(c_male_one$Contract)
## 
## Month-to-month       One year       Two year 
##              0            755              0
________________________________________


sqldf("select count(gender) from customer_churn where gender='Female'")
##   count(gender)
## 1          3488
________________________________________
sqldf("select count(gender) from customer_churn where gender='Male'")
##   count(gender)
## 1          3555
________________________________________


________________________________________
sqldf("select avg(tenure), PaymentMethod from customer_churn group by PaymentMethod")
##   avg(tenure)             PaymentMethod
## 1    43.65674 Bank transfer (automatic)
## 2    43.26938   Credit card (automatic)
## 3    25.17463          Electronic check
## 4    21.83002              Mailed check
________________________________________
sqldf("select avg(MonthlyCharges), Contract from customer_churn group by Contract")
##   avg(MonthlyCharges)       Contract
## 1            66.39849 Month-to-month
## 2            65.04861       One year
## 3            60.77041       Two year
________________________________________


1. Extract these individual columns:
  a. Extract the 5th column & store it in 'customer_5'
b. Extract the 15th column & store it in 'customer_15'

customer_5 <-select(customer_churn,5)

2. Extract the column numbers 3,6,9,12,15 & 18 and store the result in 'customer_3_multiple'
customer_3_multiple <- select(customer_churn,3,6,9,12,15,18)

3. Extract all the columns from column number-10 to column number-20 and store the result in
'c_10_20'
c_10_20 <- select (customer_churn,10:20
                   )
4. Extract all the columns which start with letter 'P' & store it in 'customer_P'
customer_p <- select (customer_churn, starts_with("P"))

5. Extract all the columns which end with letter 's' & store it in 'customer_s'
customer_s <- select(customer_churn,ends_with('s'))

sample_n(customer_churn,333) -> customer_333
nrow(customer_churn)

customer_churn<-read.csv("C:/Jay'sFolderPlusTelliant/career/IntellipatDocuments/customer_churn.csv")

customer_1 <- read.table("C:/Jay'sFolderPlusTelliant/career/IntellipatDocuments/customer_churn.csv",
                         header = FALSE, sep="'", quote="", stringsAsFactors=FALSE, skip=5)

getOption("max.print")

library(dplyr)

sample_n(customer_churn,10)
sample(customer_churn,1)

1.Extract 333 random records from the customer_churn dataframe & store the result in
'customer_333'
sample_n(customer_churn,333) -> customer_333


2. Extract 1000 random records from the customer_churn dataframe & store the result in
'customer_1000'

options(max.print=1000)
getOption("max.print")

sample_n(customer_churn,1000) -> customer_1000
print(customer_1000)


3. Randomly extract 23% of the records from the customer_churn dataframe & store the result in
'customer_23_percent'
sample_n(customer_churn,0.5)
sample (customer_churn,0.23)
sample_frac(customer_churn,0.2)

4. Get the count of different levels from the 'PaymentMethod' column
count(customer_churn,PaymentMethod)

5. Get the count of different levels from the 'Churn' column
count(customer_churn,Churn)

path.expand("~")

sample_frac(customer_churn, size = .001)

#1.Get the median, variance & standard deviation for the 'tenure' column
summarize(customer_churn, median(tenure))
summarize(customer_churn, mean(tenure))
summarise(customer_churn, var1 = variance(tenure))
var(customer_churn$tenure)
sd(customer_churn$tenure)

#2. Get the median, variance & standard deviation for the 'MonthlyCharges' column
#3. Get the standard deviation of 'tenure' & group it w.r.t 'PaymentMethod' column
summarise(group_by(customer_churn,PaymentMethod),var_tenure=var(tenure))

#4. Get the median of 'MonthlyCharges' & group it w.r.t 'Contract' column
summarise(group_by(customer_churn,Contract),med_monthlycharges=median(MonthlyCharges))

library(sqldf)

#Select the 'OnlineBackup' column from the customer_churn dataframe & store the result in
#'customer_onine_backup'
sqldf("Select OnlineBackup from customer_churn")

#Select all the customers whose payment method is 'mailed check' and store the result in
#'customer_mail'
sqldf("select *from customer_churn where PaymentMethod = 'Mailed check' ") -> customer_mail

#Select all the Female customers whose tenure is of 1 month & Payment Method 
sqldf("select *from customer_churn where gender = 'Female' and tenure = 1 and 
      PaymentMethod = 'Mailed check' ") -> customer_mail

filter(placementdataset,hsc_s=="Science" | hsc_s=="Commerce")

median(customer_churn$tenure)
median(placementdataset$ssc_p) -> med1-

mutate(customer_churn, Age= ifelse(SeniorCitizen==1, sample(x=56:100),sample(x=16:55))) -> customer_churn
head(customer_churn,100)

mutate(customer_churn,customer_category=ifelse(MonthlyCharges <45, "Low Paying", ifelse(MonthlyCharges<90,"Medium_paying","High_paying")))-> customer_churn
head(customer_churn,20)

v1 = c(1,"jay",4,7)
class(v1)


alpha <- c(2,4,6,8,10,12,14,"jay","shri","teju", "manu",24,26,28,30,32)
matrix(data=alpha, nrow=4, ncol=4, byrow=T)->mat_alpha
mat_alpha


A list of different data types -   list( 'A', 2, 'C') ->  List1
A second list of characters - list2 <-list('Jay', 'Vidya', 'Teju', 'Manu')
A third list of characters and a list list3 <-list ('Ramesh', 'geetha', list1)

A  fourth list is a list of list  list4 <- list (list1, list2,list3)




beta <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32)
matrix(data=beta, nrow=16, ncol=1, byrow=T)->mat_beta
mat_beta
class(placementdataset$hsc_s[67])

switch ((placementdataset$hsc_s[67]),
        "Science"=placementdataset$degree_p[67]+5, 
        "Commerce"=placementdataset$degree_p[67]+3) -> placementdataset$degree_p

placementdataset[1:10,11] +7.5-> placementdataset[1:10,11]
placementdataset[1:10,11]
placementdataset[1,2]

#-> placementdata[1:3,12]


(head(placementdataset,6)$salary) + 5000
(head(placementdataset,3)$salary) + 500 -> placementdataset[1:3,15]
(head(placementdataset,3)$salary) + 100 -> placementdataset[1:3,15]

(head(placementdataset,3)$mba_p) + 100 -> placementdataset[1:3,13]
(head(placementdataset,3)$mba_p) -5  -> placementdataset[1:3,13]

View(customer_churn)

list1 <- list("Month To Month", "One year", "Two Year", "Three Year")
list2 <- list(1,2,3,4,5)
list3 <- list(TRUE,FALSE, TRUE, TRUE)

contract_list <- list(list1, list2, list3)

contract_list[1,1,1]

#The below mutate statement creates a new column customer_category, its value will be based on a logic,  
#When MonthlyCharges<45 then the customer_category will be "Low Paying" ,  when MonthlyCharges<90 then 
#the customer_category will be "medium paying" else if MonthlyCharges> 90 then the customer_category 
#will be "high paying"

library(dplyr)

mutate(customer_churn,customer_category=ifelse(MonthlyCharges <45, "Low Paying", ifelse(MonthlyCharges<90,"Medium_paying","High_paying")))-> customer_churn
mutate(customer_churn,customer_category=ifelse(MonthlyCharges <45, "Low Paying", ifelse(MonthlyCharges<90,"Medium_paying","High_paying")))-> customer_churn
customer_churn

count(customer_churn,gender)
table(customer_churn$gender)

customer_churn %>% filter(InternetService=="DSL") %>% group_by(gender)%>%summarise(mean_mc=mean(MonthlyCharges))

customer_churn %>% group_by(PaymentMethod) %>% summarise(mean_tenure=mean(tenure))
customer_churn %>% group_by(PaymentMethod) %>% summarise(count_tenure=n(), mean_tenure=mean(tenure))

select (placementdataset,specialisation)

select (placementdataset,1:5) -> Placement_15

customer_churn$TotalCharges -> TotalChargesColumn

sd(TotalChargesColumn,na.rm=TRUE)


summarise(placementdataset,sd(etest_p))

summarise(group_by(placementdataset,salary),sd(etest_p),na.rm=TRUE)
var(customer_churn$TotalCharges,na.rm=TRUE) -> cus_Totalchg

placementdataset <-read.csv("C:/Jay'sFolderPlusTelliant/Career/IntellipatDocuments/Placement_Data_Full_Class.csv")

summarise(group_by(placementdataset,degree_t),var(degree_p),na.rm=TRUE) -> var_degree

sample(placementdataset,33)

sample_n(placementdataset,33)


sample_n(placementdataset, 10, weight = ssc_p)

customer_churn<-read.csv("C:/Jay'sFolder2020/Career/IntellipatDocuments/customer_churn.csv")

library(ggplot2)

ggplot(data = customer_churn, aes(x=tenure))

plot(density(customer_churn$MonthlyCharges),col="red")

hist(customer_churn$MonthlyCharges,col="Palevioletred", breaks=50)


plot(customer_churn$Contract,col="palegreen4",xlab="Contract",main="Distribution of Contract")

library(plotly)


plot(customer_churn$Dependents)

customer_churn<-read.csv("C:/Jay'sFolder2020/Career/IntellipatDocuments/customer_churn.csv")

customer_churn$Dependents

as.factor(customer_churn$Dependents) -> customer_churn$Dependents
plot(customer_churn$Dependents)


library(ggplot2)

ggplot(data=customer_churn,aes(x=tenure))+geom_histogram()

ggplot(data = customer_churn, aes(x=tenure))+geom_histogram(fill="palegreen4")


ggplot(data = customer_churn, aes(x=tenure))+geom_histogram(fill="palegreen4", col="green")

ggplot(data = customer_churn, aes(x=tenure, fill=Partner))+geom_histogram()

ggplot(data = customer_churn, aes(x=MonthlyCharges, fill=Contract))+geom_histogram(position = "identity")

customer_churn[,"MonthlyCharges"]->c_month
customer_churn$MonthlyCharges -> c_month1

table(customer_churn$gender) list2 <-list('Jay', 'Vidya', 'Teju', 'Manu')
list3 <-list ('Ramesh', 'geetha', list1)
list4 <- list (list1, list2,list3)
alpha <- c(2,4,6,8,10,12,14,"jay","shri","teju", "manu",24,26,28,30,32)

library(plyr)

count(customer_churn$gender) 


summarise(group_by(customer_churn,InternetService),mean_tenure=mean(tenure))

library(ggplot2)

ggplot(data = customer_churn,aes(x=SeniorCitizen))+geom_bar()

ggplot(data = customer_churn,aes(x=SeniorCitizen))+geom_histogram()


sessionInfo()

library(KernSmooth)