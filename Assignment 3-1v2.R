library(dplyr)
library(tidyr)

refine_original <- read.csv("~/Dropbox/Data Science - Springboard/Assignments/Data Wrangling 3-1/refine_original.csv")

productlist <- tbl_df(refine_original)

#3.1.2 separate product code and number into two distinct columns#
productlist <- productlist %>% 
  separate(Product.code...number, into = c("product_code", "product_number"), sep = '-')


#3.1.1 fix brand labels in company category #
# phillips: determine variations and number #
table(agrep(pattern = "phillips", x = productlist$company, value = T, max.distance = 5))

# phillips: matches are okay so replace name #
productlist$company[agrep(pattern = "phillips", x = productlist$company, max.distance = 5, value = F)] <- "phillips"

# akzo: determine variations and number #
table(agrep(pattern = "akzo", x = productlist$company, value = T, max.distance = 2))

# akzo: matches are okay so replace name and make lowercase #
productlist$company[agrep(pattern = "akzo", x = productlist$company, max.distance = 2, ignore.case = TRUE, value = F)] <- "akzo"

# van houten: determine variations and number #
table(agrep(pattern = "van houton", x = productlist$company, value = T, max.distance = 7, ignore.case = TRUE))

# van houten: matches are okay so replace name and make all lowercase #
productlist$company[agrep(pattern = "van houten", x = productlist$company, max.distance = 6, ignore.case = TRUE, value = FALSE)] <- "van houten"

# unilever: determine variations and number #
table(agrep(pattern = "unilever", x = productlist$company, value = TRUE, max.distance = 5, ignore.case = TRUE))

# unilever: matches are okay so replace name and make lowercase
productlist$company[agrep(pattern = "unilever", x = productlist$company, max.distance = 5, ignore.case = TRUE, value = FALSE )] <- "unilever"

#order alphabetically by company #
productlist <- productlist  %>% arrange(company)

#remove levels that remain from untidy data#
productlist$company <- droplevels(productlist$company)


#3.1.3 add product categories #
productlist <- productlist %>% 
  mutate(product_category = 
      ifelse(product_code == "p", "Smartphone",
      ifelse(product_code == "v", "TV",
      ifelse(product_code == "x", "Laptop", "Tablet"))))


#3.1.4 combine address elements into one column"
productlist <- productlist  %>% 
   unite(full_address, address, city, country, sep = ", ")


#3.1.5 create biinary columns that identify each company for all company types#
productlist <- productlist  %>% 
  mutate(company_phillips = ifelse(productlist$company == "phillips", 1, 0))

productlist <- productlist  %>%   
  mutate(company_akzo = ifelse(productlist$company == "akzo", 1, 0))

productlist <- productlist  %>%
  mutate(company_van_houten = ifelse(productlist$company == "van houten", 1, 0))

productlist <- productlist  %>%  
  mutate(company_unilever = ifelse(productlist$company == "unilever", 1, 0))


#3.1.5 create biinary columns that identify each product for all product types#

productlist <- productlist %>% 
  mutate(product_smartphone = ifelse(productlist$product_category == "Smartphone", 1, 0))

productlist <- productlist %>% 
  mutate(product_tv = ifelse(productlist$product_category == "TV", 1, 0))

productlist <- productlist %>% 
  mutate(product_laptop = ifelse(productlist$product_category == "Laptop", 1, 0))

productlist <- productlist %>% 
  mutate(product_tablet = ifelse(productlist$product_category == "Tablet", 1, 0))

# rename table #
refine_clean <- productlist


#now make a .csv file#
write.csv(refine_clean, "refine_clean.csv")