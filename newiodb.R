install.packages("tidyverse")
install.packages("readxl")
install.packages("stringr")
library(tidyverse)
library(readxl)
library(stringr)


filedir <- "C:/users/tengkun/desktop/"
analydb <- read_excel(paste(filedir, "wiot2014.xlsx", sep=""))
analydb <- analydb %>% mutate("rowfactor" = paste("input", country, code, sep="_")) %>% select(rowfactor, everything())

colname_list <- colnames(analydb)
colname_length <- length(colname_list)
colname_list <- colname_list[5:colname_length]
country_list <- analydb[2,]
country_list <- as.vector(country_list[1,-c(1,2,3,4)])

name_list <- str_c("input", country_list, colname_list, sep = "_")

colnames(analydb)[5:colname_length] <- name_list

analydb <- analydb[-c(1,2,3), -c(2,3,4)]
analydb <- type_convert(analydb)
name_list0 <- colnames(analydb)

name_replace_n <- grep( "input_.+_CONS", name_list0)
name_replace_str <- name_list0[name_replace_n]
name_list0[name_replace_n] <- str_replace(name_replace_str,"input_", "fin_")

name_replace_n <- grep( "input_.+_GFCF", name_list0)
name_replace_str <- name_list0[name_replace_n]
name_list0[name_replace_n] <- str_replace(name_replace_str,"input_", "fin_")

name_replace_n <- grep( "input_.+_INVEN", name_list0)
name_replace_str <- name_list0[name_replace_n]
name_list0[name_replace_n] <- str_replace(name_replace_str,"input_", "fin_")

colnames(analydb) <- name_list0

analydb <- analydb %>% rename(out=input_TOT_GO)

temp_length <- dim(analydb[,1])[1]

replace_str <- analydb[seq(temp_length - 7,temp_length),1]

str_replace0 <- function(db){
  temp_list <- c()
  for(stri in db[,1]){
    temp_list <- c(temp_list,str_replace(stri, "input_", "other_"))
  }
  return (temp_list)
}

replace_str0 <- str_replace0(replace_str)
analydb[seq(temp_length - 7,temp_length),1] <- replace_str0
analydb[temp_length - 2,1] <- "factor_VA"
analydb[temp_length,1] <- "out"

write_csv(analydb, paste(filedir, "wiot2014.csv", sep=""))

tibble_over <- function(db_f, db_c){
  db_f <- db_f
  db_c <- as.data.frame(db_c)
  for(i in 1:dim(db_c)[1]){
    db_f[,i] = db_f[,i] / as.double(db_c[i,1])
  }
  return(db_f)
}

final_demand <- function(db, countryvar){
  country_name <- paste("input_", countryvar, sep="")
  temp_db <- db %>% filter(str_detect(rowfactor, paste("^input_", countryvar, sep=""))) %>% select(-starts_with(country_name), -rowfactor)
  
  temp_db <- temp_db %>% filter(out!=0)
  temp_db <- temp_db %>% select(-out, -starts_with(paste("fin_", countryvar, sep="")))
  final_db <- temp_db %>% mutate("fin_demand" = rowSums(temp_db))
  write_csv(final_db, "D:/wiot_final2014_chn.csv")
  return (final_db)
  }

unitcountrydc <- function(db, countryvar){
  # intermediate input matrix extract,iim  dim(iim) ----- n*k,n*k
  iim <- analydb %>% filter(str_detect(rowfactor, paste("^input_", countryvar, sep=""))) %>% select(starts_with(paste("input_", countryvar, sep="")))
  # out value matrix, out dim(out) ------n*k,1
  out <- analydb %>% filter(str_detect(rowfactor, paste("^input_", countryvar, sep=""))) %>% select(starts_with("out"))
  out_logical <- out["out"] == 0
  logical_locate <- grep(FALSE, out_logical)
  iim_name <- colnames(iim)
  iim_name_extract <- iim_name[logical_locate]
  out <- out[!out_logical,]
  
  iim <- iim[!out_logical, iim_name_extract]
  # built direct input coefficient matrix, dic
  dic <- tibble_over(iim, out)
  return (dic)
}

unitcountryav <- function(db, countryvar){
  # intermediate input matrix extract,iim  dim(iim) ----- n*k,n*k
  factor_value <- analydb %>% filter(str_detect(rowfactor, "factor_")) %>% select(starts_with(paste("input_", countryvar, sep="")))
  # out value matrix, out dim(out) ------n*k,1
  out <- analydb %>% filter(str_detect(rowfactor, paste("^input_", countryvar, sep=""))) %>% select(starts_with("out"))
  out_logical <- out["out"] == 0
  logical_locate <- grep(FALSE, out_logical)
  factor_value_name <- colnames(factor_value)
  factor_value_name_extract <- factor_value_name[logical_locate]
  out <- out[!out_logical,]
  
  factor_value <-  factor_value[1,  factor_value_name_extract]
  # built direct input coefficient matrix, dic
  factorcoe <- tibble_over( factor_value, out)
  return (factorcoe)
}

factor_coefficience <- unitcountryav(analydb, "CHN")
factor_coefficience <- as.data.frame(t(factor_coefficience))
factor_coefficience["rowfactor"] <- rownames(factor_coefficience)
factor_coefficience <- as.tibble(factor_coefficience)

direct_consumption  <- unitcountrydc(analydb, "CHN")
final_demand_db <- final_demand(analydb, "CHN")
final_demand_db <- final_demand_db %>% select(fin_demand)
complete_consumption <- solve(diag(rep(1, dim(direct_consumption)[1])) - direct_consumption)
export_out <- as.matrix(complete_consumption) %*% as.matrix(final_demand_db)
export_out <- as.data.frame(export_out)
export_out["rowfactor"] <- rownames(export_out)
export_out <- export_out %>% as.tibble() %>% select(rowfactor, everything())

colnames(complete_consumption) <- rownames(complete_consumption)
write_csv(as.tibble(complete_consumption), paste(filedir, "wiot_chn.csv", sep=""))
write_csv(export_out, paste(filedir, "wiot_chn_ef.csv", sep=""))
write_csv(factor_coefficience, paste(filedir, "wiot_chn_av.csv", sep=""))
