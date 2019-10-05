# Javad Khataei 
# 10-05-2019
# find identity perwise

library(tidyverse)
library(data.table)

########################### Data Prep ############################
# Read and make a clean data set
df <- fread("60_teleosts_nt_aligned.fasta" , header = F)

# seperate names and sequences
n_row <- nrow(df)
row_index <- seq(1,n_row-1,2)
seq_index <- seq(2, n_row,2)
df_rows <- df[row_index]
df_rows <- gsub(pattern = ">",replacement = "",df_rows$V1) %>%  as.data.frame()
df_seq <- df[seq_index]
# convert a single column of strings to multiple columns
df_seq <- data.frame(str_split(string = df_seq$V1,pattern = "") %>% 
             as.vector()) %>% 
  data.table::transpose()

df <- data.frame(df_rows,df_seq)
colnames(df) <- c("species" , paste0("p",1:654))

###################### End Data Prep #########################

n_species <- nrow(df)
n_seq <- ncol(df) - 1 # first one is the name
mt_result <- matrix(nrow = n_species, ncol = n_species)
colnames(mt_result) <- df$species
row.names(mt_result) <- df$species

for (first_spe in 1:n_species ) {
  for (second_spe in first_spe:n_species) {
    ls_compare <- df[first_spe,] == df[second_spe,] 
    n_true <- ls_compare[ls_compare == TRUE] %>% length()
    mt_result[first_spe, second_spe] <-  round(x = (n_true / n_seq *100), digits = 2)
    mt_result[second_spe, first_spe] <- mt_result[first_spe, second_spe]
  }
  mt_result[first_spe, first_spe] <- 100 # fill the diagonal
}

fwrite(x = data.frame(mt_result), "nt-identity-results.csv", row.names = TRUE)
