#ROW AND COLUMN DATA FILTERING SCRIPT FOR IMPUTATION



#Description------------------------------

#This script/Function receives data in form of a xlsx, data frame or a matrix and it filters
#for missing data for both row and column and it recommends the adoption of the
#one with the lowest percentage data loss of both methods


#Directory -------------------------

#Set the data working directory/data location.. Kindly edit the one i used below
setwd("C:/Users/1030 G2/Desktop/SOSE 2022/Breeding Informatic/Row and colmn omit script")

#kindly install if you do not have this before

install.packages("openxlsx")
library(openxlsx) #load library


#Read in data-----------------------------

#Read in the data using the function read.xlsx from the installed package if the
#data is in excel format. If not you could read in as matrix or data table.

(input <- read.xlsx("GenoMediumWithNA.xlsx", sheet
                    = 1))

#Take a look at the head.
(head(input)) 

#NOTE - Only numeric data is needed so we need to remove non-numeric ones



#FUNCTION ------------------------
Row_and_column_Filtering <- function(x) {
  dimension <- prod(dim(x))
  y <- t(na.omit(t(x)))
  print(head(y))
  
  #column filtering percentage
  No_removed <- ncol(t(x)) * length(na.action(y))
  total_SNP <-  dimension
  col_percentage_missing <- (No_removed / total_SNP) * 100
  cat(col_percentage_missing,
      "% of data is lost when column filtering is used")
  
  
  cat("\n")
  
  #row filtering percentage
  No_of_row_removed <- nrow(t(x)) * length(na.action(na.omit(x)))
  row_percentage_missing <- (No_of_row_removed / total_SNP) * 100
  cat(row_percentage_missing,
      "% of data is lost when row filtering is used")
  
  
  cat("\n")
  
  if (row_percentage_missing <= min(col_percentage_missing, row_percentage_missing)) {
    cat(
      "It is advisable to make use of row filtering for this data as the loss of SNP data points is minimal"
    )
  } else {
    cat("It is advisable to filter this data by column as it minimizes the loss of SNP data points")
  }
}


#Using the Function ------------------------

Row_and_column_Filtering(input[, -1])

#Kindly note that the -1 index here is to remove the column with non numeric data in the test data used.
#This would not be needed if the data contains ONLY numeric data on SNP
