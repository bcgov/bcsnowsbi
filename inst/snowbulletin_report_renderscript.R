# Script for rendering snow bulletin R markdown product
# Also includes the SBI function that actually calculates the SBI for a particular date.
# Renders all of the necessary graphs for the snow bulletin within a PDF format
# Started by Ashlee Jollymore, 2019
# Note that all of the variables should be set within this document and passed to the RMarkdown file, such that the RMarkdown file shouldn't need any further changes between snow bulletin dates
# ==================

# ==================
# Clean up environment before rendering PDF
rm(list = ls())
# ==================

# ==================
# Script that contains all necessary packages and scripts
# ==================
library(bcsnowdata)
library(bcsnowstats)
library(bcsnowsbi)

# ==================
# Script for rendering tables for the snow bulletin
# ==================
bulletin_date <- "01March2021"
survey_date <- as.Date("01-03-2021", "%d-%m-%Y") # CHANGE HERE ONLY

# Exceptions and manual data corrections

#exceptions_Mar2019 <- c("1A01P", "1E08P", "1E14P", "1D18P", "1D18", "1C05P", "1C14P", "1C40P", "1C29P", "2F08P", "4A03P", "4A20P", "2A18P", "2A30P", "1A12P", #"3A28P", "3A09P", "4A04P", "4A34P", "4A36P", "4A33P", "1A05P")
#exceptions_Apr2019 <- c("1A01P", "1A12P", "1E14P", "1D18P", "1D18", "1C05P", "1C14P", "1C40P", "1C29P", "2F08P", "4A03P", "4A20P", "2A18P", "2A30P", "1A12P", "3A28P", "3A09P", "4A04P", "4A34P", "4A36P", "4A33P", "4A31P", "1A05P", "4C01P")
#exceptions_June2019 <- c("1A01P", "1A05P",
#                        "1E14P", "1D18P", "1D18",
#                        "1C05P", "1C14P", "1C40P", "1C29P","1C38P",
#                        "1F04P2",
#                        "2A18P", "2A30P", "2A31P", "2A32P",
#                        "2F08P", "2F01AP", "2F10P",  # added 2F01A and 2F10P
#                        "3A28P", "3A09P",
#                        "3B24P", "3B26P",
#                        "4A04P", "4A03P", "4A20P", "4A34P", "4A36P", "4A33P", "4A31P",
#                        "4C01P",
#                        "4D10P")

#incorrect_sitesMarch <- c("2D03", "1C17")
#incorrect_dataMarch <- c(285, 280)

incorrect_sitesApril <- NaN
incorrect_dataApril <- NaN

incorrect_sitesMay <- NaN
incorrect_dataMay <- NaN

incorrect_sitesJune <- NaN
incorrect_dataJune <- NaN




# Assign the final variables according to what month it is
exceptions <- c("1D17P") # DOn't use Chilliwack River
incorrect_sites <- NA
incorrect_data <- NA

rmarkdown::render("./inst/SnowBulletin_Tables.Rmd", "pdf_document",
       output_file = paste0("G:/Snow/Statistics/SBI_tables/SnowBulletin_Tables_", bulletin_date, ".pdf"),
       params = list(survey_date = survey_date,
                     exceptions = exceptions,
                     incorrect_sites = incorrect_sites,
                     incorrect_data = incorrect_data)) # pass parameters to the RMarkdown script so you don't have to make any changes to the RMarkdown script itself
