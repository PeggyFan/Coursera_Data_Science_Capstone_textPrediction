### global.R

## dictionary
my_dict2 <- read.table("./data/us_dict.txt", sep=" ")
my_dict2 <- as.character(my_dict2$V1)

stemCompletion_mod <- function(x,dict=my_dict2) {
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" ")))
}

##
#n1_table <- load("data/n1_table")
#n2_table <- load("data/n2_table")
#n3_table <- load("data/n3_table")
#n4_table <- load("data/n4_table")

#n2_SGT_DT  <- load("data/n2_SGT_DT")
#n3_SGT_DT  <- load("data/n3_SGT_DT")
#n4_SGT_DT  <- load("data/n4_SGT_DT")

new_table1  <- readRDS("data/new_table1F.Rds")
new_table2  <- readRDS("data/new_table2F.Rds")
new_table3  <- readRDS("data/new_table3F.Rds")
new_table4  <- readRDS("data/new_table4F.Rds")

##
n1_table <- readRDS("data/n1_tableF.Rds")
n2_table <- readRDS("data/n2_tableF.Rds")
n3_table <- readRDS("data/n3_tableF.Rds")
n4_table <- readRDS("data/n4_tableF.Rds")

n2_SGT_DT <- readRDS("data/n2_SGT_DT.Rds")
n3_SGT_DT <- readRDS("data/n3_SGT_DT.Rds")
n4_SGT_DT <- readRDS("data/n4_SGT_DT.Rds")



