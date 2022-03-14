# This script prepares the municpality data obtained from
# the BFS for use in an exercise

library(tidyverse)
library(readxl)
library(WriteXLS)
library(waldo)

tmpdir <- tempfile("mundata")
dir.create(tmpdir)


# prepare the data #####

# download the file
file <- file.path(tmpdir, "municipalities_orig.xlsx")
download.file("https://www.bfs.admin.ch/bfsstatic/dam/assets/7786544/master",
              file)

# read the file. Column titles are split into multiple columns and
# must be read and prepared separately
titles <- read_excel(file, col_names = FALSE, skip = 3, n_max = 4)
raw_data <- read_excel(file, col_names = FALSE, skip = 9, n_max = 2212,
                       na = c("", "X"))

# create table with (more or less) useable names
tc <- lapply(1:nrow(titles), function(i) as.character(titles[i, ]))
names <- paste(
  if_else(is.na(tc[[1]]), "", tc[[1]]),
  if_else(is.na(tc[[3]]), "", tc[[3]])
  ) %>%
  str_trim() %>%
  str_replace_all("\\s+", "_") %>%
  make.unique()
data_all <- set_names(raw_data, names)


# select a few columns and municipalities
set.seed(1)
data <- sample_n(data_all, 100) %>%
          select(Gemeindecode,
                 Gemeindename,
                 Einwohner = Bevölkerung_Einwohner,
                 Bevoelkerungsdichte = "Bevölkerungs-dichte_pro_km²",
                 Anzahl_Privathaushalte = "Haushalte_Anzahl_Privathaushalte",
                 Neu_gebaute_Wohnungen_pro_1000_Einwohner,
                 Anzahl_Beschaeftigte = "Wirtschaft_2)_Beschäftigte_total",
                 Anzahl_Arbeitsstaetten = Arbeitsstätten_total)

# for some reason, R does not realise that the names are UTF-8-encoded...
# This has no noticeable consequences except that compares() does not
# realise that the columns are the same => fix it.
Encoding(names(data)) <- "UTF-8"

# write the files #####

# file1: Excel file without problems
WriteXLS(list(Sheet1 = data), file.path(tmpdir, "file1.xlsx"), AdjWidth = TRUE)

# read
test1 <- read_excel(file.path(tmpdir, "file1.xlsx"))
compare(data, test1)

# file2: csv file without problems
write_csv(data, file.path(tmpdir, "file2.csv"))

# read
test2 <- read_csv(file.path(tmpdir, "file2.csv"))
compare(data, test2)

# file3: csv with delimiter ;
write_delim(data, file.path(tmpdir, "file3.csv"), delim = ";")

# read
test3 <- read_delim(file.path(tmpdir, "file3.csv"), delim = ";")
compare(data, test3)

# file4: csv with missing column in a row
lines <- read_lines(file.path(tmpdir, "file2.csv"))
lines[75] <- str_replace(lines[75], ",[0-9]+$", "")
write_lines(lines, file.path(tmpdir, "file4.csv"))

# read
test4 <- read_csv(file.path(tmpdir, "file4.csv"))
compare(data, test4)
i_bad <- which(is.na(test4$Anzahl_Arbeitsstaetten))
test4$Anzahl_Arbeitsstaetten[i_bad] <- data$Anzahl_Arbeitsstaetten[i_bad]
compare(data, test4)


# file5: csv with aditional separator in a row
lines <- read_lines(file.path(tmpdir, "file2.csv"))
lines[25] <- str_replace(lines[25], "\\.", ",")
write_lines(lines, file.path(tmpdir, "file5.csv"))

# read
test5 <- read_csv(file.path(tmpdir, "file5.csv"))
compare(data, test5)
i_bad <- which(test5$Anzahl_Arbeitsstaetten != data$Anzahl_Arbeitsstaetten)
test5[i_bad, 4:8] <- data[i_bad, 4:8]
compare(data, test5)


# file6: csv file with bad name
write_csv(data, file.path(tmpdir, "file6.xlsx"))

# read
test6 <- read_csv(file.path(tmpdir, "file6.xlsx"))
compare(data, test6)


# file7: Excel file with spaces in headers
WriteXLS(list(Sheet1 = set_names(data, str_replace_all(names(data), "_", " "))),
         file.path(tmpdir, "file7.xlsx"), AdjWidth = TRUE)

# read
test7 <- read_excel(file.path(tmpdir, "file7.xlsx"))
compare(data, test7)
fix_names <- function(x) gsub("\\s+", "_", x)
test7 <- read_excel(file.path(tmpdir, "file7.xlsx"), .name_repair = fix_names)
compare(data, test7)


# file8: csv file with Window-1252 encoding

write.table(data, file.path(tmpdir, "file8.csv"),
            sep = ",",
            fileEncoding = "Windows-1252",
            row.names = FALSE)

# read
test8 <- read_csv(file.path(tmpdir, "file8.csv"))
compare(data, test8)
guess_encoding(file.path(tmpdir, "file8.csv"))
test8 <- read_csv(file.path(tmpdir, "file8.csv"),
                  locale = locale(encoding = "Windows-1252"))
compare(data, test8)
test8 <- read_csv(file.path(tmpdir, "file8.csv"),
                  locale = locale(encoding = "ISO-8859-1"))
compare(data, test8)


# create zip file with all the input files #####
zip(file.path(tmpdir, "files.zip"),
    list.files(tmpdir, "^file\\d\\.(xlsx|csv)", full.names = TRUE),
    flags = "-j9X")

# copy to inst/extdata
usethis::use_directory("inst/extdata")
file.copy(file.path(tmpdir, "files.zip"),
          file.path(usethis::proj_path(), "inst", "extdata"))
