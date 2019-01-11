# this code chunk allows us to load the data in a programmatic way and also to have intermediate code that will be useful for later. For example, the file_name function can use the name of an object and automatically create file names for saving them for later use.
#
data_table_list <- list("catalog", "customers", "order_lines", "orders")

file_name <- function(t, ex = ".xls") {
   # generate a character vector by pasting the table or object name and a file extension designation, with a default value of ".xls"
   as.character(paste0(t, ex))}

# use the `data_table_list` and our function to create a list of the file paths and then import them to create a master list where each element is a data frame object that we will want to work with
all_data <- map(data_table_list, file_name) %>% map(read_excel)

# then make a data frame where the data_table_list is the names of the tables and the tables are contained in a list column called `data`
df <- tibble(
   table_name = as.character(data_table_list),
   data = all_data)

# finally, we can now unpack those tables and break them out into separate objects
for(i in seq_along(df$table_name)) {
   df[i,1]
}
```