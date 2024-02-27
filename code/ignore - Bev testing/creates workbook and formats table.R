## add formatting to `mtcars` using `wb_dims()`----
wb <- wb_workbook()
wb$add_worksheet("test wb_dims() with an object")
dims_mtcars_and_col_names <- wb_dims(x = mtcars) # "A1:K33"
wb$add_data(x = mtcars, dims = dims_mtcars_and_col_names) # puts mtcars colnames and data in A1

# Put the font as Arial for the data
dims_mtcars_data <- wb_dims(x = mtcars, select = "data") # "A2:K33"
wb$add_font(dims = dims_mtcars_data, name = "Arial")

# Style col names as bold using the special `select = "col_names"` with `x` provided.
dims_column_names <- wb_dims(x = mtcars, select = "col_names") # "A1:K1"
wb$add_font(dims = dims_column_names, bold = TRUE, size = 13)

# Finally, to add styling to column "cyl" (the 4th column) (only the data)
# there are many options, but here is the preferred one
# if you know the column index, wb_dims(x = mtcars, cols = 4) also works.
dims_cyl <- wb_dims(x = mtcars, cols = "cyl") # "B2:B33"
wb$add_fill(dims = dims_cyl, color = wb_color("pink"))

# Mark a full column as important(with the column name too)
wb_dims_vs <- wb_dims(x = mtcars, cols = "vs", select = "x") # "H1:H33"
wb$add_fill(dims = wb_dims_vs, fill = wb_color("yellow"))
wb$add_conditional_formatting(dims = wb_dims(x = mtcars, cols = "mpg"), type = "dataBar")
# wb_open(wb)


wb_save(wb, paste0(dashboard_dataframes_folder, "/", "out_file.xlsx"), overwrite = TRUE)
