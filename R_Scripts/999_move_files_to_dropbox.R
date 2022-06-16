here("Plots", list.files("Plots"))
#### Move Graphs over to Dropbox####
#Move all graphs over to Results folder
file.copy(here("Plots", list.files("Plots")), to="~/Dropbox/Public_Health/Results")
#Move CJPH Plots over to CJPH subfolder
cjph_plots<-here("Plots", list.files("Plots"))
file.copy(cjph_plots[str_detect(cjph_plots, "cjph")], to="~/Dropbox/Public_Health/CJPH/Plots", overwrite=T)

#Move CJPH Tables over
cjph_tables<-here("Tables", list.files("Tables"))
cjph_tables
cjph_tables[str_detect(cjph_tables, "cjph")]
file.copy(cjph_tables[str_detect(cjph_tables, "cjph")], to="~/Dropbox/Public_Health/CJPH/Tables", overwrite=T)
#### Move Recoded Data File ####


#file.copy(here('data', str_extract(list.files(path="data"), "^recoded_data.+[0-9].sav?")), to="~/Dropbox/Public_Health/Data")

