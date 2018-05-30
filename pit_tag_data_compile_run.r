
#####################################################################################################################
### INPUTS ##########################################################################################################
#####################################################################################################################

functionPath = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\pit_tag_data_compile_functions.r"
DBfile = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\pitTagDB.csv"
inputFileList = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example\\fileList1.csv"
timeZone = "America/Los_Angeles"

#####################################################################################################################



source(functionPath)  
PITcompile(DBfile, inputFileList, timeZone)
