
#####################################################################################################################
### INPUTS ##########################################################################################################
#####################################################################################################################

functionsPath = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\pit_tag_data_compile_functions.r"
dataDir = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example"
dbDir = "C:\\Users\\braatenj\\Documents\\GitHub\\pit-tag-data-compile\\example"
timeZone = "America/Los_Angeles"

#####################################################################################################################


source(functionsPath)  
PITcompile(dataDir, dbDir, timeZone)
