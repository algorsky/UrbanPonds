#get list of files to process
chlfiles <- list.files(path = "raw_data/")

#get seperate out pre and post acid files
chlfiles_pre <- chlfiles[seq(1,length(chlfiles),2)]
chlfiles_acid <- chlfiles[seq(2,length(chlfiles),2)]

#check to make sure all files exist
if(length(chlfiles_pre) != length(chlfiles_acid)) {
    stop("Missing pre or post files")
}
