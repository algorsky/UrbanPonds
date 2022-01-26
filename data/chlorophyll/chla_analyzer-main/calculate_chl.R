for (i in 1:length(chlfiles_acid)) {
    
    chl_acid <- chlfiles_acid[i]
    chl_pre <- chlfiles_pre[i]
    
    
    #parse data
    data_chl_acid <- read.csv(paste0("raw_data/",chl_acid))
    skip_number <- which(data_chl_acid$X.1 == "Abs 2")
    id_start <- which(data_chl_acid$X == "Sample Name")
    
    #get sample ids
    data_chl_acid_id <- data_chl_acid[(id_start+1):(skip_number-2), c(1,2,4)]
    colnames(data_chl_acid_id) <- c("runid","sample_id","vol")
    data_chl_acid_id$runid <- paste0("Abs.",data_chl_acid_id$runid)
    data_chl_acid_id <- data_chl_acid_id %>% 
        mutate(vol = as.numeric(vol)/1000) %>% 
        mutate(acid_max = NA) %>% 
        arrange(sample_id)
    
    #get data 
    samples <- data_chl_acid_id$sample_id
    data_chl_acid <- read.csv(paste0("raw_data/",chl_acid), skip = skip_number) %>% 
        select("nm",data_chl_acid_id$runid)
    colnames(data_chl_acid) <- c("nm", samples)
    data_chl_acid <- data_chl_acid %>% 
        mutate(diff = nm-floor(nm)) %>% 
        filter(diff==0) %>% 
        select(-diff)
    
    #parse data
    data_chl_pre <- read.csv(paste0("raw_data/",chl_pre))
    skip_number <- which(data_chl_pre$X.1 == "Abs 2")
    id_start <- which(data_chl_pre$X == "Sample Name")
    
    #get sample ids
    data_chl_pre_id <- data_chl_pre[(id_start+1):(skip_number-2), c(1,2,4)]
    colnames(data_chl_pre_id) <- c("runid","sample_id","vol")
    data_chl_pre_id$runid <- paste0("Abs.",data_chl_pre_id$runid)
    data_chl_pre_id <- data_chl_pre_id %>% 
        mutate(vol = as.numeric(vol)/1000) %>% 
        mutate(pre_max = NA) %>% 
        arrange(sample_id)
    
    #get data 
    data_chl_pre <- read.csv(paste0("raw_data/",chl_pre), skip = skip_number) %>% 
        select("nm",data_chl_pre_id$runid)
    samples <- data_chl_pre_id$sample_id
    colnames(data_chl_pre) <- c("nm", samples)
    data_chl_pre <- data_chl_pre %>% 
        mutate(diff = nm-floor(nm)) %>% 
        filter(diff==0) %>% 
        select(-diff)
    
    #Check to ensure that sample information matches between pre and post acid files 
    if(compare(data_chl_acid_id[,c(2:3)],data_chl_pre_id[,c(2:3)])$result==FALSE) {
        stop("Sample information doesn't match")
    }
    #generate flags if peaks are outside of 660 - 670 nm
    dat.max <- data_chl_pre_id[,1:2]
    dat.max$pre <- NA
    maxvalues <- data_chl_pre[,(2:ncol(data_chl_pre))]
    rownames(maxvalues) <- data_chl_pre[,1]
    for(z in 1:ncol(maxvalues)) {
        max_ab <- which(maxvalues[,z]==max(maxvalues[,z]))
        if(colnames(maxvalues)[z] != "BLA"){
            if(length(max_ab) == 1) {
                dat.max$pre[z] <- rownames(maxvalues)[max_ab]
            } else {
                if(min(as.numeric(rownames(maxvalues)[max_ab])) < 660) dat.max$pre[z] <- rownames(maxvalues)[min(max_ab)]
                if(max(as.numeric(rownames(maxvalues)[max_ab])) > 670) dat.max$pre[z] <- rownames(maxvalues)[max(max_ab)]
                if(min(as.numeric(rownames(maxvalues)[max_ab])) >= 660 & max(as.numeric(rownames(maxvalues)[max_ab])) <= 670) dat.max$pre[z] <- "multiple_peaks"
            }
            if(min(as.numeric(rownames(maxvalues)[max_ab])) < 660 | max(as.numeric(rownames(maxvalues)[max_ab])) > 670 ) {data_chl_pre_id$pre_max[z] <- "v"}
        }
        # print(min(as.numeric(rownames(maxvalues)[max_ab])))
    }
    dat.max$post <- NA
    maxvalues <- data_chl_acid[,(2:ncol(data_chl_acid))]
    rownames(maxvalues) <- data_chl_acid[,1]
    for(z in 1:ncol(maxvalues)) {
        max_ab <- which(maxvalues[,z]==max(maxvalues[,z]))
        if(colnames(maxvalues)[z] != "BLA"){
            if(length(max_ab) == 1) {
                dat.max$post[z] <- rownames(maxvalues)[max_ab]
            } else {
                if(min(as.numeric(rownames(maxvalues)[max_ab])) < 660) dat.max$post[z] <- rownames(maxvalues)[min(max_ab)]
                if(max(as.numeric(rownames(maxvalues)[max_ab])) > 670) dat.max$post[z] <- rownames(maxvalues)[max(max_ab)]
                if(min(as.numeric(rownames(maxvalues)[max_ab])) >= 660 & max(as.numeric(rownames(maxvalues)[max_ab])) <= 670) dat.max$post[z] <- "multiple_peaks"
            }
            if(min(as.numeric(rownames(maxvalues)[max_ab])) < 660 | max(as.numeric(rownames(maxvalues)[max_ab])) > 670 ) {data_chl_acid_id$acid_max[z] <- "v"}
        }
        # print(min(as.numeric(rownames(maxvalues)[max_ab])))
    }
    
    
    
    dat_pre <- data_chl_pre %>% 
        filter(nm == 665 | nm==666 | nm == 653 | nm == 750) %>% 
        mutate(nm = paste0("nm_",nm)) %>% 
        pivot_longer(-nm) %>% 
        pivot_wider(name, names_from="nm", values_from="value") %>%
        rename(Type=name) %>% 
        mutate(nm_750 = if_else(nm_750<0,0,nm_750))
    
    dat_post <- data_chl_acid %>% filter(nm == 665 | nm == 750) %>% 
        mutate(nm = paste0("nm_",nm)) %>% 
        pivot_longer(-nm) %>% 
        pivot_wider(name, names_from="nm", values_from="value") %>%
        rename(Type=name) %>% 
        mutate(nm_750 = if_else(nm_750<0,0,nm_750))
    
    dat.max <- dat.max %>% 
        mutate(pre750 = dat_pre$nm_750) %>% 
        mutate(post750 = dat_post$nm_750) %>% 
        mutate(file = chl_pre)
    
    c_a <- 15.65*(dat_pre$nm_666-dat_pre$nm_750) - 7.34*(dat_pre$nm_653-dat_pre$nm_750) 
    #formula from: Lichtenthaler & Wellburn YEAR Determinations of total caroteniods and chlorophyll a and be of leaf extracts in different solvents
    c_b <- 27.05*(dat_pre$nm_653-dat_pre$nm_750) - 11.21*(dat_pre$nm_666-dat_pre$nm_750)
    #formula from: Lichtenthaler & Wellburn YEAR Determinations of total caroteniods and chlorophyll a and be of leaf extracts in different solvents
    cc_a <- ((dat_pre$nm_665-dat_pre$nm_750)-(dat_post$nm_665-dat_post$nm_750)) * (1.56/.56) * (1000/75)
    #Formula: from Sartory 1982 Spectrophomatic analysis of chlorophyll a in freshwater phytoplankton
    #Acid Ratio from:
    #Absorbance Coefficient from:
    p_a <- (1.56 * (dat_post$nm_665-dat_post$nm_750) - (dat_pre$nm_665-dat_pre$nm_750)) * (1.56/.56) * (1000/75)
    #Formula: from Sartory 1982 Spectrophomatic analysis of chlorophyll a in freshwater phytoplankton
    #Acid Ratio from:
    #Absorbance Coefficient from:
    
    dat_chl <- data_chl_pre_id[,c(2,3)] %>% 
        mutate(chla = round((c_a*0.01)/(vol*1)*1000,digits = 1)) %>% 
        mutate(chlb = round((c_b*0.01)/(vol*1)*1000,digits = 1)) %>% 
        mutate(chla_correct_ugl = round((cc_a*0.01)/(vol*1)*1000,digits = 1)) %>% 
        mutate(phaeophytin = round((p_a*0.01)/(vol*1)*1000,digits = 1)) %>% 
        mutate(bad = pmax(dat_pre$nm_750,dat_post$nm_750)) %>% 
        mutate(flag_turb = ifelse(bad >0.005,"k","")) %>% 
        select(-bad) %>% 
        mutate(pre_max = data_chl_pre_id$pre_max) %>% 
        mutate(post_max = data_chl_acid_id$acid_max) %>% 
        mutate(file = chl_pre)
    
    
    
    if(file.exists(paste0("processed_data/",output_file)) == FALSE) {
        write.csv(x = dat_chl,file = paste0("processed_data/",output_file),row.names = FALSE,na="")
        write.csv(x = dat.max,file = paste0("processed_data/peaks_",output_file),row.names = FALSE,na="")
    } else {
        write.table(dat_chl, paste0("processed_data/",output_file), sep = ",", col.names = !file.exists(paste0("processed_data/",output_file)),append = T,row.names = FALSE,na="")
        write.table(dat.max, paste0("processed_data/peaks_",output_file), sep = ",", col.names = !file.exists(paste0("processed_data/",output_file)),append = T,row.names = FALSE,na="")
    }  
} 
