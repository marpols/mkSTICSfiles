
makeFiles <- function(filepath, 
          addExt = T,
          save2csv = T,
          outdir = NULL){
  #read sheets from excel. update 
  usms <- read_excel(filepath, sheet = "usms")
  init <- read_excel(filepath, sheet = "init")
  soils <- read_excel(filepath, sheet = "sol")
  tec <- read_excel(filepath, sheet = "tec")
  sta <- read_excel(filepath, sheet = "sta")
  
  if(addExt){
    init$Ini_name <- paste(init$Ini_name, "_ini.xml", sep="")
    tec$Tec_name <- paste(tec$Tec_name, "_tec.xml", sep="")
    sta$Sta_name <- paste(sta$Sta_name,"_sta.xml",sep="")
    
    usms <- updtExtUSM(usms)

  }
  
  files <- list(usms = usms,
                init = init,
                sol = soils,
                tec = tec,
                sta = sta
                )
  funcs <- list(gen_usms_xml,
                gen_ini_xml,
                gen_sols_xml,
                gen_tec_xml,
                gen_sta_xml
                )
  
  if(save2csv){
    #save to csv files in R project folder
  mapply(function(f,n){
    write.csv(f,sprintf("files/%s.csv",n), row.names = F, quote = F)
  }, files, names(files))
  }
  
  if(is.null(outdir)){
    outdir = "files/"
  }
  
  mapply(function(f,fnc){
    do.call(fnc,c(f,outdir))
  }, files, funcs)
}

makeUsm <- function(filepath, 
                     addExt = T,
                     save2csv = T,
                     outdir = NULL){
  
  usms <- read_excel(filepath, sheet = "usms")
  
  if(addExt){
    usms <- updtExtUSM(usms)
  }
  
  if(save2csv){
    write.csv(usms,"files/usms.csv", row.names = F, quote = F)
  }
  
  if(is.null(outdir)){
    outdir = "files/"
  }
  
  gen_usms_xml(param_df = usms, out_dir = outdir)
  
}

updtExtUSM <- function(usmf){
  usmf[which(is.na(usmf$finit)),4] <- paste(usmf$usm_name[which(is.na(usmf$finit))],
                                            "_ini.xml", sep="")
  usmf$fclim1 <- paste(usmf$fstation,usmf$fclim1, sep=".") 
  usmf$fclim2 <- paste(usmf$fstation,usmf$fclim2, sep=".") 
  usmf$fstation <- paste(usmf$fstation,"_sta.xml",sep="")
  usmf
} 
