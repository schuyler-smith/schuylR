#' classify_ARG_codes
#'
#' This function checks if the rows in a dataframe are contained in the other.
#' Generally most useful for testing if the outputs of 2 functions are =.
#' @usage classify_ARG_codes(codes)
#' @param codes vector or dataframe column of ARG codes.
#' @import stringr
#' @export
#' 
classify_ARG_codes <- function(codes){
  arg_classes <- list(Aminocoumarins = c("AAC","bae","mdt","nov","cpx"),
                      Aminoglycosides = c("aad","amr","ANT","APH","arm","Borrelia","mdf","npm","Pasteurella","Pvr",
                                          "rmt","spd","apm","opc"),
                      Betalactams = c("ACC","ACI","ACT","AER","AIM","AQU","BcI","BcII","BEL","BJP","BLA","CARB","CAU","Cbl",
                                      "Ccr","cep","CFE","Cfx","CGB","CMY","cph","CPS","CTX","DHA","DIM","EBR","ESP",
                                      "EXO","FEZ","FOX","GES","GOB","IMI","imi","IMP","IND","L1","KPC","LAT","LCR",
                                      "LEN","LRA","mec","MIR","MOX","MSI","MUS","NDM","Nmc","NPS","OCH","OKP","OXA",
                                      "PBP","PC1","PDC","PED","PER","r39","ROB","Sed","SFB","sfh","SHV","SIM","SLB",
                                      "SMB","spg","SPM","SRT","TEM","TLA","TLE","TUS","VCC","VEB","VIM","y56","sme","Rm3",
                                      "Axy","BUT","JOHN","THIN"),
                      Elfamycins = c("Clostridium","fac","Planobispora"),
                      FFA = c("far"),
                      Fluoroquinolones = c("arl","blt","bmr","Campylobacter","Cme","emr","mfp","Mycoplasma","Neisseria",
                                           "nor","Pseudomonas","qac","Qnr","mex","ceo","CRP","lrf","pat"),
                      Fosfomycin = c("Fom","Fos"),
                      Fusidic_acid = c("fus"),
                      Glycopeptides = c("bleomycin","van"),
                      Lipopeptides = c("arn","lpx","MCR","pho","Pmr","ros"),
                      Metronidazole = c("msb"),
                      MLS = c("car","cfr","chr","clb","des","eat","Ere","Erm","gim","lin","lmr","lnu","mac","mef","mel",
                              "mgt","mph","msr","myr","ole","qep","sal","srm","tlr","vat","vga","vgb","lsa","efr","gad",
                              "mtr","tcm"),
                      Multi_drug_resistance = c("acr","opm","Pmp","ram","tol"),
                      Mupirocin = c("mup"),
                      Oxazolidinone = c("optr"),
                      Pleuromutilin = c("Tae"),
                      Peptide = c("Brucella","bac","bcr"),
                      Phenicol = c("cat","cml","cmr","cmx","fex","flo","mds","pp","opr"),
                      Rifampin = c("arr","iri","Rbp","rifampin","efp","rgt","rph"),
                      Streptothricin = c("sat","Str"),
                      Sulfonamides = c("sul"),
                      Tetracyclines = c("ade","mep","mgr","otr","Propionibacterium","tap","tcr","tet","evg","mux","oqx"),
                      Thiostrepton = c(""),
                      Trimethoprim = c("dfr","tri"),
                      Tunicamycin = c("tmr")
  )
  
  codes <- data.frame(codes, stringsAsFactors = FALSE)
  classified_codes <- unlist(unname(apply(codes,1,FUN = function(code){
    if(length(unlist(strsplit(code, ""))) == 4){code <- substr(code,1,3)
    } else {code <- unlist(strsplit(code, split = "(", fixed = TRUE))[1]}
    if(length(unlist(strsplit(code, ""))) == 4){code <- substr(code,1,3)
    } else {code <- unlist(strsplit(code, split = "-", fixed = TRUE))[1]}
    if(length(unlist(strsplit(code, ""))) > 4){code <- substr(code,1,3)}
    
    class <- names(arg_classes)[!(is.na((sapply(arg_classes, FUN = function(class_codes){
      grep(paste0("^",code,"$"), class_codes, ignore.case=TRUE, value=FALSE)
    }) > 0)))]
    if(length(class) > 0){return(class)
    } else {return("Unclassified")}
  })))
  
  # codes <- data.frame(codes, stringsAsFactors = FALSE)
  # classified_codes <- vector()
  # for(i in 1:nrow(codes)){
  #   code <- codes[i,]
  #   if(length(unlist(strsplit(code, ""))) == 4){code <- substr(code,1,3)
  #   } else {code <- unlist(strsplit(code, split = "(", fixed = TRUE))[1]}
  #   if(length(unlist(strsplit(code, ""))) == 4){code <- substr(code,1,3)
  #   } else {code <- unlist(strsplit(code, split = "-", fixed = TRUE))[1]}
  #   if(length(unlist(strsplit(code, ""))) > 4){code <- substr(code,1,3)}
  # 
  #   arg_class <- names(arg_classes)[!(is.na((sapply(arg_classes, FUN = function(class_codes){
  #     grep(paste0("^",code,"$"), class_codes, ignore.case=TRUE, value=FALSE)
  #   }) > 0)))]
  #   
  #   print(arg_class)
  #   
  #   if(length(arg_class) == 0){arg_class <- "Unclassified"}
  #   classified_codes[i] <- arg_class
  # }
  
  return(classified_codes)
}
