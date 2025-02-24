library(readxl)
library(taxotools)

prep <- read_excel("input/NAOC_preparations_mappings (1).xlsx",
                   sheet = "NAOC_preparations")
oldnames <- names(prep)
names(prep) <- c("Participant","prep","body_part","prep_type","language","noc","notes")
prep$slno <- seq(1:nrow(prep))
prep$oprep <- prep$prep

library(taxotools)

prep1 <- NULL
pb = txtProgressBar(min = 0, max = nrow(prep), initial = 0)
for(i in 1:nrow(prep)){
  #for(i in 1:100){
  rec <- prep[i,]
  if(grepl( "|", rec$prep, fixed = TRUE)){
    rec <- melt_cs_field(rec,"prep","|")
  } else   if(grepl( ";", rec$prep, fixed = TRUE)){
    rec <- melt_cs_field(rec,"prep",";")
  } else   if(grepl( ",", rec$prep, fixed = TRUE)){
    rec <- melt_cs_field(rec,"prep",",")
  } else   if(grepl( "+", rec$prep, fixed = TRUE)){
    rec <- melt_cs_field(rec,"prep","+")
  }
  prep1 <- rbind(prep1,rec)
  setTxtProgressBar(pb,i)
}

library(readr)
body_lookup <- read_csv("input/body_lookup.csv")

body_lookup <- body_lookup[!is.na(body_lookup$name),]
prep1_backup <- prep1
prep1$body_part_a <- NA
prep1$prep_type_a <- NA
pb = txtProgressBar(min = 0, max = nrow(prep1), initial = 0)
#for(i in 1:1000){
for(i in 1:nrow(prep1)){
  for(j in 1:nrow(body_lookup)){
    if(grepl( body_lookup$name[j], prep1$prep[i], fixed = TRUE)){
      addval <-  body_lookup$value[which(body_lookup$name==body_lookup$name[j])]
      prep1$body_part_a[i] <- ifelse(is.na(prep1$body_part_a[i]),
                                     addval,
                                     paste(prep1$body_part_a[i],"|",addval))
    } else {
    }
  }
  setTxtProgressBar(pb,i)
}





library(readr)
prep_lookup <- read_csv("input/prep_lookup.csv")

prep_lookup <- prep_lookup[!is.na(prep_lookup$name),]
pb = txtProgressBar(min = 0, max = nrow(prep1), initial = 0)
#for(i in 1:1000){
for(i in 1:nrow(prep1)){
  for(j in 1:nrow(prep_lookup)){
    if(grepl( prep_lookup$name[j], prep1$prep[i], fixed = TRUE)){
      addval <-  prep_lookup$value[which(prep_lookup$name==prep_lookup$name[j])]
      prep1$prep_type_a[i] <- ifelse(is.na(prep1$prep_type_a[i]),
                                     addval,
                                     paste(prep1$prep_type_a[i],"|",addval))
    } else {
    }
  }
  setTxtProgressBar(pb,i)
}

prep2_1 <- cast_cs_field(data=prep1,
                         pri = "slno",
                         sec = "body_part_a",
                         sepchar = " |",
                         duplicate = TRUE)

prep2_2 <- cast_cs_field(data=prep1,
                         pri = "slno",
                         sec = "prep_type_a",
                         sepchar = " |",
                         duplicate = TRUE)
prep2 <- prep2_1
prep2$prep_type_a <- prep2_2$prep_type_a

prep3 <- prep2[,c("slno", "oprep", "noc","body_part_a", "prep_type_a")]
prep3$slno <- as.numeric(prep3$slno)
write.csv(prep3,"prep_map1.csv",row.names = F)


