require(RODBC)


AFSC <- odbcConnect("AFSC","mkapur","N5w!Pw4mkq",  believeNRows = FALSE)

AKFIN <- odbcConnect("AKFIN","mkapur","ssmamk22",  believeNRows=FALSE) ## need login/pw from Rob


afsctab <- sqlTables(AFSC)

afsctab[grep('LENGTH',afsctab$TABLE_NAME),]



afsc.sql <- "SELECT * FROM OBSINT.DEBRIEFED_LENGTH WHERE ROWNUM = 1" ## this view no longer exists

(afsc.test <- sqlQuery(AFSC, afsc.sql))

afsctab <- sqlTables(AKFIN)

akfin.sql <-
  
  "SELECT * FROM COUNCIL.COMPREHENSIVE_BLEND_CA WHERE ROWNUM = 1"

(akfin.test <- sqlQuery(AKFIN, akfin.sql))

