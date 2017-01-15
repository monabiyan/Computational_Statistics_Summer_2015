
anes_all <-  read.table("C:/Users/monabiyan/SkyDrive/Summer 2015/Statistics/HW9/data.tsv",sep="\t",header=TRUE,stringsAsFactors=FALSE)
# Keep only the latest year, 2008
anes_2008 <- anes_all[anes_all$VCF0004==2008,]
# Keep only a subset of variables
anes_2008tr <- data.frame(age=anes_2008$VCF0101,race_white=anes_2008$VCF0106,
                          gender_male=anes_2008$VCF0104,education=anes_2008$VCF0140a,
                          income=anes_2008$VCF0114,ideology_con=anes_2008$VCF0803,
                          partyid_rep=anes_2008$VCF0301,vote_rep=anes_2008$VCF0704a,
                          voted=anes_2008$VCF0702)
head(anes_2008tr)
anes_2008tr$race_white[anes_2008tr$race_white != 1] <- 0 # non-white to 0 
anes_2008tr$gender_male[anes_2008tr$gender_male == 2] <- 0 # non-male to 0
anes_2008tr$education[anes_2008tr$education > 7] <- 3 # DK/NA to middle
anes_2008tr$income[anes_2008tr$income == 0] <- 3 # DK/NA to middle
anes_2008tr$ideology_con[anes_2008tr$ideology_con == 9] <- 4 # DK to middle
anes_2008tr$ideology_con[anes_2008tr$ideology_con == 0] <- 4 # NA to middle
anes_2008tr$partyid_rep[anes_2008tr$partyid_rep == 0] <- 4 # DK/NA to middle
anes_2008tr$vote_rep <- anes_2008tr$vote_rep - 1 # 1/2 -> 0/1
anes_2008tr$vote_rep[anes_2008tr$vote_rep == -1] <- NA # DK/NA to NA
anes_2008tr$voted <- anes_2008tr$voted - 1 # 1/2 -> 0/1
anes_2008tr$voted[anes_2008tr$voted == -1] <- 0 # DK/NA to 0

write.table(anes_2008tr,"C:/Users/monabiyan/SkyDrive/Summer 2015/Statistics/HW9/anes_2008tr.csv",sep=",",row.names=FALSE)
anes_2008tr <- read.table("anes_2008tr.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)

bv1 <- lm(ideology_con ~ education,data=anes_2008tr)
summary(bv1)


mr1 <- lm(ideology_con ~ education + income,data=anes_2008tr)
summary(mr1)


mr2 <- lm(ideology_con ~ age + gender_male + race_white + 
            education + income,data=anes_2008tr)
summary(mr2)

mr3 <- lm(ideology_con ~ age + gender_male + race_white + 
            education + income + partyid_rep,data=anes_2008tr)
summary(mr3)


