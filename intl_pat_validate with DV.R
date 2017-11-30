#Now we want to see the overlap of intl patent authors with US authors. 
install.packages("dbConnect")
library(dbConnect)

con<-dbConnect(MySQL(),user="GenBankUser", password="123GenBank456",host="metadatalab.syr.edu", dbname="genbank")
test<-dbGetQuery(con, "SELECT COUNT(*), r.* FROM test.par_PatentReference as r 
                 WHERE patenttype NOT LIKE 'US' 
                 GROUP BY patentNumber
                 LIMIT 1000000 ;")

t<-table(test$`COUNT(*)`)
#View(t)

u<-test[which(test$`COUNT(*)`>10000),]
#View(head(unique.auths))

unique.auths<-test[!duplicated(test$`ReferencePatent$authors`) ,]
#nrow(unique.auths)

#Format authors so we have one author per line: TRANSPOSE TIME.
library(reshape2)
N_test<-f[,c(1:51)]
N_long = melt(N_test, id.vars=c("row_names"))
View(head(N_long))
rm(N_test)

#cleaning
library(splitstackshape)
a=cSplit(N_long, "value", sep=",")
View(head(a))
a$value_2<-substr(a$value_2,1,1)
a$variable<-gsub("Author", "", a$variable)
max(a$variable)
colnames(a) = c("row_names","seq","Lname","Fname")

a=as.data.frame(a)
a<-a[,c("row_names","Lname","Fname","seq")]
#View(head(a))


#### Connect and Compare DV authors to Intl patent authors by grep last name first initial
refPatAuthors <- mutate_all(a, funs(tolower))
refPatAuthors$name <- paste(refPatAuthors$Lname, refPatAuthors$Fname, sep=",")
View(head(refPatAuthors))

b = read.csv("dataverse_unique_authors.csv")
dataverseAuthors <- mutate_all(b, funs(tolower))
dataverseAuthors$name <- paste(dataverseAuthors$Lname, dataverseAuthors$Fname, sep=",")
colnames(dataverseAuthors)
dataverseAuthors<-dataverseAuthors[,c(2:6)]
colnames(dataverseAuthors)[5]<-"name"

View(head(dataverseAuthors))

refPat = as.data.frame(refPatAuthors)
class(refPat)

dataverse = as.data.frame(dataverseAuthors)
class(dataverse)

df<-merge(x = refPat, y = dataverse, by = "name", all.x = TRUE) #7,884,257 
df.all<-df<-merge(x = refPat, y = dataverse, by = "name") #3,727,914 

unique.auths<-df.all[!duplicated(df.all$name) ,] #There are 30628 matches.

unique.A<-refPat[!duplicated(refPat$name) ,]
#Of the international patent authors 30628/96627, 31.6% (about a third, h'okay!)

#View(head(refPat))
result.intl.PatAuthors.match<-df.all

write.csv(result.intl.PatAuthors.match, "result.intl.PatAuthors.match.csv")

colnames(dataverseAuthors)
colnames(refPatAuthors)
View(head(df.all))
##########################################################################



