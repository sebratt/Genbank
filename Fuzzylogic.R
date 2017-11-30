# Packages:
library(stringdist)
library(reshape2)
library(data.table)
?adist

#########Test run fuzzy function
View(a)
View(b)

#
#fuzzy1 takes two string, to calculate their distances?
x<-"Lebron"
y<-"Steph"

dist.a1b1<-adist(a1,b1, partial = TRUE, ignore.case = TRUE)
distance.methods<-'cosine' #define distance methods
dist.methods<-list()

fuzzy1(x,y)
fuzzy1("bratt","bratt jr")
rbind(fuzzy1("Robert","Roberts"),fuzzy1("bratt","bratt jr"),fuzzy1("bratt","bratt iii"),fuzzy1("James","Jamez"))

adist("bratt","bratt iii")

if(false){
"method='hamming:counts the number of character substitutions that
turns b into a. If a and b have different number of characters the distance is Inf.
method='lv' counts the number of deletions, insertions and substitutions necessary to turn b into a. This method is equivalent to R’s native adist function.

(method='osa') is like the Levenshtein distance but also
allows transposition of adjacent characters. Here, each substring may be edited only once. (For example, a character cannot be transposed twice to move it forward in the string).

(method='dl') is like the optimal string alignment distance
except that it allows for multiple edits on substrings.

***(method=’lcs’) is defined as the longest string that can be obtained
by pairing characters from a and b while keeping the order of characters intact. 
The lcs-distance is defined as the number of unpaired characters. The distance is equivalent to the edit distance allowing only deletions and insertions, each with weight one.
  
method=’qgram’ is a subsequence of q consecutive characters of a string. If x (y) is the
vector of counts of q-gram occurrences in a (b), the q-gram distance is given by the sum over the
  absolute differences jxi 􀀀yij. The computation is aborted when q is is larger than the length of any
  of the strings. In that case Inf is returned.

The cosine distance (method=’cosine’) is computed as 1 􀀀 x  y=(kxkkyk), where x and y were defined above.
  Let X be the set of unique q-grams in a and Y the set of unique q-grams in b. 

  The Jaro distance (method='jw', p=0), is a number between 0 (exact match) and 1 (completely dissimilar) measuring dissimilarity between strings. It is defined to be 0 when both strings have
  length 0, and 1 when there are no character matches between a and b. Otherwise, the Jaro distance is defined as 1 􀀀 (1=3)(w1m=jaj + w2m=jbj + w3(m 􀀀 t)=m). Here,jaj indicates the number
  of characters in a, m is the number of character matches and t the number of transpositions of
  matching characters. The wi are weights associated with the characters in a, characters in b and
  with transpositions. A character c of a matches a character from b when c occurs in b, and the index
  of c in a differs less than max(jaj; jbj)=2 􀀀 1 (where we use integer division) from the index of c in
  b. Two matching characters are transposed when they are matched but they occur in different order
  in string a and b.

method=jw, 0<p<=0.25) adds a correction term to the Jaro-distance.
  It is defined as d  p d, where d is the Jaro-distance. Here, l is obtained by counting, from
  the start of the input strings, after how many characters the first character mismatch between the
  two strings occurs, with a maximum of four. The factor p is a penalty factor, which in the work of
  Winkler is often chosen 0:1.

(method=’soundex’), strings are translated to a soundex code (see phonetic
  for a specification). The distance between strings is 0 when they have the same soundex code,
  otherwise 1. Note that soundex recoding is only meaningful for characters in the ranges a-z and
  A-Z. A warning is emitted when non-printable or non-ascii characters are encountered. Also see
  printable_ascii.
  
  
  "


}



###Fuzzy Function:
fuzzy1 <- function(a,b)
{
  a1 = as.character(a)
  b1 = as.character(b)
  dist.a1b1<-adist(a1,b1, partial = TRUE, ignore.case = TRUE)
  distance.methods<-c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw','soundex') #define methods to calculate the distance of two strings
  
  
  dist.methods<-list()
  
  #Fuzz 1###########
  for(m in 1:length(distance.methods))
  {
    dist.name.enh<-matrix(NA, ncol = length(b1),nrow = length(a1))
    for(i in 1:length(b1)) {
      for(j in 1:length(a1)) { 
        dist.name.enh[j,i]<-stringdist(tolower(b1[i]),tolower(a1[j]),method = distance.methods[m])      
        #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
      }  
    }
    dist.methods[[distance.methods[m]]]<-dist.name.enh
  }
  match.s1.s2.enh<-NULL # what is that?
  for(m in 1:length(dist.methods))
  {
    dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
    min.name.enh<-apply(dist.matrix, 1, base::min)
    for(i in 1:nrow(dist.matrix))
    {
      s2.i<-match(min.name.enh[i],dist.matrix[i,])
      s1.i<-i
      match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=b1[s2.i],s1name=a1[s1.i], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
    }
  }
  
  # Let's have a look at the results
  matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
  Fuzzy1 = matched.names.matrix
  return(Fuzzy1)
}

# Creating a dummy df:######
# Note: you can pass any dataframe here too, just make sure to use the same structure as these ones. Dataframe a should have the columns patent_no, Lname, Fname and seq. Dataframe b should have unique_id, Lname, Fname and seq. Also it is very important to use the same name nomenclature for both the dataframes. Either use Full Lname, Full Fname in both or Full Lname, Fname initial on both, this will define the accuracy of the algorithm.

#patentref
a = data.frame( patent_no = c(1,1,1,2,3,4,4,6,7,8)
                ,Lname = c("roberts","evans","jeff","yu","bahl","stevens","Alyssia","Cecelia","george","Stevens")
                ,Fname = c("A","L","A","F","G","M","A","A","A","M")
                ,seq = c(1,2,3,1,1,1,1,1,1,1))

#reference
b = data.frame( unique_id = c(1,2,2,3,4,5,6,7,8,9)
                ,Lname = c("Robert","Evans","george","watson","baxter","stevens","Elissa","Cecily","Evans","Stevens")
                ,Fname = c("A","L","A","F","G","M","A","A","L","M")
                ,seq = c(1,2,1,1,1,1,1,1,1,1))

tmp<-cbind(a,b)
# Then convert the dataframes to tables since tables have better processing speeds in R and have a better memory utilization. It may not matter with a small dataset but with over 10k rows, this little trick will come in very handy.
a = data.table(a)
b = data.table(b)

# Structure for final author list:
merged_author_list_temp = data.frame(   patentAuthorLname = as.character(a$Lname[1])
                                        ,patentAuthorFname = as.character(a$Fname[1])
                                        ,refAuthorLname = as.character(b$Lname[1])
                                        ,refentAuthorFname = as.character(b$Fname[1])
                                        ,patentNumber = a$patent_no[1]
                                        ,unique_id = b$unique_id[1]
)
# Here we define the final list where we will put all the matched authors and this will form a connection bridge between the two tables. 
final_list = merged_author_list_temp[0,]
final_list = data.table(final_list) #it's just an empty dataframe with colnames

# The main loop: 
# This loop controls the row flow of the main dataset. Suppose dataframe a (patref) has 100 rows then this loop will run 100 times.
for(i in 1:length(a$patent_no))
{
  print(paste0("mainloop:",i,' ',a$Lname[i])) # Necessary only if you want to keep track of the progress, 
  # First, we will take the main author of the Patent in dataframe a, and match its last name with all the last names in dataframe b.
  argument1 = a$Lname[i]
  argument2 = b$Lname[i]
  result = fuzzy1(argument1,argument2) # First result
  df = result[0,] # For copying the df structure
  
  # For loop to check the matches between L_name in 1st table with all L_names in 2nd table
  # Here, we will loop through the entire length of dataframe b and try to match the last names with the current last name in dataframe a.
  for(j in 1:length(b$unique_id)) # Loops through b
  {
    #print(paste0("secondary for loop",j))
    argument2 = b$Lname[j] #the LName column from ref
    result = fuzzy1(argument1,argument2) # Stores all the cosine values of all the matches
    df = rbind(df,result)
  }
  
  # Now, since we only want similar last names:
  # Once we have all the values, we will filter them out to keep only the similar last names. Due to the errors in names and other problems we will set a threshold of 0.2 in this case. 
#This means that we will pass all the author names which are somewhat similar too. For example, a threshold value of 0.2 will pass George and georg as same authors. You can change the value depending on the requirement.
  current_df = df[df$cosine<0.2,] 
  
  # Filtering process.
  # If the lnames are similar enough (cosine<0.2), lets check if the first initial matches:
  # Once we know that the last names are similar, we can move on to the first names/initials. We follow the same process to match the first initials.
  firstInitial_table1 = a$Fname[a$Lname == as.character(current_df$s1name[1])]
  firstInitial_table2 = b$Fname[b$Lname == as.character(current_df$s2name[1])]
  # Fuzzy match for Fnames having the same Lnames
  result_firstInitial_1 = fuzzy1(firstInitial_table1,firstInitial_table2) 
  
  # Now, if:
  #1. the firstname match is good enough 
  # 2. there is just one entry in the table, 
  #then we can say that the two authors from diff table are the same. Only one entry in the table means that there is an unique match and we do not need to match for co-authors. 
#Here’s how we specify that in R:
  if(!is.na(result_firstInitial_1$cosine[1])&result_firstInitial_1$cosine[1]<0.2&  length(result_firstInitial_1$cosine)==1)
  {
    print("first if-if fName match is good after filtering out bad-matched lastname")
    # Let's create a final merged author list, all the final matched authors will go into this list
    # Make sure to get the patent number and unique_id directly from the original dataframes. In fact, that is the only way to retrieve the ids, since we are matching just the strings, we need to use that to trace back and get the unique_id and patent number.
    merged_author_list = data.frame(  patentAuthorLname = as.character(current_df$s1name[1]),patentAuthorFname = as.character(result_firstInitial_1$s1name[1]),refAuthorLname = as.character(current_df$s2name[1])
,refentAuthorFname = as.character(result_firstInitial_1$s2name[1])
                                      ,patentNumber = a$patent_no[a$Fname==as.character(result_firstInitial_1$s1name[1]) &
                                                                    a$Lname==as.character(current_df$s1name[1])]
                                      ,unique_id = b$unique_id[b$Fname==as.character(result_firstInitial_1$s2name[1]) &
                                                                 b$Lname==as.character(current_df$s2name[1])]
    )
    l = list(final_list,merged_author_list)
    final_list = rbindlist(l)
  }
#If there is more than one match in the table (a)?
  # Now we get into the co-authors. This else represents that there are more than one author(s) with a similar lastname/first initial combination, so we move on to the co-authors.
  else #else if the same combination appears twice, we check for co-authors
  {
    print ("first else (run this to check co-author))")
    # To get the co-author list, we simply take the authors with the same patent numbers or unique_ids
    # To get all the authors from table1 with same patent number
    currentPatentNumber = a$patent_no[a$Lname == as.character(argument1)]
    argument1_df = a[a$patent_no == currentPatentNumber,]
    argument1 = argument1_df$Lname[argument1_df$Lname != as.character(argument1)][1]
    # Same with table 2
    currentUnique_id = b$unique_id[b$Lname == as.character(argument2)]
    argument2_df = b[b$unique_id == currentUnique_id,]
    argument2 = argument2_df$Lname[argument2_df$Lname != as.character(argument2)][1]
    result1 = fuzzy1(argument1,argument2)
    df1 = result1[0,] # Again, For copying the df structure
    
    # Next step is similar to the previous ones. We match the lastnames, then firstnames and if those are similar and have more than one entry then we move onto the third author match and so on
    for(k in 1:length(argument2_df$unique_id))
    {print(paste0("third for loop, continue to checking co authors?"))
      argument2 = argument2_df$Lname[k]
      result1 = fuzzy1(argument1,argument2)
      df1 = rbind(df1,result1)
    }
    #filter out all the shit whose cosign>=0.1 after coauthor check
    current_df = df1[df1$cosine<0.1,]

    firstInitial_table1=argument1_df$Fname[argument1_df$Lname==as.character(current_df$s1name[1])]
    firstInitial_table2=argument2_df$Fname[argument2_df$Lname==as.character(current_df$s2name[1])]
    result_firstInitial_1 = fuzzy1(firstInitial_table1,firstInitial_table2) 
    
    if(!is.na(result_firstInitial_1$cosine[1])&result_firstInitial_1$cosine[1]<0.1 &
       length(result_firstInitial_1$cosine) == 1)
    {
      print ("SECOND IF!!! If fName and lname match is good after checking co-authors ") # for checking purposes
      merged_author_list1 = data.frame(  patentAuthorLname = as.character(current_df$s1name[1])
                                         ,patentAuthorFname = as.character(result_firstInitial_1$s1name[1])
                                         ,refAuthorLname = as.character(current_df$s2name[1])
                                         ,refentAuthorFname = as.character(result_firstInitial_1$s2name[1])
                                         ,patentNumber = a$patent_no[a$Fname==as.character(result_firstInitial_1$s1name[1]) &
                                                                       a$Lname==as.character(current_df$s1name[1])]
                                         ,unique_id = b$unique_id[b$Fname==as.character(result_firstInitial_1$s2name[1]) &
                                                                    b$Lname==as.character(current_df$s2name[1])]
      )
      l1 = list(final_list,merged_author_list1)
      final_list = rbindlist(l1)
      
    }
    else
   #these are the guys with no match as far as cosine is set
    {
      print("TBD.Under construction if fname and lname of coauthor are not cosign>0.1 v...run the third author match? ")
      
     # this is where the third author match is done, which is to be included in the later versions.
    }
  }#else end
} # Main for loop end

final_list # these are the guys that are matched from two tables
b
l1
print(argument2_df)
print(argument1_df)
a
b
final_list
#
#AuthorID, reference type, 
class(tmp)
write.csv(tmp,file="dummylist.csv")
'
#add in
#stepthrough, run the loop one line at a time
#debugging in rstudio
#research what other string dist are and 
#take all the solo guys from ref and patref #easy shit

#For those guys that match
#ReferenceNumber, referenceType, Author Name(Lname, First, MI)

#For guys that dont match
Put them in a different table
Solo Patref author vs. ALL THE ref authors (Including middle initial)


a
b
