
rm(list = ls())


library(dplyr)
library(knn.covertree)
library(tm)

# tags1 = tags %>%
#   group_by(tag)%>%
#   count(.) #1589 unique tags
# 


#Reading the datasets

path = "C:/Users/pedro.e.sequera/Desktop/Intent HQ - Movie Similarity/ml-latest-small/"

links = read.csv(paste0(path,"links.csv"),header = T)
movies = read.csv(paste0(path,"movies.csv"),header = T)
tags = read.csv(paste0(path,"tags.csv"),header = T)
ratings = read.csv(paste0(path,"ratings.csv"),header = T)

#Deduping the movie titles


#Test with ratings only

ratings.matrix = ratings %>%
  group_by(movieId) %>%
  summarise(rating_avg = mean(rating),rating_number = length(rating))


#Renaming the rows as the movieIDs and removing the index
ratings.matrix2 = ratings.matrix[,-1]
  





#Creating the Genre Matrix by movieID


 
genre.list = strsplit(as.character(movies$genres),split="\\|")


docs <- as.VCorpus(genre.list)

docs <- tm_map(docs, PlainTextDocument)

dtm = DocumentTermMatrix(docs)

dtm.matrix = as.matrix(dtm) #9742 22

#removing redudant variables and renaming
genre.matrix = cbind.data.frame(data.frame(movies$movieId),dtm.matrix[,-1])
names(genre.matrix)[1] = "movieID"
names(genre.matrix)[15] = "Not Listed"



#####TAGS Matrix


#Substituting white space with underscore
tags$tag_mod = gsub(" ","_",tags$tag)

#Concatenate tags by movieID
tags_list = tapply(tags$tag_mod, tags$movieId, function(x){paste(x,sep=",")})

docs.tags <- as.VCorpus(tags_list)

docs.tags = tm_map(docs.tags,removePunctuation)

docs.tags <- tm_map(docs.tags, tolower)

docs.tags <- tm_map(docs.tags, removeWords, stopwords("english")) 

docs.tags <- tm_map(docs.tags, PlainTextDocument)

dtm.tags = DocumentTermMatrix(docs.tags)

dtm.tags.matrix = as.matrix(dtm.tags) #1572 1742
#1572 movies and 1457 unique tags


tags.matrix = cbind.data.frame(data.frame(as.integer(names(tags_list))),dtm.tags.matrix)
names(tags.matrix)[1] = "movieID"


###Next step is to do it bigrams


#############
########
###Joining genre.matrix, tags.matrix and ratings.matrix

#Genre has 9742 movieIDs
#Ratings has 9742 movieIDs
#Tags has 1572 movieIDs

final.df.stage = left_join(genre.matrix,ratings.matrix,by=c("movieID" = "movieId"))

final.df = left_join(final.df.stage,tags.matrix,by="movieID") #9742 by 1766

#Replacing NAs with 0's
final.df[is.na(final.df)] = 0








p = find_knn(final.df,10,distance = "rankcor")
#p = find_knn(final.df,10)

# #Creating table to translate index into title
temp = left_join(final.df,movies,by = c("movieID" = "movieId")) %>%
   select(one_of(c("movieID","title")))


top10_nn =p$index


#translating movieIDs into Titles
top10_nn_title = top10_nn


title <- function(x) {
  return(temp[x,2])
} 

top10_nn_title = as.data.frame(apply(top10_nn, 2, title))



#Final datafame
top10_nn_final = cbind.data.frame(data.frame(temp[,2]),top10_nn_title)
names(top10_nn_final) = c("Movie Title",paste0("top",(1:10)))

###Creating Function
top10_nn_function = function(title){
  top10_vector = top10_nn_final[which(top10_nn_final$`Movie Title` %in% title),2:11]
  return(t(top10_vector))
  
}


























# # The default: symmetricised pairwise distances between all rows
# pairwise <- find_knn(mtcars, 5L)
# image(as.matrix(pairwise$dist_mat))
# 
# # Nearest neighbors of a subset within all
# mercedeses <- grepl('Merc', rownames(mtcars))
# merc_vs_all <- find_knn(mtcars, 5L, query = mtcars[mercedeses, ])
# # Replace row index matrix with row name matrix
# matrix(
#   rownames(mtcars)[merc_vs_all$index],
#   nrow(merc_vs_all$index),
#   dimnames = list(rownames(merc_vs_all$index), NULL)
# )[, -1]  # 1st nearest neighbor is always the same row
