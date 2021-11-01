#This is the begining of my project
library('ggplot2')
library('rmarkdown')
library('knitr')
library('dplyr')
library('gridExtra')
library('forcats')
library('gridExtra')
library('tidyr')


#Read the book dataset
Book_details <- read.csv(file = 'Merged_books_summary.csv')
head(Book_details)
#converting all blank space to NA
Books_details1 <- Book_details %>% mutate_all(na_if,NA)
Books_details1 <- Book_details %>% mutate_all(na_if,"")
#Loading User Review Dataset
User_Review<-read.csv(file = 'Merged_books_users_rating.csv')
glimpse(Books_details1)
glimpse(User_Review)

# I will use the first file for data analysis and user_review to build a recommender system
#Number of books published each year
#How many years data do we have
unique(Books_details1[c("publication_year")])
#There are errors like 20, 206 . I will analyze the data between 1900-2017 and see number of books 
#published every 10 years
g <- ggplot(data = Book_details, aes(x=publication_year))
zoom <- coord_cartesian(xlim = c(1900,2017))
g+geom_histogram(binwidth = 10)+zoom

#The maximum books are published between 2000 and 2017
#I will see the microscopic details between these years and limit the dataset for after 2000

g <- ggplot(data = Book_details, aes(x=publication_year))
g+geom_histogram(binwidth = 1,fill='blue',color = 'black')+scale_x_continuous(breaks = seq(from = 2000, to = 2017, by = 1))+coord_cartesian(xlim = c(2000,2017))
#2013 has the maximum number of books published from my dataset

#I will filter the data from 2000-2017 for my analysis
Books_21st_century <-subset(Books_details1, publication_year >=2000  & publication_year <= 2017)

#I have 93659 books from 21st century
glimpse(Books_21st_century)

#How does number of pages affect popularity(number of pages vrs rating/number of rating)
#Spread of number of pages in 21st century
ggplot(data = Books_21st_century, aes(x="", y=num_pages, na.rm=TRUE)) +geom_boxplot()+coord_cartesian(ylim = c(0, 5000))+xlab("Spread of Number of Pages")
summary(Books_21st_century$num_pages)
#The page numbers range from 0-945077 with mean 274.6 and median 250 pages
Books_21st_century %>%filter (num_pages == 0) %>%group_by (title)
#Some books have 0 pages which is a misrepresantation

#Does number of pages effect popularity (Ratings and number of ratings)
plot1<-ggplot(data = Books_21st_century, aes(x=num_pages, y=average_rating,na.rm=TRUE)) +geom_point()+coord_cartesian(xlim = c(0, 500))+xlab("number of pages(0-500)")+ylab("Average Ratings")
plot2<-ggplot(data = Books_21st_century, aes(x=num_pages, y=average_rating,na.rm=TRUE)) +geom_point()+coord_cartesian(xlim = c(1000, 5000))+xlab("number of pages(1000-5000)")+ylab("Average Ratings")
grid.arrange(plot1, plot2, ncol=2)
#Books with more than 1000 pages are highly rated with very few outliers

#Number of Ratings
plot3<-ggplot(data = Books_21st_century, aes(x=num_pages, y=rating_individual,na.rm=TRUE)) +geom_point()+coord_cartesian(xlim = c(0, 500))+xlab("number of pages(0-500)")+ylab("Number of Ratings")
plot4<-ggplot(data = Books_21st_century, aes(x=num_pages, y=rating_individual,na.rm=TRUE)) +geom_point()+coord_cartesian(xlim = c(1000, 5000))+xlab("number of pages(500-1000)")+ylab("Number of Ratings")
grid.arrange(plot3, plot4, ncol=2)
#Popularity(Number of purchases/number of ratings) of books is less as the number of pages are more than 1000
#Most popular books were between 200-500 pages 

#How does number of authors affect popularity(number of pages vrs rating/number of rating)
ggplot(data = Books_21st_century, aes(x="", y=num_authors, na.rm=TRUE)) +geom_boxplot()+ggtitle("Spread of number of authors")
#There are many outliers
#Books with more than 2 authors
auth_2<-Books_21st_century %>%filter(num_authors>2) %>%select(num_authors)
summary(auth_2)
#The dataset had books upto 51 authors
glimpse(auth_2)
#There are 5760 books with more than 3 authors

plot5<-ggplot(data = Books_21st_century, aes(x=num_authors, y=average_rating,na.rm=TRUE)) +geom_point()+coord_cartesian(xlim = c(0, 51))+xlab("number of authors")+ylab("Average Rating")+ggtitle("Effect of number of Authors on average Rating")
plot6<-ggplot(data = Books_21st_century, aes(x=num_authors, y=rating_individual,na.rm=TRUE))+geom_point()+coord_cartesian(xlim = c(0, 51))+xlab("number of authors")+ylab("Number of Rating")+ggtitle("Effect of number of Authors on number of Ratings")
grid.arrange(plot5, plot6, ncol=2)
#From the plot, books with more number of authors are not rated low 
#books with less number(<10) of authors does not have any correlation with rating
#however less people purchased books with many number of authors

#Top 10 maximum book version
distinct_work_id = Books_21st_century %>% distinct(work_id,.keep_all = TRUE)%>%select(work_id,title,books_count)
distinct_work_id %>% arrange(desc(books_count))%>%head(10)%>%ggplot(aes(x=reorder(title,books_count),books_count),y=books_count) +xlab("Books")+geom_col(fill='blue',color = 'black') + coord_flip()

#German for Pride & Prejudice has more than 3000 versions including different languages and edition

#Top 10 most popular languages(language vrs sum(number of rating))
Books_21st_century%>%group_by(language_code)

plot7<-Books_21st_century%>%group_by(language_code)%>%filter(!is.na(language_code))%>%summarise(average_buy= sum(rating_individual))%>% arrange(desc(average_buy))%>%head()%>%ggplot(aes(x = reorder(language_code,average_buy),y = average_buy,na.rm=TRUE))+xlab("Language code")+geom_col(fill='blue',color = 'black')+ggtitle("Top 10 popular languages")+coord_flip()+ylab("Number of Ratings")
plot8<-Books_21st_century%>%group_by(language_code)%>%filter(!is.na(language_code))%>%summarise(average_buy= sum(rating_individual))%>% arrange(desc(average_buy))%>%slice(4:15)%>%ggplot(aes(x = reorder(language_code,average_buy),y = average_buy,na.rm=TRUE))+xlab("Language code")+geom_col(fill='blue',color = 'black')+coord_flip()+ggtitle("Popular Lang. other than English")+ylab("Number of Ratings")
grid.arrange(plot7, plot8, ncol=2)

#How did popularity of ebooks change between 2000-2017(number of ebooks every year)
Books_21st_century%>%group_by(is_ebook==TRUE)%>%ggplot(aes(publication_year))+geom_histogram(binwidth = 1,fill='blue',color = 'black')+scale_x_continuous(breaks = seq(from = 2000, to = 2017, by = 1))+coord_cartesian(xlim = c(2000,2017))+xlab("Publication year")+ylab("Number of ebooks")+ggtitle("ebooks published between 2000-2017")
#The publication of ebook sored after 2011, this is the time when kindle launched and started its own publication

#Distribution of format of book(hardcover, paperback etc)
format_count<-Books_21st_century %>%filter(!is.na(format))%>%group_by(format) %>%summarise(counts = n())%>%arrange(desc(counts))%>%head(10)
ggplot(format_count,aes(fct_reorder(format,counts),counts))+geom_col(fill='blue',color = 'black')+coord_flip()+xlab("Formats")+ylab("Number of books")+ggtitle("Top 10 formats of books in the dataset")
#Paperback is the most available books, followed by hardcover and ebook, there are still less number of audiobooks
#which format is more popular among people
Books_21st_century%>%group_by(format)%>%filter(!is.na(format))%>%summarise(format_sum= sum(rating_individual))%>% arrange(desc(format_sum))%>%head()%>%ggplot(aes(x = reorder(format,format_sum),y = format_sum,na.rm=TRUE))+xlab("Format")+ylab("Number of Ratings")+geom_col(fill='blue',color = 'black')+coord_flip()+ggtitle("Popularity of different formats")
#kindle edition is more popular than all other types of ebooks, kindle has brought a lot of change in reading behavior

#Is number of rating correlated to average rating
plot11<-ggplot(data = Books_21st_century, aes(x = rating_individual, y = average_rating)) +geom_point()+xlim(1,1000)+geom_smooth()+xlab("Number of Ratings")+ylab("average Rating")+ggtitle("Effect of Popularity(1-1000) on average rating")
plot12<-ggplot(data = Books_21st_century, aes(x = rating_individual, y = average_rating)) +geom_point()+xlim(1000,10000)+geom_smooth()+xlab("Number of Ratings")+ylab("average Rating")+ggtitle("Effect of Popularity(1000-10000) on average rating")
grid.arrange(plot11, plot12, ncol=2)

#Effect of Average Rating on Popularity
plot9<-ggplot(data = Books_21st_century, aes(x =average_rating, y = rating_individual)) +geom_point()+xlim(1,3)+ylim(0,1000)+xlab("average Rating")+ylab("Number of Rating")+ggtitle("Effect of average rating on Popularity(1-1000)") 
plot10<-ggplot(data = Books_21st_century, aes(x =average_rating, y = rating_individual)) +geom_point()+xlim(3,5)+ylim(0,50000)+xlab("average Rating")+ylab("Number of Rating")+ggtitle("Effect of average rating on Popularity(1000-10000)")  
grid.arrange(plot9, plot10, ncol=2)
#Number of ratings is not correlated with average_rating, however lower rated books are very less likely to be purchased(which is obvious)
#There are almost no books with average rating 5, THERE ARE CRITICS EVERYWHERE!!!average rating above 3.5 is safe.
#ITS DIFFICULT TO KEEP EVERYONE HAPPY, JUST DO YOUR BEST!!

#distribution of average rating from 2000-2017(what rating is most likely)
Books_21st_century%>%summarise(n=n(),Rating_1=sum(num_rating_1),Rating_2=sum(num_rating_2),Rating_3=sum(num_rating_3),Rating_4=sum(num_rating_4),Rating_5=sum(num_rating_5))%>%
  gather("key","value",-n)%>%ggplot(aes(x=key,y=value))+geom_col(fill='blue',color = 'black')+xlab("Rating")+ylab("Number of Rating")+ggtitle("Most Likely ratings")

#Proportion of different ratings from 2000-2017
Books_21st_century%>%group_by(publication_year)%>%
    summarise(n=n(),Rating_1=sum(num_rating_1),Rating_2=sum(num_rating_2),Rating_3=sum(num_rating_3),Rating_4=sum(num_rating_4),Rating_5=sum(num_rating_5))%>%
    gather("key","value",-c(publication_year,n))%>%ggplot(aes(x=publication_year,y=value,group=key,fill=key))+geom_bar(position = "fill",stat = "identity")+xlab("years")+ylab("Proportion of Different Ratings")
#There is a tendency of people to give 4-5 rating

#Number of unique publishers
pub<-unique(Books_21st_century$publisher)
length(pub)
#There are 25111 unique publishers in my dataset
#Top 10 publishers with highest number of publications
publisher_count<-Books_21st_century %>%filter(!is.na(publisher))%>%group_by(publisher) %>%summarise(counts = n())%>%arrange(desc(counts))%>%head(10)
ggplot(publisher_count,aes(fct_reorder(publisher,counts),counts))+geom_col(fill='blue',color = 'black')+coord_flip()+xlab("Publishers")+ylab("Number of books published")+ggtitle("Top 10 publishers")
#Many books are self published by Createspace owned by amazon
#FUN FACT! kindle revolutionized the way people read, it helped amazon reach 10Bn revenue in 2011
#kindle beat apple 

#Average Rating of these 10 publishers with highest rated books
Books_21st_century%>%group_by(publisher)%>%filter(publisher %in% publisher_count$publisher)%>%summarise(publisher_rating= mean(average_rating))%>%
  arrange(desc(publisher_rating))%>%head(10)%>%ggplot(aes(x = reorder(publisher,publisher_rating),y = publisher_rating,na.rm=TRUE))+ylab("Average Rating")+geom_col(fill='blue',color = 'black')+coord_flip()+ylab("Ratings")+xlab("publishers")+ggtitle("Ratings of top 10 publishers")
#All these publishers publish high rated books

#Popularity of these publishers among users
Books_21st_century%>%group_by(publisher)%>%filter(publisher %in% publisher_count$publisher)%>%summarise(number_rating= sum(rating_individual))%>%
  arrange(desc(number_rating))%>%head(10)%>%ggplot(aes(x = reorder(publisher,number_rating),y = number_rating,na.rm=TRUE))+ylab("Average Rating")+geom_col(fill='blue',color = 'black')+coord_flip()+ylab("Number of Ratings")+xlab("publishers")+ggtitle("Number of of top 10 publishers")
#Vintage is the most popular publication with many highly rated books
#Createspace is not very popular but has many books and are rated high

#For highly rated books(>4.5) how does page number, number of authors,number of ratings and number of reviews vary
highly_rated_books<-Books_21st_century%>%filter(average_rating >4.5)
plot_page<-ggplot(data = highly_rated_books, aes(x="", y=num_pages, na.rm=TRUE)) +geom_boxplot()+ylim(0,1000)+ylab("Number of pages(rating>4.5)")
plot_author<-ggplot(data = highly_rated_books, aes(x="", y=num_authors, na.rm=TRUE)) +geom_boxplot()+ylab("Number of Authors(rating>4.5)")
plot_num_rating<-ggplot(data = highly_rated_books, aes(x="", y=rating_individual, na.rm=TRUE)) +geom_boxplot()+ylim(0,50)+ylab("Number of ratings(rating>4.5)")
plot_num_review<-ggplot(data = highly_rated_books, aes(x="", y=text_reviews_count_individual, na.rm=TRUE)) +geom_boxplot()+ylim(0,50)+ylab("Number of reviews(rating>4.5)")
grid.arrange(plot_page, plot_author,plot_num_rating,plot_num_review, ncol=2)


#For recommender system
#I will model a collaborative recommendation system
#User based:look at similarity between users and then find n users closest to
#the person we want to recommend and search for product those people have bought
#one has purchased,other will be recommended
#I will use the user_review dataset to build this recommender system


#Looking at the distribution of ratings 
User_Review%>%group_by(rating)%>%summarize(cases=n())%>%
  ggplot(aes(rating,cases))+geom_col(fill='blue',color = 'black')+ylab("Number of Users")

#There are some users who read the book but did not rate it. Since, we do not know about the liking
#we will just get rid of all the zero ratings
User_review1<-User_Review[User_Review$rating!=0,]

#Now the rating distribution is
User_review1%>%group_by(rating)%>%summarize(cases=n())%>%
  ggplot(aes(rating,cases))+geom_col(fill='blue',color = 'black')+ylab("Number of Users")

#Now I will look at how many recommendation does each user leaves
user_num_recom<-User_review1%>%group_by(user_id)%>%count()
summary(user_num_recom)
#75% of user has given 6 recommendation or less
#Mean is more than 3rd quartile which implies lot of outliers, there are people leaving upto 221 recommendations

#User_review1 is the dataset I will work with
#To simplify the user_ids, I will use the mapping file that I have downloaded to map users with
#numeric id
user_map<- read.csv(file = 'user_id_map.csv')
glimpse(user_map)
user_rating_review<-merge(x = User_review1, y = user_map, by = "user_id", all.x = TRUE)

#I will create the User-product matrix
#First I will derive a rating dataframe from my user_rating_review dataset
#Rating containing user_id_csv, book_id,book_rating
Rating <- data.frame(user_rating_review$user_id_csv, user_rating_review$book_id, user_rating_review$rating)
names(Rating) <- c("user_id", "book_id", "rating")


#Creating the user-item matrix based on recommendation

user_item = Rating%>%pivot_wider(names_from=book_id,values_from = rating)%>%
  as.data.frame()
row.names(user_item)=user_item$user_id
user_item$user_id = NULL  
user_item = as.matrix(user_item)  
user_item[1:5,1:5]
glimpse(user_item)

#There are lot of NA in the matrix which is called sparsity.
#To find similarity between products we will use cosine similarity function

#Code for function cosine similarity calculation
cos_similarity = function(A,B){
  num=sum(A*B,na.rm = TRUE)
  den = sqrt(sum(A^2,na.rm = TRUE))*sqrt(sum(B^2,na.rm = TRUE))
  result = num/den
  
  return(result)
}

#Once my user-item matrix is ready, I will use the matrix to code user-based collaborative recommender system
#writing the user_recommendation function
user_recommendation = function(user_id_no,user_item_matrix=user_item,ratings_matrix=Rating,
                               n_recommendation=5,threshold=1,nearest_neighbors=10){
  user_index = which(rownames(user_item_matrix)==user_id_no)
#Since similarity function will be applied rowwise, margin =1,rows are for users  
  similarity = apply(user_item_matrix,1,FUN = function(y)cos_similarity(user_item_matrix[user_index,],y))
  
  similar_users = tibble(user_id=names(similarity), similarity=similarity) %>%
    filter(user_id != user_id_no) %>%
    arrange(desc(similarity)) %>%
    top_n(nearest_neighbors,similarity)
  
  read_book_user = ratings_matrix$book_id[ratings_matrix$user_id ==user_id_no]
  
  recommendations = ratings_matrix%>%
    filter(user_id %in% similar_users$user_id &
             !(book_id %in% read_book_user))%>%
    group_by(book_id)%>%
    summarise(count=n(),rating = mean(rating))%>%
    filter(count>threshold)%>%
    arrange(desc(rating),desc(count)) %>%
    head(n_recommendation)
  
  return(recommendations)
}

recom_cf_user = user_recommendation("2955",n_recommendation = 10)
recom_cf_user

#The above code looks for similar people based on recommended books and once maximum similarity is found
#books that are not read by one user is recommended to the other

recom_cf_user1 = merge(x=recom_cf_user,y=Books_details1,by="book_id",all.x=TRUE)


recom_cf_user1







