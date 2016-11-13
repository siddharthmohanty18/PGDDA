
#Q-1. 
#	Import the Bollywood data set in Rstudio. Make sure that the data file is in the folder (directory) where
# you are currently working.
     
	bollywood <- read.csv("bollywood.csv",header = T)

#Q0.
#	When you import a data set, R stores character vectors as factors (by default).
# Change the attribute 'Movie' from factor to character type. 
     
	bollywood$Movie <- as.character(bollywood$Movie)

	 
	 
#Q1.
#	Create a vector "first_10" and store the first 10 movies (from top) from the bollywood dataset.
# Use the column bollywood$Movie
     
	first_10 <- bollywood$Movie[1:10]
    
	
#Q2.
#	What is the name of 7th movie from the top?
     
	movie_7 <- bollywood$Movie[7]
     
	 
#Q3. 
#	How many rows and columns are present in the bollywood dataset? 
     
	rows <- nrow(bollywood)
  columns <- ncol(bollywood)

	 
#Q4.
#	Find out the total number of  missing values (NA) in the dataset.
     
	na_bollywood <- sum(is.na(bollywood))

	
#Q5.
#	How many variables are of type 'factor' in the dataset. Try using the str function 
# on bollywood. Assign the numeric value to fact_vars.
     
	fact_vars <- length(grep("Factor",capture.output(str(bollywood))))
    
	
#Q6.0 
#	Now let's find out the movies shot by Salman, SRK and Akshay separately.
# subset() function is used for that. The code has already been written for you. 
 
  salman <- subset(bollywood, Hero == "Salman")
  srk <- subset(bollywood,Hero == "SRK")
  akshay <- subset(bollywood,Hero == "Akshay_Kumar")
     
 
#Q6.1 
#	What is the total collection of Akshay, Salman and SRK movies individually?
# Look for a column named 'Tcollection'.
 
  akshay_collection <- sum(akshay$Tcollection) 
	salman_collection <- sum(salman$Tcollection)
	srk_collection <- sum(srk$Tcollection)

#Q6.2 
#	Among the three actors, who has done the most number of movies?
# Store "akshay", "salman" or "srk" as the variable name most_movies_actor.
# (only the correct answer)
  
  no_of_movies <- c(nrow(akshay),nrow(salman),nrow(srk))
  names(no_of_movies) <- c("Akshay","Salman","Srk")
  max_no_of_movies <- max(no_of_movies)
  if(max_no_of_movies == no_of_movies["Akshay"]){
  most_movies_actor <-  "akshay" 
  }  else if(max_no_of_movies == no_of_movies["Salman"]){
    most_movies_actor <-  "salman" 
  } else {
    most_movies_actor <- "srk"
  }
	
  
#Q6.3
# Also, whose collection is the largest (total collection/Tcollection)? 
    
	collection_vector <- c(akshay_collection,salman_collection,srk_collection)
	names(collection_vector) <- c("Akshay","Salman","Srk")
	max_collection <- max(collection_vector)
	if(max_collection == collection_vector["Akshay"]){
	  largest_collection_actor <-  "akshay" 
	}  else if(max_collection == collection_vector["Salman"]){
	  largest_collection_actor <-  "salman" 
	} else {
	  largest_collection_actor <- "srk"
	}

#Q7 Let's try to find out which actor has made the largest opening collection (Ocollection).
#	First, let's see which movie has made the largest opening collection.
# Find out the maximum collection amount first.
    
	max_number <- max(bollywood$Ocollection)

# Great. You got a number as the output. Now you can use which() function to get the index
# of the row where max_number is located. This code is already written for you.
	
  row_num <- which(bollywood$Ocollection == max_number)

	
#	We know the row number corresponding to maximum collection now. To get the actor,
#	extract the relevant column corresponding to row_num.
    
	max_opening_actor <- as.character(bollywood[row_num,c("Hero")])

#Q8 
#	In which month most movies were released? Note that we are talking about dates and months now.
# Let's first install the lubridate package.
    
	install.packages("lubridate")
    
#   After installing any package, we have to load it into the R environment.
	
	library(lubridate)
	
#   The month() function in lubridate extracts the month from a date format.
#   Apply month() to the date column of bollywood.
 
    bollywood$month <- month(bollywood$Rdate)
	
#   as.factor() function converts months into factors.
#   Apply as.factor() to bollywood$month
    
	  bollywood$month <- as.factor(bollywood$month)
	
#   A great way to look at the summary of a data frame is the summary() function.
#   Look at the summary of bollywood data set.
 
    summary(bollywood)

#Q9 
#	How many movies are in flop,average, Hit & Superhit categories. Simply see the summary of 
#	the column 'Verdict'.

    summary(bollywood)
#Average  :15
#Flop     :22
#Hit      :11
#Super Hit: 4    
    
#Q10.1 
#	Find the date on which three movies had launched on the same day?
    tab_rdate <- table(bollywood$Rdate)
    vec_tabdata <- as.vector(tab_rdate)
    date_three <- names(tab_rdate)[which(vec_tabdata == 3)[[1]]] 
    
    
#Q10.2 
#   Find the names of the movies released on date_three and store them in movie1, movie2 and movie3.
    
	  #which()
	  rows_num_rdate <- which(bollywood$Rdate == date_three)
	
    movie1 <- bollywood[rows_num_rdate[1],"Movie"]
    movie2 <- bollywood[rows_num_rdate[2],"Movie"]
    movie3 <- bollywood[rows_num_rdate[3],"Movie"]
    
	
#Q11. 
#	Convert the Rdate column into date format. 
    
	bollywood$Rdate <- as.character(bollywood$Rdate)
  bollywood$Rdate <- as.Date(bollywood$Rdate, format="%d-%m-%Y")   
    
	
# see structure to check if you got it right. 
	str(bollywood)
    
	
	
#Q12. 
#	Change all the characters of movie names into lower case 
# Hint: tolower()
    
	bollywood$Movie <- tolower(bollywood$Movie)

	
# Finally, view the data frame again
    
	View(bollywood)
	
	

   
    


    
    
    
