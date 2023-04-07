# Evan Vigorito
# STA308- Homework 3
# April 3rd 2023

library(tidyverse) #Loading in the necessary packages


GuessTheNumber <- function(lower=0,upper=10,seed=NULL){ ##Creating the function called GuessTheNumber
  if(! is.numeric(lower) | lower-as.integer(lower)!=0 | ##Making sure each each answer is categorized as.numeric for lose lower than the "lower" barrier
     !is.numeric(upper) | upper-as.integer(upper)!=0) {. ##Making sure each each answer is categorized as.numeric for those higher than the "upper" barrier
    stop("upper and lower must be an integer") #Ending the function if the answer given is not an integer and making sure the limits (upper and lower) are integer
  } else if(lower >= upper){  #Lines 12 and 13 stop the function if the lower bound value is greater than or equal to the upper bound
    stop("value of upper must be greater than lower")
  }
  
  set.seed(seed) ##Setting the seed to the seed from the function which again would just be "seed"
  RandomNumber <- sample(c(lower:upper),size=1) ## Assigning the sample() function to create a random number between the limits that gives us 1 number
  signal = 0  ## if the got the answer right or not, if not the signal is 0
  NumGuesses = 0 ##Value created to report the number of guesses the player needed, setting it to 0 to start
  Guesses <- c() ##Creating the empty vector for all the guesses and random number to go into 
  while(signal == 0){ #Using while function to 
    value <- readline("Please enter a number: ") ##Prompting the player to guess a number when the function is run
    value = as.numeric(value) #Value they entered must be entered as a number so the "value" must be as.numeric()
    NumGuesses = NumGuesses+1 ## Every guess they make the NumGuesses goes up 1 so when it starts at 0 it goes up 1 everytime
    Guesses <- c(Guesses, value) ##Filling in the empty guesses vector with the guesses and the values of the guesses
    if (value == RandomNumber){ 
      signal = 1 ## If they got the answer correct or not, if correct the signal is 1
      print("Congratulations, you guessed correct!") ##Lines 26-28 show that if the random number is guessed correctly, it will print this message
    } else if(value < RandomNumber){
      print("Too Low, try again") ## Lines 29-30 show that if the number guessed is below the random number this message will be displayed
    } else if(value > RandomNumber){
      print("Too high, try again!") ## Lines 31-32 show that if the number guessed is above the random number it will print this message
    }
  }
  list(RandomNumber, Guesses, NumGuesses) ##Used list() to put all information into one list (the random number, the guesses, and the number of guesses)
}

GuessTheNumber() #running the function
