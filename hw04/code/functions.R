#takes in a vector and returns the input vector with out misssing values
remove_missing <- function(a) {
  a <- a[!is.na(a)]
}

#takes a numeric vector, and an optional logical na.rm argument,to find the minimum value
get_minimum <- function(a, na.rm) {
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  
  if (na.rm == TRUE) {
    a <- remove_missing(a)
    a <- sort(a)
    head(a, 1)
  } else {
    head(sort(a), 1)
  }
}

#takes a numeric vector, and an optional logical na.rm argument,to find the maximum value
get_maximum <- function(a, na.rm) {
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  
  if (na.rm == TRUE) {
    a <- remove_missing(a)
    a <- sort(a)
    tail(a, 1)
  } else {
    tail(sort(a), 1)
  }
}

#takes a numeric vector, and an optional logical na.rm argument, 
#to compute the overall range of the input vector.
get_range <- function(a, na.rm) {
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  get_maximum(a, na.rm) - get_minimum(a, na.rm)
}

#takes a numeric vector, and an optional na.rm argument, to compute the
#10th percentile of the input vector
get_percentile10 <- function(a, na.rm) {
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  unname(quantile(a, c(0.1), na.rm = na.rm))
}

#takes a numeric vector, and an optional na.rm argument, to compute the
#90th percentile of the input vector
get_percentile90 <- function(a, na.rm) {
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  unname(quantile(a, 0.9, na.rm = na.rm))
}

#takes a numeric vector, and an optional logical na.rm argument, to
#compute the median of the input vector
get_median <- function(a, na.rm) {
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  
  if (na.rm == TRUE) {
    a <- remove_missing(a)
    a <- sort(a)
    if (length(a) %% 2 == 0) {
      a[length(a)/2] * 0.5 + a[length(a)/2 + 1]*0.5
    } else {
      a[length(a)/2 + 1]
    }
  } else {
    a <- sort(a)
    if (length(a) %% 2 == 0) {
      a[length(a)] * 0.5 + a[length(a) + 1]*0.5
    } else {
      a[length(a)/2]
    }
  }
}

#takes a numeric vector, and an optional logical na.rm argument, to
#compute the average (i.e. mean) of the input vector
get_average <- function(a, na.rm) {
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  
  if (na.rm == TRUE) {
    a <- remove_missing(a)
    b <- 0
    for (i in a) {
      b <- b + i
    }
    b / length(a)
  } else {
    b <- 0
    for (i in a) {
      b <- b + i
    }
    b / length(a)
  }
}

#takes a numeric vector, and an optional logical na.rm argument, to
#compute the standard deviation of the input vector.
get_stdev <- function(a, na.rm){
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  
  if (na.rm == TRUE) {
    a <- remove_missing(a)
    b <- 0
    average <- get_average(a, na.rm)
    for (i in a) {
      b <- b + (i - average) * (i - average)
    }
    sqrt(b / (length(a) - 1))
  } else {
    b <- 0
    average <- get_average(a, na.rm)
    for (i in a) {
      b <- b + (i - average) * (i - average)
    }
    sqrt(b / (length(a) - 1))
  }
}

#takes a numeric vector, and an optional na.rm argument, to compute
#the first quartile of the input vector
get_quartile1 <- function(a, na.rm) {
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  unname(quantile(a, 0.25, na.rm = na.rm))
}


#takes a numeric vector, and an optional na.rm argument, to compute
#the third quartile of the input vector
get_quartile3 <- function(a, na.rm) {
  if (!is.numeric(a)) {
    stop("non_numeric argument")
  }
  unname(quantile(a, 0.75, na.rm = na.rm))
}

#takes a numeric vector, and calculates the number of missing values NA
count_missing <- function(a){
  length(a) - length(remove_missing(a))
}

#takes a numeric vector, and returns a list of summary statistics
summary_stats <- function(a) {
  list(minimum = get_minimum(a, na.rm = TRUE),
       percent10 = get_percentile10(a, na.rm = TRUE),
       quartile1 = get_quartile1(a, na.rm = TRUE),
       median = get_median(a, na.rm = TRUE), 
       mean = get_average(a, na.rm = TRUE), 
       quartile3 = get_quartile3(a, na.rm = TRUE),
       percent90 = get_percentile90(a, na.rm = TRUE),
       maximum = get_maximum(a, na.rm = TRUE),
       range = get_range(a, na.rm = TRUE),
       stdev = get_stdev(a, na.rm = TRUE),
       missing = count_missing(a))
}

#takes a list of summary statistics, and prints the values in a nice format
print_stats <- function(stats) {
  for (j in 1:length(stats)) {
    cat(names(stats)[j], format(round(unlist(stats[names(stats)[j]]), 4), nsmall = 4), sep = " : ", fill = TRUE)
  }
}

#takes three arguments: a numeric vector x, a minimum xmin, and a
#maximum xmax, to compute a rescaled vector with a potential scale from 0 to 100
rescale100 <- function(b, xmin, xmax) {
  100 * (b - xmin) / (xmax - xmin)
} 

#takes a numeric vector of length n, and returns a vector of length n − 1
#by dropping the lowest value
drop_lowest <- function(b) {
  b <- sort(b)
  b <- b[2:length(b)]
}

#takes a numeric vector of homework scores (of length n), and an
#optional logical argument drop, to compute a single homework value. If drop = TRUE, the
#lowest HW score must be dropped. The function should return the average of the homework
#scores
score_homework <- function(hws, drop) {
  if (drop == TRUE) {
    get_average(drop_lowest(hws), na.rm = TRUE)
  } else {
    get_average(hws, na.rm = TRUE)
  }
}

#takes a numeric vector of quiz scores (of length n), and an optional
#logical argument drop, to compute a single quiz value. If drop = TRUE, the lowest quiz score
#must be dropped. The function should return the average of the quiz scores
score_quiz <- function(quizzes, drop) {
  score_homework(quizzes, drop)
}

#takes a numeric value of lab attendance, and returns the lab score.
score_lab <- function(x) {
  if (x == 11 | x == 12) {
    100
  } else if (x == 10) {
    80
  } else if (x == 9) {
    60
  } else if (x == 8) {
    40
  } else if (x == 7) {
    20
  } else {
    0
  }
}

