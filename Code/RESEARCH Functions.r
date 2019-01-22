####################################################################################
## Functions provided by Professor

panel.smooth.asp <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                              cex = 1, col.smooth = "red", span = 2/3, iter = 3, asp,...) 
{
  #browser()
  points(x, y, pch = pch, col = col, bg = bg, cex = cex, asp=1)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth,...) 
}

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)*r
  text(0.5, 0.5, txt, cex = cex.cor)
}

## put histograms on the diagonal
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


#====================================
# Friedman's 1981 paper - For Fig. 1"
# Consider two elliptic clouds in flatland.
#====================================
# Centre and sphere data
#====================================
Sphere.Data <- function(data) {
  data <- as.matrix(data)
  data <- t(t(data) - apply(data, 2, mean))
  data.svd <- svd(var(data))
  sphere.mat <- t(data.svd$v %*% (t(data.svd$u) * (1/sqrt(data.svd$d))))
  return(data %*% sphere.mat)
}

#======================
# Standardize data
#======================
f.data.std <- function(data) {
  data <- as.matrix(data)
  bar <- apply(data, 2, mean)
  s   <- apply(data, 2, sd)
  t((t(data) - bar)/s)
}

#============================================ 
# Convert standardized coeff to original
#============================================ 
std.to.orig <- function (std.coef, mean.X, mean.Y, s.X, s.Y) {
  sz <- length(std.coef)
  B.i <- matrix(0, sz, 1)
  for (i in 2:sz) {
    B.i[i,1] <- s.Y/s.X[i-1]*std.coef[i]
    std.coef[i] <- B.i[i,1]
    B.i[1,1] <- B.i[1,1] + B.i[i,1]*mean.X[i-1] 
  }
  std.coef[1] <- mean.Y - B.i[1,1]
  std.coef
}



#============================================ 
# A function to produce those indices NOT in a set.
# Use this to get the test sample.
#============================================ 
"%w/o%" <- function(x,y) x[!x %in% y]

#============================================ 
# Set the indices for the training/test sets
#============================================ 
get.train <- function (data.sz, train.sz) {
  # Take subsets of data for training/test samples
  # Return the indices
  train.ind <- sample(data.sz, train.sz)
  test.ind <- (1:data.sz) %w/o% train.ind
  list(train=train.ind, test=test.ind)
}

####################################################################################
## Functions Created by Student

## Function to be used to print the Tables in the report (Knitr)

print_kable <- function(data, num_lines = 0, latex_options = 0, caption = NULL) {
  
  # By default print all the data
  ifelse(num_lines == 0, num_lines <- dim(data)[1], num_lines)
  
  if (length(knitr::all_labels()) > 0) {
    #print("Executing Kable inside Knitr")
    
    # By default "striped"
    ifelse(latex_options == 0, latex_options <- c("striped"),
           latex_options <- c("striped","scale_down"))
    
    kable(head(data,num_lines), format = "latex", booktabs = TRUE, caption = caption) %>%
      kable_styling(latex_options = latex_options)
  } else {
    print_table(data,num_lines,latex_options,caption)
  }
}

print_kablev2 <- function(data, num_lines = 0, latex_options = 0, caption = NULL) {
  
  # By default print all the data
  res <- capture.output(print(data))
  
  ifelse(num_lines > 0, res <- head(res,num_lines), res <- res)
  
  if (length(knitr::all_labels()) > 0) {
    #print("Executing Kable inside Knitr")
    
    # By default "striped"
    ifelse(latex_options == 0, latex_options <- c("striped"),
           latex_options <- c("striped","scale_down"))
    
    kable(res, format = "latex", booktabs = TRUE, caption = caption) %>%
      kable_styling(latex_options = latex_options)
  } else {
    print(res)
  }	  
}

## Function to be used to print the Tables in R 

print_table <- function(data, num_lines = 0, latex_options = 0, caption = NULL) {
  # By default print all the data
  ifelse(num_lines == 0, num_lines <- dim(data)[1], num_lines)
  
  head(data, num_lines)
}

## Function to be used to check if one of the kmeans cluster has a zero value (is empty) 

has_empty_cluster <- function(data) {
  res = FALSE
  
  l <- length(data)
  
  for (i in 1:l) { 
    if (data[i] == 0) {
      res <- TRUE
      break
    }
  }
  res
}

## Function to be used to check which variables can be used in a dataset to identify an observation

find_unique_variables <- function(data, single = FALSE) {
  res <- NULL
  
  vars <- colnames(data)
  l <- length(vars)
  num_rows <- nrow(data)
  
  if (single == TRUE) {
    # Check single variables
    for (i in 1:l) {
      if (num_rows == length(unique(data[,c(vars[i])]))) {
        res <- c(vars[i])
        break
      }
    }
  } else {
    
    
    if (is.null(res)) {
      # Check double pairs of variables
      vars1 <- colnames(data)
      vars2 <- colnames(data)
      
      for (i in 1:l) {
        
        for (j in 1:l) {
          
          if (j==i) {break}
          
          if (num_rows == nrow(unique(data[,c(vars1[i],vars2[j])]))) {
            res <- c(vars1[i],vars2[j])
            break
          }
        }
        if (!is.null(res)) {break}
      }
    } 
  }
  res
}


## Function to be used to correct the kmeans clusters ids

correct_kmeans_ids <- function(class_ids,kmeans_ids) {
  
  temp <- data.frame(table(class_ids,kmeans_ids))
  res <- NULL
  last_class_id <-0
  
  unique_class_ids <- unique(temp$class_ids)
  unique_kmeans_ids <- unique(temp$kmeans_ids)
  
  for (i in unique_class_ids) {
    #print(paste0("Checking Class Id=",i))
    
    rows_i <- temp[which(temp$class_ids == i & temp$kmeans_ids %in% unique_kmeans_ids),]
    row <- rows_i[which.max(rows_i$Freq),]
    
    if (nrow(row) > 1) {
      #print("   More than one maximun frequency, selecting the minimun class_id ...")
      row <- row[which.min(row$class_ids),]
    }  
    
    if (nrow(row) == 1) {
      #print(paste0("    Kmeans Cluster id=",row$kmeans_ids," must be changed to=",i))
      res[as.numeric(row$kmeans_ids)] <- as.numeric(i) 
      unique_kmeans_ids <- unique_kmeans_ids[unique_kmeans_ids != row$kmeans_ids]
      last_class_id <- last_class_id +1
    }
  }
  
  if (length(unique_kmeans_ids) > 0) {
    #print("Re-assigning Additional Kmeans Clusters Ids (not assigned) ..")
    for (k in unique_kmeans_ids) {
      last_class_id <- last_class_id +1
      #print(paste0("    Kmeans Cluster id=",k," must be changed to=",last_class_id))
      
      res[as.numeric(k)] <-  last_class_id
    }
  }
  
  l <- length(res)
  
  for (i in 1:l) {
    # Put temporary values to change later
    kmeans_ids[kmeans_ids == i] <- i*100
  }
  
  #print("Doing Changes ...")
  
  for (i in 1:l) {
    #print(paste0("    Changing kmean_cluster=",i," to ",res[i]))
    kmeans_ids[kmeans_ids == (i*100)] <- res[i]
  }
  kmeans_ids
}


# To count missclassificationsfind the best k we will loop 
count_missclassifications <- function(class_ids,kmeans_ids){
  
  # Correct the kmeans indices
  new_kmeans_ids <- correct_kmeans_ids(class_ids,kmeans_ids)
  
  classif_results <- (new_kmeans_ids == class_ids)
  
  length(classif_results[classif_results == FALSE])
}

# To find the best k we will loop using different cluster values and record the results,
# we will use this function
create_kmeans_clusters <- function(data, method, k_max, type_data, var1, var2) {
  
  total_ss <- NULL
  clusters_plots <- list()
  res <- list()
  
  # We are assuming to plot 8
  par(mfrow = c(4, 2))
  
  for (k in 1:k_max) {
    
    xx.km <- eclust(data,k=k,hc_method=method, nstart=10, graph = FALSE)
    
    main <- paste0("Kmeans cluster with k=",k,type_data,"\nDist=",method)
    
    # Avoid empty clusters, this function checks that do not exists clusters with 0
    if (!has_empty_cluster(xx.km$size)) {
      
      plot(data, col=xx.km$cluster, pch=16, cex=1.5, frame = FALSE,
           main = main)
      total_ss[k] <- sum(xx.km$withinss)
      points(xx.km$centers, pch=11, col='black', cex=2)
      
      # To visualice later the cluster plots using the function fviz_cluster from factoextra
      
      #clusters_plots[[k]] <- fviz_cluster(xx.km, data = data, main = main, 
      #                                    choose.vars = c(var1,var2), stand = FALSE) #No standarize the data
      
      temp_data <- data.frame(data)
      temp_data$cluster <- xx.km$cluster
      
      clusters_plots[[k]] <- ggplot(data=temp_data, aes(temp_data[[var1]], temp_data[[var2]],
                                                        color = as.factor(cluster))) + 	                                          
        scale_color_manual(values = 1:k, name ="Cluster") +
        geom_point() +
        labs(title = main, x = var1, y = var2) +
        geom_text(aes(label = 1:nrow(temp_data)), size = 3, vjust = -0.7) +
        stat_chull(aes(group = as.factor(cluster)),geom = "polygon", fill = NA)
    }
  }
  
  res$total_ss <- total_ss
  res$clusters_plots <- clusters_plots
  res
}

# Function to plot the clusters 
plot_clusters <- function(clusters_plots) {
  
  # We are assuming to plot 8
  #par(mfrow = c(4, 2))
  for (i in c(1,3,5,7)) {
    grid.arrange(
      clusters_plots[[i]], 
      clusters_plots[[i+1]], ncol = 2)
  }
}


# This function calculate the best seed (minimun classification errors and minimun sum(whithinss)) 
# for a specific range of seeds and a value of k
find_best_seed_kv2 <- function(data, method, initial_seed, final_seed, k, real_class) {
  all_errors <- NULL
  best_km <- NULL
  min_errors <- 0
  best_seed <- 0
  num_cluster <- 0
  
  for (seed in initial_seed:final_seed) {
    #The eclust function set the seed
    xx.km <- eclust(data,k=k,hc_method=method, nstart=10, graph = FALSE, seed = seed)
    
    if(has_empty_cluster(xx.km$size)) {
      # Has an empty cluster, do not consider
      print(paste0("Empty Cluster ... seed:",seed))
      
    } else {
      num_cluster <- num_cluster + 1
      
      xx.km$cluster <- correct_kmeans_ids(real_class,xx.km$cluster)
      
      current_errors <- count_missclassifications(real_class,xx.km$cluster)
      current_tot_withinss <- sum(xx.km$withinss)
      
      all_errors[num_cluster] <- current_errors  
      
      #print(paste0("Total Whithin", current_tot_withinss))
      if ( (seed==initial_seed) || 
           (current_errors < min_errors) || 
           (current_errors == min_errors && (current_tot_withinss < min_tot_withinss ))) {
        best_seed <- seed
        min_errors <- current_errors
        min_tot_withinss <- current_tot_withinss
        print(paste0("Best Seed find: k=",k," seed=",seed," errors=", current_errors," sum(whithinss)=",min_tot_withinss))
        # Save the info related with the best cluster
        best_km <- xx.km
      }
    }
  }
  
  res <- list("all_errors" = all_errors,  
              "best_seed" = best_seed,
              "best_km" = best_km,
              "min_errors" = min_errors
  )
}


# This function calculate the best seed (minimun classification errors and minimun sum(whithinss)) 
# for a specific range of seeds and a range value of k

find_best_seedv2 <- function(data, method, min_seed, max_seed, min_k, max_k, real_class) {
  
  res <- list()
  best_seeds <- NULL
  min_errors <- 0
  
  for (k in min_k:max_k) {
    print(paste0("Results for k=",k))
    print("=======================")
    best_seed_k <- find_best_seed_kv2(data,method,min_seed,max_seed,k,real_class)
    
    current_errors <- best_seed_k$min_errors
    current_tot_withinss <- sum(best_seed_k$best_km$withinss) 
    
    if ( (k == min_k) || 
         (current_errors < min_errors) ||
         (current_errors == min_errors && (current_tot_withinss < min_tot_withinss ))) {
      
      res$best_seed <- best_seed_k$best_seed
      res$best_k <- k
      res$min_errors <- current_errors
      res$best_km <- best_seed_k$best_km
      min_errors <- current_errors
      min_tot_withinss <- current_tot_withinss
    }
    print("")
  }
  print("")
  print("Final Results")
  print("==============")
  print(paste0("Best Seed find: k=",res$best_k," seed=",res$best_seed," errors=", 
               res$min_errors," sum(whithinss)=",min_tot_withinss))
  
  res
}

# Function to count the number of rows in a dataframe in a specific column based on one condition

count_rows <- function(data,column,value) {
  nrow(data[which(data[[column]] == value),])  
}


# Function to count the number of rows in a dataframe in a specific column based on one condition

count_rows <- function(data,column,value) {
  nrow(data[which(data[[column]] == value),])  
}

# Function to Calculate the percentage of rows in a data.frame in a specific column based on one condition 

per_rows <- function(data,column,value) {
  count_rows(data,column,value) / nrow(data)
}	

# Function to Saving (Convert) the Code from the Report (\Report\REVERON-Enrique-STAT5703-AS-2.Rmd)
# to (\Code\101066270-AS-2 Main.r) including documentation and changing the print functions:
#		print_kable() -> print_table()
#		print_rules_kable() -> print_rules_table()

convert_to_main.r <- function() {
  
  rmd.file <- paste(report.dir, "Research-Project.Rmd", sep="/")
  r.function.file <- paste(code.dir, "Research-Project Main.r", sep="/")
  
  if (!file.exists(rmd.file)) {
    stop("no valid Rmd data file:", rmd.file)
  }
  
  # Convert the .Rmd file to .r
  #purl(rmd.file, output = r.function.file, documentation = 2)
  purl(rmd.file, output = r.function.file, documentation = 0)
  
  # Make sustitutions
  temp <- readLines(r.function.file)
  # Change 'print_kable' to 'print_table'
  temp <- gsub("^print_kable\\(", "print_table(", temp)
  # Change 'print_rules_kable' to 'print_rules_table'
  temp <- gsub("^print_rules_kable\\(", "print_rules_table(", temp)
  # Comment call to 'library(knitr)'
  temp <- gsub("^library\\(knitr\\)", "#library(knitr)", temp)
  # Comment calls to 'include_graphics('
  temp <- gsub("^include_graphics\\(", "#include_graphics(", temp)
  # Comment call to 'knitr::opts_chunk'
  temp <- gsub("^knitr::opts_chunk", "#knitr::opts_chunk", temp)
  # Comment calls to 'options('
  temp <- gsub("^options\\(", "#options(", temp)
  
  # Update the file
  cat(temp, file=r.function.file, sep="\n")
}

####################################
# For print elapsed time
elapsed_time <- function(tic1,tic2) {
  format((tic2-tic1)[3][[1]], digits = 2)
}

####################################################################################

print("successful!")

