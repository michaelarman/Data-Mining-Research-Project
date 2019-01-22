# 7. Applications 
## 7.1 Calling Neccesary Libraries 
l_packages = c("pixmap","factoextra","caret","ggplot2","ggpubr",
               "knitr","kableExtra","formatR", "xtable") # used to create the Report

for(p in l_packages){
  if (!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)  
}
## 7.2 Setting Up the Directory and Variables for Reproducibility
# Student and Assigment Information Variables
Student.Number <- "100978616"
ASorLAB <- "RESEARCH"
Student.info <- paste(Student.Number, ASorLAB, sep="-")
# Folder Variables
drive="C:"
path.upto <- paste("Users", "Enrique","Documents", 
                   "Carleton", "Winter 2018", "STAT5703 Data Mining I", sep="/" )
code.dir <- paste(drive, path.upto, Student.info, "Code", sep="/")
data.dir <- paste(drive, path.upto, Student.info, "Data", sep="/")
img.src.dir <- paste(drive, path.upto, Student.info, "Data", "ImgSrc", sep="/")
img.stego.dir <- paste(drive, path.upto, Student.info, "Data", "Stego", sep="/")
work.dir <- paste(drive, path.upto, Student.info, "Work", sep="/")
report.dir <- paste(drive, path.upto, Student.info, "Report", sep="/")
setwd(work.dir)
getwd()
# For reproducibility
set.seed(12345)
## 7.3 Calling Neccesary Functions
source(paste(code.dir, paste(Student.info, "functions.r", sep=" "), sep="/"))
lsf.str()
## ## 7.4 Image Steganography (*stegasaur*)
## ### 7.4.1 Installation
## # First needs to be installed, is not availaible trought RStudio so we need
## # to installed manually
## install.packages("devtools")
## library(devtools)
## install_github("richfitz/stegasaur")
### 7.4.2 Getting and Image (png) and Loading in R
# Download a png file, for example this
library(utils)
URL <- "https://pbs.twimg.com/profile_images/537625264277028864/14jJPXpX_400x400.png"
# and put it in the working dir, a called "canada_flag.png"
download.file(URL,"canada_flag.png", mode ="wb")
getwd()

library(png)
# Load the png image from working dir
img <- png::readPNG("canada_flag.png")

# Write and load the file to create a baseline
png::writePNG(img,"canada_flag.png")
img <- png::readPNG("canada_flag.png", info = TRUE)

# Details about the file
str(img)
kable(file.info("canada_flag.png"))

# 256 x 256, 4 bits

# Show the original file

plot(0, type='n', xlim=0:1, ylim=0:1, main="Original Image", axes = FALSE,
     xlab = "", ylab = "")
rasterImage(img,0,0,1,1)
### 7.4.3 Example with Plain Text
#### 7.4.3.1 Encoding the Message
library(stegasaur)
(msg1 <- "This is a test of Steganography using K library stegasaur LSB")
# Encode the text in the image using LSB
img2 <- stegasaur::lsb_encode(msg1, img)

# Write the new image to the working dir
png::writePNG(img2,"canada_flag2.png")
kable(file.info("canada_flag2.png"))

# Load the file to plot
img2 <- png::readPNG("canada_flag2.png", info = TRUE)
# Details about the file
str(img2)

# 256 x 256, 4 bits

# Show the New Image
plot(0, type='n', xlim=0:1, ylim=0:1, main="New Image", axes = FALSE,
     xlab = "", ylab = "")
rasterImage(img2,0,0,1,1)

#### 7.4.3.2 Decoding the Message
# Load the image with the message encoded
img2 <- png::readPNG("canada_flag2.png", info = TRUE)
kable(file.info("canada_flag2.png"))
dim(img2)

# Decode the message
stegasaur::lsb_decode(img2)

### 7.4.4 Example with an R Object
#### 7.4.4.1 Encoding the Message
library(stegasaur)
(msg2 <- data.frame(ID = c("101066270","1010XXXXXX","1010XXXXXX","101066XXXX"), 
                    NAME = c("Enrique","Muneer","Alex","Michael")))

# Load the png image from working dir
img <- png::readPNG("canada_flag.png")

# Encode the text in the image using LSB
img3 <- stegasaur::lsb_encode(msg2, img)

# Write the new image to the working dir
png::writePNG(img3,"canada_flag3.png")

# Load the file to plot
img3 <- png::readPNG("canada_flag3.png")
# Details about the file
dim(img3)
kable(file.info("canada_flag3.png"))

# 256 x 256, 4 bits

# Show the New Image
plot(0, type='n', xlim=0:1, ylim=0:1, main="New Image", axes = FALSE,
     xlab = "", ylab = "")
rasterImage(img3,0,0,1,1)

#### 7.4.4.2 Decoding the Message
# Load the image with the message encoded
img3 <- png::readPNG("canada_flag3.png")
dim(img3)

# Decode the message
x <- stegasaur::lsb_decode(img3)
x
is.data.frame(x)
## ### 7.4.5 Source Code
## #### LSB
## # Exported Functions
## ##' Takes an image matrix from something like \code{readPNG}.  Note
## ##' that this cannot be saved out as jpeg as the lossy compression
## ##' will drop the message
## ##'
## ##' @title Encode text into a lossless image
## ##' @param content Content to save into the image; can be a text
## ##' string or an arbitrary R object.
## ##' @param img An image matrix to save the message into
## ##' @param force_object Logical: Force saving a scalar text string as
## ##' an R object (will be slightly more space efficient).
## ##' @export
## ##' @author Rich FitzJohn
## ##' @importFrom png readPNG
## ##' @examples
## ##' img <- png::readPNG(system.file("img/Rlogo.png", package="png"))
## ##' txt <- "hello from stegasaur"
## ##' img2 <- lsb_encode(txt, img)
## ##' lsb_decode(img2)
## lsb_encode <- function(content, img, force_object=FALSE) {
##   text <- !force_object && is.character(content) && length(content) == 1L
##   if (text) {
##     content <- utf8ToInt(content)
##   } else {
##     content <- serialize(content, NULL)
##   }
## 
##   img <- lsb_prepare(img)
##   bits <- c(binvalue(length(content), LSB_BITSIZE_LEN),
##             as.integer(text),
##             binvalue(content, LSB_BITSIZE_CHAR))
##   ret <- put_binary_value(bits, img)
##   ret / 255
## }
## 
## ##' @export
## ##' @rdname lsb_encode
## lsb_decode <- function(img) {
##   img <- lsb_prepare(img)
##   i <- seq_len(LSB_BITSIZE_LEN)
##   len <- intvalue(img[i] %&% 1L, LSB_BITSIZE_LEN)
##   is_text <- as.logical(img[LSB_BITSIZE_LEN + 1L] %&% 1L)
## 
##   bits <- img[seq_len(LSB_BITSIZE_CHAR * len) + LSB_BITSIZE_LEN + 1L] %&% 1L
##   bytes <- intvalue(bits, LSB_BITSIZE_CHAR)
## 
##   if (is_text) {
##     intToUtf8(bytes)
##   } else {
##     unserialize(as.raw(bytes))
##   }
## }
## 
## LSB_BITSIZE_LEN  <- 16L
## LSB_BITSIZE_CHAR <- 8L # also for raw
## INT_LEN <- length(intToBits(0L)) # 32L
## 
## binvalue <- function(val, bitsize) {
##   b <- matrix(as.integer(intToBits(val)), INT_LEN)
##   i <- seq_len(bitsize)
##   if (sum(b[-i, ])) {
##     stop("Overflow detected")
##   }
##   c(b[i, ])
## }
## 
## intvalue <- function(bits, bitsize) {
##   m <- matrix(as.integer(bits), bitsize)
##   m <- rbind(m, matrix(0L, INT_LEN - bitsize, ncol(m)))
##   packBits(m, "integer")
## }
## 
## put_binary_value <- function(bits, img) {
##   if (length(bits) > length(img)) {
##     stop("not enough space")
##   }
##   i <- seq_along(bits)
##   j <- bits == 1L
##   k <- !j
##   img[i][j] <- img[i][j] %|% 1L
##   img[i][k] <- img[i][k] %&% 254L
##   img
## }
## 
## lsb_prepare <- function(img) {
##   if (is.double(img)) {
##     img <- img * 255
##     storage.mode(img) <- "integer"
##   }
##   img
## }
## 7.5 Statistical Data Mining Image Steganalysis
### 7.5.2 Steganopraphy Algorithm / Creation of Stego Images
# For each image (\\Data\\ImgSrc\\[1-200].pgm) we will select randomly an object an generate
# an Stego Image (\\Data\\Stego\\[1-200]-Stego.pgm)

file.name <- paste(work.dir, "stego_labels.Rds", sep="/")
file.name.et <- paste(work.dir, "stego_labels-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv) 
  load(file.name.et,.GlobalEnv)
} else {
  
  t1 <- proc.time()
  
  # Create a Set of Messages to Encode
  stego.data <- list(msg1,msg2) 
  # The text message
  object.size(msg1)
  # The data.frame
  object.size(msg2)
  # To print only the ten first images  
  j <- 1
  # To save the kind of stego in each Stego-image
  stego.labels <- NULL
  
  if (!dir.exists(img.stego.dir)) {
    dir.create(img.stego.dir)
  }
  
  par(mfrow=c(1,2))
  
  print("Creating Stego Images ....")
  for (i in 1:200) {
    
    msg_id <- sample(c("a","b"),1)
    
    msg <- ifelse(msg_id == "a", stego.data[1], stego.data[2])
    
    img <- read.pnm(paste0(img.src.dir,"\\",i,".pgm"))
    
    print(paste0("Loading Image: ",i,".pgm ..."))
    print("============================")
    
    if (j <= 10) {
      # Only show in the Report the first 10 Images
      plot(img, main = paste0("Original Image (Src)\n",i,".pgm"))
    }
    
    print("Encoding Message:")
    print(msg)
    print(paste0("Into image file:",i))
    
    img@grey <- stegasaur::lsb_encode(msg, img@grey)
    
    if (j <= 10) {
      # Only show in the Report the first 10 Images
      print("New Image:")
      plot(img, main = paste0("Encoded Image (Stego)\n",i,"-Stego.pgm"))
      print("Decoding Message:")
      (x <- stegasaur::lsb_decode(img@grey))
      print(x)
      j <- j+1
    } 
    
    print(paste0("Saving Stego Image:", img.stego.dir,"/",i,"-Stego.pgm"))
    write.pnm(img,paste0(img.stego.dir,"\\",i,"-Stego",msg_id,".pgm"))
    stego.labels[i] <- paste0(i,"-Stego",msg_id)
  }
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0("Stego Images Created ... time:",et))
  save(stego.labels, file = file.name)
  save(et, file = file.name.et)
}

## %% If it will be running against the stego images
## stego = 1
## for i = 1:200
##   myFile = dir(strcat(int2str(i),'-Stego*.pgm'))
##   disp(myFile.name)
##   x = spam686(myFile.name)
## 
##   if i == 1
##      res = transpose(x)
##   else
##      res = [res ; transpose(x)]
##   end
## end
## 
## if (stego == 1)
##   csvwrite('SPAM-Stego.csv',res)
## else
##   csvwrite('SPAM-ImgSrc.csv',res)
## end
feature.src.data.file <- paste(img.src.dir, "SPAM-ImgSrc.csv", sep="/")
feature.stego.data.file <- paste(img.stego.dir, "SPAM-Stego.csv", sep="/")

dt.imgsrc <- read.table(feature.src.data.file, sep = ",")
# Class = 1 No Stego Image
dt.imgsrc$class <- 1
row.names(dt.imgsrc) <- paste0(row.names(dt.imgsrc),"-Src")

dt.imgstego <- read.table(feature.stego.data.file, sep = ",")
# Class = 2 Stego Image
dt.imgstego$class <- 2
row.names(dt.imgstego) <- stego.labels


# Some details about the data
dim(dt.imgsrc)
kable(dt.imgsrc[1:5,c(1:5,687)])

str(dt.imgsrc[,c(1:5,687)])
### 7.5.4 Split the Dataset into Train and Data
(n.var <- length(dt.imgsrc))
# The firsts 686 variables are the predictors and the 687 is the class

(l.class <- dim(dt.imgsrc)[1])

(Train.class <- round(l.class * 2 / 3, digits = 0))

# Get the indices for the training and test samples
tt.ind.class <- get.train(dim(dt.imgsrc)[1], Train.class)

# Let's sort the values for easy visualization
tt.ind.class$train <- sort(tt.ind.class$train)
tt.ind.class$test <- sort(tt.ind.class$test)
tt.ind.class
# Length of the Train and Test 
length(tt.ind.class$train)
length(tt.ind.class$test)

# Find Train and Test Sets
train.X.src.class <- dt.imgsrc[tt.ind.class$train,]

dim(train.X.src.class)

train.X.stego.class <- dt.imgstego[tt.ind.class$train,]
dim(train.X.stego.class)

test.X.src.class <- dt.imgsrc[tt.ind.class$test,]
dim(test.X.src.class)

test.X.stego.class <- dt.imgstego[tt.ind.class$test,]
dim(test.X.stego.class)

# Put all Together, first the train Set
train.X.all <- rbind (train.X.src.class, train.X.stego.class)
dim(train.X.all)

train.Y.all <- train.X.all$class
length(train.Y.all)

# Remove class from train.X.all set
train.X.all <- train.X.all[,1:(n.var-1)]

# The best results we get using the std data, let's transform the data
train.X.all.std <- f.data.std(train.X.all)
train.X.all.sp <- Sphere.Data(train.X.all)


# Do the same for test set
test.X.all <- rbind (test.X.src.class, test.X.stego.class)
test.Y.all <- test.X.all$class
# Remove class from train.X. set
test.X.all <- test.X.all[,1:(n.var-1)]

# Std the data to have better results
test.X.all.std <- f.data.std(test.X.all)
test.X.all.sp <- Sphere.Data(test.X.all)
### 7.5.5 Clustering Using K-means 
#### 7.5.5.1 PCA
pc.train <- prcomp(as.matrix(train.X.all.std), scale = TRUE, center = TRUE)

# Eigenvalues 
eig.val <- get_eig(pc.train)

# Let's see the first 60
kable(eig.val[1:60,])

fviz_screeplot(pc.train, addlabels = TRUE, ncp=20)

# Extract the results for variables
var <- get_pca_var(pc.train)

kable(print(var))

# Let see the model generated (first five dimension and 10 variables)
kable(var$coord[1:5,1:5])

# Let see the contribution of each variable to each PC
kable(var$contrib[1:5,1:5])

# Contributions of variables to PC1
fviz_contrib(pc.train, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(pc.train, choice = "var", axes = 2, top = 10)

# Contributions of variables to PC3
fviz_contrib(pc.train, choice = "var", axes = 3, top = 10)

# Extract the results for individuals
ind <- get_pca_ind(pc.train)

# Let's use the first 60 variables for the model
xx.pc.train <- as.matrix(ind$coord[,1:60])
min_k <- 2
max_k <- 4
min_seed <- 1
max_seed <- 100
method <- "euclidean"

# Find Best Seed and K Using the PCA Data on training 
file.name  <- paste(work.dir, "best_seeds_std_pca.Rds", sep="/")
file.name.et  <- paste(work.dir, "best_seeds_std_pca-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  t1 <- proc.time()
  
  best_seeds_std_pca <- find_best_seedv2(xx.pc.train,
                                         method,
                                         min_seed,
                                         max_seed,
                                         min_k,
                                         max_k,
                                         train.Y.all)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0("Best Seed for Kmeans PCA Finded ... time:",et))
  save(best_seeds_std_pca, file = file.name)
  save(et, file = file.name.et)
}
(k <- best_seeds_std_pca$best_k)
(best_seed <- best_seeds_std_pca$best_seed)

res.train.kmeans_pca <- confusionMatrix(best_seeds_std_pca$best_km$cluster, train.Y.all)
res.train.kmeans_pca

# Let's see the Complete Classification/Missclassification Considering the Type the next
# function will calculate that

plot_results_by_type <- function(predict, real.class, main) {
  
  new.set <- data.frame(predict) 
  new.set$classification <- new.set == real.class
  # Type = 1 ImgSrc (Clean), Type =2 Stegoa, Type =3 Stegob
  new.set$type <- ifelse(grepl("-Src",row.names(new.set)),1, 
                         ifelse(grepl("-Stegoa",row.names(new.set)),2,3))
  new.set$factorC <- with(new.set, 
                          interaction(factor(classification),  factor(type)))
  plot(new.set$factorC, main = main, col = c(1,1,2,2,3,3))
}


pc.test <- prcomp(as.matrix(test.X.all.std), scale = TRUE, center = TRUE)

# Eigenvalues 

eig.val <- get_eig(pc.test)

# Let's see the first 60
kable(eig.val[1:60,])

fviz_screeplot(pc.test, addlabels = TRUE, ncp=20)

# Extract the results for variables
var <- get_pca_var(pc.test)

kable(print(var))

# Let see the model generated (first five dimension and 10 variables)
kable(var$coord[1:5,1:5])

# Let see the contribution of each variable to each PC
kable(var$contrib[1:5,1:5])

# Contributions of variables to PC1
fviz_contrib(pc.test, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(pc.test, choice = "var", axes = 2, top = 10)

# Contributions of variables to PC3
fviz_contrib(pc.test, choice = "var", axes = 3, top = 10)

# Extract the results for individuals
ind <- get_pca_ind(pc.test)

# Let's use the first 60 variables for the model
xx.pc.test <- as.matrix(ind$coord[,1:60])

xx.km <- eclust(xx.pc.test,k=k,hc_method=method, nstart=10, graph = FALSE, seed = best_seed)
kmeans_class_pca <- correct_kmeans_ids(test.Y.all,xx.km$cluster)

res.test.kmeans_pca <- confusionMatrix(kmeans_class_pca, test.Y.all)
res.test.kmeans_pca



main <- paste0("Kmeans-PCA Classification/Misclassification by Type (Train Set) \n",
               " (1=ImgSrc, 2= Stegoa, 3= Stegob)")

plot_results_by_type(best_seeds_std_pca$best_km$cluster,
                     train.Y.all,
                     main)

main <- paste0("Kmeans-PCA Classification/Misclassification by Type (Test Set)\n",
               " (1=ImgSrc, 2= Stegoa, 3= Stegob)")

plot_results_by_type(xx.km$cluster,
                     train.Y.all,
                     main)



# RMSE
(rmse_kmeans_pca <- sqrt(mean((as.numeric(kmeans_class_pca)-as.numeric(test.Y.all))^2)))

# Store the Values for Report
results_all <- data.frame("Method"= "Kmeans Clustering PCA",
                          "Prediction Accuracy in Training Set" = res.train.kmeans_pca$overall[1] ,
                          "Prediction Accuracy in Test Set" =  res.test.kmeans_pca$overall[1],
                          "RMSE Test"= rmse_kmeans_pca,
                          "Time Elapsed" = et,
                          stringsAsFactors = FALSE)
fviz_cluster(xx.km)

#This function will be used to plot the results 

plot_results <- function(X.data, Y.data, Y.predict, title, var1 = NULL, var2 = NULL, labels = TRUE) {
  
  res <- data.frame(X.data)
  res$class <- Y.data
  res$ID <- row.names(res)
  res$predict_class <- Y.predict
  res$classification_ok <- (res$class == res$predict_class)
  res$stego <- grepl("-Stego",res$ID)
  
  if (is.null(var1)) {
    var1 <- colnames(res)[1]
  }
  
  if (is.null(var2)) {
    var2 <- colnames(res)[2]
  }
  
  g <- ggplot(res,
              aes(res[[var1]], res[[var2]],
                  shape = as.factor(
                    ifelse(res$classification_ok, res$class,
                           as.numeric(res$predict_class) + 2 )))) +
    scale_color_manual(values = c(1,2), 
                       name ="Real Class (Color)",
                       labels =c("Black (Class 1)","Red (Class 2)")) +
    scale_shape_manual(values=c(15,16,0,1),
                       name ="Classification/Missclassification (Shape)",
                       labels=c("Classified as Class 1 (OK)", "Classified as Class 2 (OK)",
                                "Classified as Class 1 (Wrong)","Classified as Class 2 (Wrong)")) +
    geom_point() + 
    
    aes(color = as.factor(res$class)) +
    
    labs(title = title) + xlab(var1) + ylab(var2) + 
    
    stat_chull(aes(group = as.factor(res$predict_class),
                   color = as.factor(res$predict_class)),
               geom = "polygon", fill = NA)
  
  if (labels) {
    # Add the labels to the graph
    g <- g + geom_text(aes(label = res$ID),
                       size = 3, 
                       vjust = ifelse(res$stego, 0.7,-0.7),
                       color = res$class)
  }
  g
}

# Let's find some good variables to plot
vars <- find_unique_variables(xx.pc.test)

plot_results(xx.pc.test, test.Y.all, kmeans_class_pca, 
             paste0("Kmeans Clustering Classification/Missclasiffication (PCA)\n"),
             "Dim.1","Dim.2", labels = TRUE)

plot_results(xx.pc.test, test.Y.all, kmeans_class_pca, 
             paste0("Kmeans Clustering Classification/Missclasiffication (PCA)\n"),
             "Dim.1","Dim.2", labels = FALSE)

#### 7.5.6.1 All Data Whithout PCA
min_k <- 2
max_k <- 4
min_seed <- 1
max_seed <- 100
method <- "euclidean"

# Find Best Seed and K and Kmeans for the all Std Data
file.name  <- paste(work.dir, "best_seeds_std_m.Rds", sep="/")
file.name.et  <- paste(work.dir, "best_seeds_std_m-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv) 
  load(file.name.et,.GlobalEnv)
} else {
  t1 <- proc.time()
  best_seeds_std_m <- find_best_seedv2(train.X.all.std,
                                       method,
                                       min_seed,
                                       max_seed,
                                       min_k,
                                       max_k,
                                       train.Y.all)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0("Best Seed for Kmeans Finded ... time:", et))
  save(best_seeds_std_m, file = file.name)
  save(et, file = file.name.et)
}
(k <- best_seeds_std_m$best_k)
(best_seed <- best_seeds_std_m$best_seed)

res.train.kmeans <- confusionMatrix(best_seeds_std_m$best_km$cluster, train.Y.all)
res.train.kmeans

# Use the k and best seeds in the test set

xx.km2 <- eclust(test.X.all.std,k=k,hc_method=method, nstart=10, graph = FALSE, seed = best_seed)

kmeans_class <- correct_kmeans_ids(test.Y.all,xx.km2$cluster)

res.test.kmeans <- confusionMatrix(kmeans_class, test.Y.all)
res.test.kmeans


main <- paste0("Kmeans Classification/Misclassification by Type (Train Set) \n",
               " (1=ImgSrc, 2= Stegoa, 3= Stegob)")

plot_results_by_type(best_seeds_std_m$best_km$cluster,
                     train.Y.all,
                     main)

main <- paste0("Kmeans Classification/Misclassification by Type (Test Set)\n",
               " (1=ImgSrc, 2= Stegoa, 3= Stegob)")

plot_results_by_type(xx.km2$cluster,
                     train.Y.all,
                     main)

# RMSE
(rmse_kmeans <- sqrt(mean((as.numeric(kmeans_class)-as.numeric(test.Y.all))^2)))

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= "Kmeans Clustering",
                                "Prediction Accuracy in Training Set" = res.train.kmeans$overall[1] ,
                                "Prediction Accuracy in Test Set" =  res.test.kmeans$overall[1],
                                "RMSE Test"= rmse_kmeans,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
fviz_cluster(xx.km2)

vars <- find_unique_variables(test.X.all.std)

plot_results(test.X.all.std, test.Y.all, kmeans_class, 
             paste0("Kmeans Clustering Classification/Missclasiffication (All Data)\n"),
             "V1","V2", labels = TRUE)

plot_results(test.X.all.std, test.Y.all, kmeans_class, 
             paste0("Kmeans Clustering Classification/Missclasiffication (All Data)\n"),
             "V1","V2", labels = FALSE)
### 7.5.7 Classification Using Support Vector Machine (SVM)
### 7.5.7.1 SVM Radial (Gaussian Kernel)
(d <- log2(686))
(sigma_values <- 2^(c(-d-3, -d-2, -d-1, -d, -d + 1, -d +2, -d+3)))
(c_values <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000))
kernel_method_type <- "svmRadial"
# Grid Parameters
(grid_radial <- expand.grid(C = c_values, sigma = sigma_values))
# Cross Validation Parameters
trctrl <- trainControl(method = "repeatedcv", 
                       number = 10,
                       repeats = 3)


file.name  <- paste(work.dir, "svm_radial.Rds", sep="/")
file.name.et  <- paste(work.dir, "svm_radial-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv) 
  load(file.name.et,.GlobalEnv)
} else {
  t1 <- proc.time()
  svm_Radial_Grid <- train(y = factor(train.Y.all), 
                           x = train.X.all, 
                           method = kernel_method_type,
                           trControl=trctrl,
                           preProcess = c("center", "scale"), 
                           trace = FALSE,
                           tuneGrid = grid_radial)
  t2 <- proc.time()
  et <-  elapsed_time(t1,t2)
  print(paste0("SVM-Radial Executed ... time:",et))
  save(svm_Radial_Grid, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
svm_Radial_Grid 
plot(svm_Radial_Grid, main = "SVM Radial")

# Let's check the detail in the Training Set
res.svmradial.training <- confusionMatrix(svm_Radial_Grid$finalModel@fitted, train.Y.all)
res.svmradial.training

# Let's use the model in test set
test_pred_rgrid <- predict(svm_Radial_Grid, newdata = test.X.all)
test_pred_rgrid

res.svmradial <- confusionMatrix(test_pred_rgrid, test.Y.all)

res.svmradial


main <- paste0("SVM-Radial Classification/Misclassification by Type (Train Set) \n",
               " (1=ImgSrc, 2= Stegoa, 3= Stegob)")
data_radial_train <- data.frame(svm_Radial_Grid$finalModel@fitted)
row.names(data_radial_train) <- row.names(train.X.all)

plot_results_by_type(data_radial_train,
                     train.Y.all,
                     main)

data_radial_test <- data.frame(test_pred_rgrid)
row.names(data_radial_test) <- row.names(test.X.all)


main <- paste0("SVM-Radial Classification/Misclassification by Type (Test Set)\n",
               " (1=ImgSrc, 2= Stegoa, 3= Stegob)")

plot_results_by_type(data_radial_test,
                     train.Y.all,
                     main)


# RMSE Resulting
(rmse_svmradial <- sqrt(mean((as.numeric(test_pred_rgrid)-as.numeric(test.Y.all))^2)))

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= "SVM Gausian Kernel",
                                "Prediction Accuracy in Training Set" = res.svmradial.training$overall[1] ,
                                "Prediction Accuracy in Test Set" =  res.svmradial$overall[1],
                                "RMSE Test"= rmse_svmradial,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))

plot_results(test.X.all, test.Y.all, test_pred_rgrid , 
             paste0("SVM Classification/Missclasiffication (RDF)\n"),
             "V1","V2")

plot_results(test.X.all, test.Y.all, test_pred_rgrid , 
             paste0("SVM Classification/Missclasiffication (RDF)\n"),
             "V1","V2", labels = FALSE)

### 7.5.7.2 Linear SVM 
# Setting Hyperparameters According with Paper

kernel_method_type <- "svmLinear"

grid_Linear <- expand.grid(C = c_values)

file.name  <- paste(work.dir, "svm_linear.Rds", sep="/")
file.name.et  <- paste(work.dir, "svm_linear-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv) 
  load(file.name.et,.GlobalEnv)
} else {
  t1 <- proc.time()
  svm_Linear_Grid <- train(y = factor(train.Y.all), 
                           x = train.X.all,
                           method = kernel_method_type,
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid_Linear,
                           trace = FALSE,
                           tuneLength = 10)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0("SVM Linear Executed ... time:", et))
  save(svm_Linear_Grid, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
svm_Linear_Grid 
plot(svm_Linear_Grid, main = "SVM Linear")

# Let's check the detail in the Training Set
res.svmlinear.training <- confusionMatrix(svm_Linear_Grid$finalModel@fitted, train.Y.all)
res.svmlinear.training

# Let's use the model in test set

test_pred_lgrid <- predict(svm_Linear_Grid, newdata = test.X.all)
test_pred_lgrid

res.svmlinear <- confusionMatrix(test_pred_lgrid, test.Y.all)

res.svmlinear

main <- paste0("SVM-Linear Classification/Misclassification by Type (Train Set) \n",
               " (1=ImgSrc, 2= Stegoa, 3= Stegob)")

data_linear_train <- data.frame(svm_Linear_Grid$finalModel@fitted)
row.names(data_linear_train) <- row.names(train.X.all)

plot_results_by_type(data_linear_train,
                     train.Y.all,
                     main)

main <- paste0("SVM-Linear Classification/Misclassification by Type (Test Set)\n",
               " (1=ImgSrc, 2= Stegoa, 3= Stegob)")

data_linear_test <- data.frame(test_pred_lgrid)
row.names(data_linear_test) <- row.names(test.X.all)

plot_results_by_type(data_linear_test,
                     train.Y.all,
                     main)


# RMSE Resulting
(rmse_svmlinear <- sqrt(mean((as.numeric(test_pred_lgrid)-as.numeric(test.Y.all))^2)))

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= "SVM Linear Kernel",
                                "Prediction Accuracy in Training Set" = res.svmlinear.training$overall[1] ,
                                "Prediction Accuracy in Test Set" =  res.svmlinear$overall[1],
                                "RMSE Test"= rmse_svmlinear,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
### 7.5.7 Classification Using Neural Networks (NN)
file.name  <- paste(work.dir, "nnet.Rds", sep="/")
file.name.et  <- paste(work.dir, "nnet-et.Rds", sep="/")

if (file.exists(file.name)) {
  load(file.name,.GlobalEnv)
  load(file.name.et,.GlobalEnv)
} else {
  
  grid_n_net <- expand.grid(decay = c(0.0005, 0.005, 0.05), size = c(1,2,3))
  t1 <- proc.time()
  n_net <- train(y = factor(train.Y.all), 
                 x = train.X.all,
                 method="nnet",
                 preProcess = c("center", "scale"),
                 tuneGrid = grid_n_net,
                 trace = FALSE,
                 maxit = 500)
  t2 <- proc.time()
  et <- elapsed_time(t1,t2)
  print(paste0("Neural Network Executed ... time:", et))
  save(n_net, file = file.name)
  save(et, file = file.name.et)
}
# Let's see some details about the model
n_net 
plot(n_net, main = "Neural Network")

# Let's check the detail in the Training Set (I was not able to find the fitted values in nnet)
#res.n_net.training <- confusionMatrix(n_net$finalModel$fitted.values, train.Y.all)
#res.n_net.training

# Let's use the model in test set
test_pred_n_net <- predict(n_net, newdata = test.X.all)
test_pred_n_net

res.n_net <- confusionMatrix(test_pred_n_net, test.Y.all)
res.n_net

# RMSE Resulting
(rmse_n_net <- sqrt(mean((as.numeric(test_pred_n_net)-as.numeric(test.Y.all))^2)))

# Store the Values for Report
results_all <- rbind(results_all,
                     data.frame("Method"= "Neural Networks",
                                "Prediction Accuracy in Training Set" = 
                                  max(n_net$results$Accuracy, na.rm = TRUE),
                                "Prediction Accuracy in Test Set" =  res.n_net$overall[1],
                                "RMSE Test"= rmse_n_net,
                                "Time Elapsed" = et,
                                stringsAsFactors = FALSE))
### 7.6 Comparing Results
rownames(results_all) <- NULL
kable(results_all)
## ############################################
## ############################################ END  ###################################
## ############################################
