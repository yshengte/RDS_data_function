RDSData <- function(
    data,
    unique_id,
    redeemed_coupon,
    issued_coupons,
    degree,
    zero_degree = "hotdeck",
    NA_degree = "hotdeck"
) {
  Warning_Function_ID_missing_data<- function(unique_id,data)if (any(is.na(data[,1]))) {
    stop("Function operation has been interrupted.
         Please make sure there are no missing values in ",
         unique_id," before trying again.")}
  
  Warning_Function_ID_missing_data(unique_id,data)
  
  if(unique_id == redeemed_coupon) {
    unique_id <- "ID_NEW"
    df[[unique_id]] <- df[[redeemed_coupon]]
  }
  
  
  # select relevant columns
  df<-data[,c(unique_id, redeemed_coupon,
              issued_coupons, degree)]
  df <- data.frame(lapply(df, as.character))
  df[df == ''] <- NA
  
  # rename columns
  issued_coupons <- paste0("T_CP", 1:length(issued_coupons))
  names(df) <- c("ID", "R_CP", issued_coupons,"DEGREE")
  
  get_recruiter_ID <- function(df) {
    id <- df$ID
    rc <- replace(df$R_CP, is.na(df$R_CP), '')
    tcs<-subset(df, select = -c(ID, R_CP))
    
    rid <- apply(tcs, 2, function(tc) id[match(rc, tc)])
    rid[is.na(rid)] <- ''
    rid <- apply(rid, 1, paste, collapse = '')
    rid[rid == ''] <- NA
    return(rid)
  }
  
  # construct recruitment chains
  holder <- df
  df_recruiter <- data.frame(R_ID0 = df$ID)
  i <- 0
  
  while(!all(is.na(df_recruiter[[paste0("R_ID", i)]]))) {
    rid <- get_recruiter_ID(holder)
    holder <- data.frame(ID = rid)
    holder$RowOrder <- seq_along(holder$ID)
    holder <- merge(holder, df, by = "ID", all.x = TRUE)
    holder <- holder[order(holder$RowOrder),]
    holder$RowOrder <- NULL
    
    i <- i + 1
    df_recruiter[[paste0("R_ID", i)]] <- rid
  }
  
  # get wave number
  df$WAVE <- apply(df_recruiter, 2, function(c) as.numeric(!is.na(c)))
  df$WAVE <- rowSums(df$WAVE)
  df$WAVE <- df$WAVE - 1
  
  # get seed ID
  df$S_ID <- mapply(function(i, j) df_recruiter[i, j+1],
                    1:nrow(df_recruiter),
                    df$WAVE)
  
  # get recruiter ID
  df$R_ID <- df_recruiter[["R_ID1"]]
  
  # classify seed
  df$SEED <- ifelse(is.na(df$R_ID), 1, 0)
  
  # find the number of coupons issued
  df$CT_T_CP <- rowSums(apply(df[issued_coupons], 2, function(c) as.numeric(!is.na(c))))
  
  # find the number of coupons used
  cu <- aggregate(cbind(CT_T_CP_USED = df$R_ID) ~ R_ID, data = df, FUN = length)
  names(cu)[names(cu) == "R_ID"] <- "ID"
  cu <- na.omit(cu)
  
  df$RowOrder <- 1:nrow(df)
  df <- merge(df, cu, by = "ID", all.x = TRUE)
  df <- df[order(df$RowOrder), ]
  df$RowOrder <- NULL
  df$CT_T_CP_USED <- replace(df$CT_T_CP_USED, is.na(df$CT_T_CP_USED), 0)
  
  #degree imputation
  #For calculation trans to numeric
  df$DEGREE<-as.numeric(df$DEGREE)
  #Select the rows to provide imputation data(non zero$ non NA)
  selected_rows <- df[df$DEGREE != 0 & !is.na(df$DEGREE), ]
  
  #Select the rows that need to be imputation
  zero_row<-df[df$DEGREE == 0&!is.na(df$DEGREE),]
  NA_row<-df[is.na(df$DEGREE),]
  #imputation for NA
  if(nrow(NA_row)!=0){
    if(NA_degree == 'hotdeck'){
      for (i in 1:nrow(NA_row)) {
        # Randomly select a number from the row providing the imputed data
        NA_row$DEGREE[i]<- sample(selected_rows$DEGREE, 1)
      }
    }else if(NA_degree == 'mean'){
      NA_row$DEGREE<-mean(selected_rows$DEGREE)
    }else if(NA_degree == 'median'){
      NA_row$DEGREE<-median(selected_rows$DEGREE)
    }else{
      stop("Invalid NA_degree value.")
    }
  }
  #imputation for 0
  if(nrow(zero_row)!=0){
    if(zero_degree == 'hotdeck'){
      for (i in 1:nrow(zero_row)) {
        # Randomly select a number from the row providing the imputed data
        zero_row$DEGREE[i]<- sample(selected_rows$DEGREE, 1)
      }
    }else if(zero_degree =='mean'){
      zero_row$DEGREE<-mean(selected_rows$DEGREE)
    }else if(zero_degree =='median'){
      zero_row$DEGREE<-median(selected_rows$DEGREE)
    }else{
      stop("Invalid zero_degree_method value.")
    }
  }
  
  #combine modified data
  #combine the imputation result fo 0 and NA
  df_sub<-rbind(zero_row,NA_row)
  
  #use ID to match them
  ids_to_update <- df_sub$ID
  for(ID in ids_to_update) {
    row_to_update <- which(df$ID == ID)
    df$DEGREE[row_to_update] <- df_sub$DEGREE[df_sub$ID == ID]
  }

  #For storage the data trans back to character
  df$DEGREE_IMP <- as.numeric(df$DEGREE)
  df <- cbind(df, data[,-c(match(unique_id,names(data)))])
  df$DEGREE <- NULL
  return(df)
}

RDSToydata<-read.csv("C:\\Users\\sheng\\Desktop\\RA_RDS\\toy_data\\toy_data.csv")
rds_data <- RDSData(data = RDSToydata,
                    unique_id = "ID",
                    redeemed_coupon = "CouponR",
                    issued_coupon = c("Coupon1",
                                      "Coupon2",
                                      "Coupon3"),
                    degree = "Degree",
                    zero_degree = ,
                    NA_degree ='hotdeck' )
