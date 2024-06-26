library(stringr)
library(openxlsx)
library(stringi)

#' @title
#' diff_xlsx
#'
#' @description
#' Allow to see the difference between two datasets and output it into an xlsx file. If the dimensions of the new datasets are bigger than the old one, only the matching cells will be compared, if the dimensions of the new one are lower than the old one, there will be an error.  
#' @param file_ is the file where the data is
#' @param sht is the sheet where the data is
#' @param v_old_begin is a vector containing the coordinates (row, column) where the data to be compared starts
#' @param v_old_end is the same but for its end
#' @param v_new_begin is the coordinates where the comparator data starts
#' @param v_new_end is the same but for its end
#' If the dimensions of the new datasets are bigger than the old one, only the matching cells will be compared, if the dimensions of the new one are lower than the old one, there will be an error.  
#' @param df2 is optional, if the comparator dataset is directly a dataframe
#' @param overwrite allow to overwrite differences is (set to T by default)
#' @param color_ is the color the differences will be outputed
#' @param pattern is the pattern that will be added to the differences if overwritten is set to TRUE 
#' @param output is the name of the outputed xlsx (can be set to NA if no output)
#' @param new_val if overwrite is TRUE, then the differences will be overwritten by the comparator data
#' @param pattern_only will cover differences by pattern if overwritten is set to TRUE 
#' @export

diff_xlsx <- function(file_, sht, v_old_begin, v_old_end, 
                      v_new_begin, v_new_end, df2=NA, overwrite=T, 
                      color_="red", pattern="", output="out.xlsx", new_val=T,
                      pattern_only=T){
  
  rd <- read.xlsx(file_, sheet=sht, col_names=NA)
  
  data_ <- data.frame(rd)
  
  df <- data_[v_old_begin[1]:v_old_end[1], v_old_begin[2]:v_old_end[2]]
  
  if (is.na(df2) == F){
    
    df2 <- df2[v_new_begin[1]:v_new_end[1], v_new_begin[2]:v_new_end[2]]
    
  }else{
    
    df2 <- data_[v_new_begin[1]:v_new_end[1], v_new_begin[2]:v_new_end[2]]
    
  }
  
  nb_diff = 0
  
  c_l <- c()
  
  c_c <- c()
  
  for (I in 1:ncol(df)){
    
    for (i in 1:nrow(df)){
      
      if (df[i, I] != df2[i, I]){
        
        nb_diff = nb_diff + 1
        
        c_l <- append(c_l, i)
        
        c_c <- append(c_c, I)
        
        if (overwrite == T){
          
          if (new_val == T){
            
            data_[i + v_old_begin[1] - 1, I + v_old_begin[2] - 1] <- df2[i, I]
            
          }else{
            
            if (pattern_only == F){
              
              data_[i + v_old_begin[1] - 1, I + v_old_begin[2] - 1] <- pattern
              
            }else{
              
              data_[i + v_old_begin[1] - 1, I + v_old_begin[2] - 1] <- paste0(data_[i + v_old_begin[1] - 1, I + v_old_begin[2] - 1], pattern)
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  
  if (is.na(output) == F){
    
    f <- output
    
  }else{
    
    f <- file_
    
  }
  
  if (overwrite == T){
    
    write.xlsx(data_, output, rowNames=FALSE, colNames=FALSE)
    
  }
  
  wb <- loadWorkbook(f)
  
  diff_style <- createStyle(fontColour = "red",
                            fontSize = 11,
                            fontName="Trebuchet MS",
                            halign = "center",
                            valign = "center",
  )
  
  addStyle(wb,
           "Sheet 1",
           diff_style,
           c_l,
           c_c,
  )
  
  saveWorkbook(wb, f, overwrite=T)
  
  return(nb_diff)
  
}

#' insert_df
#'
#' Allow to insert dataframe into another dataframe according to coordinates (row, column) from the dataframe that will be inserted
#' @param df_in is the dataframe that will be inserted 
#' @param df_ins is the dataset to be inserted
#' @param ins_loc is a vector containg two parameters (row, column) of the begining for the insertion
#' @examples 
#'df1 <- data.frame(c(1, 4), c(5, 3))
#'
#'df2 <- data.frame(c(1, 3, 5, 6), c(1:4), c(5, 4, 5, "ereer"))
#'
#'print(insert_df(df_in=df2, df_ins=df1, ins_loc=c(4, 2)))
#'
#'  c.1..3..5..6. c.1.4. c.5..4..5...ereer..
#'1             1      1                   5
#'2             3      2                   4
#'3             5      3                   5
#'4             6      1                   5
#'
#'print(insert_df(df_in=df2, df_ins=df1, ins_loc=c(3, 2)))
#'
#'  c.1..3..5..6. c.1.4. c.5..4..5...ereer..
#'1             1      1                   5
#'2             3      2                   4
#'3             5      1                   5
#'4             6      4                   3
#'
#'print(insert_df(df_in=df2, df_ins=df1, ins_loc=c(2, 2)))
#'
#'  c.1..3..5..6. c.1.4. c.5..4..5...ereer..
#'1             1      1                   5
#'2             3      1                   5
#'3             5      4                   3
#'4             6      4               ereer
#' @export

insert_df <- function(df_in, df_ins, ins_loc){

  ins_loc <- ins_loc - 1
  
  df_pre1 <- df_in[0:ins_loc[1], 1:ncol(df_in)] 
 
  if ((ins_loc[1] + nrow(df_ins)) > nrow(df_in)){
    
    df_pre2 <- df_in[(ins_loc[1]+1):nrow(df_in), 1:ncol(df_in)]
    
    df_pre3 <- df_in[0:0, 1:ncol(df_in)]
    
    row_end <- nrow(df_pre2)
    
  }else{
   
    df_pre2 <- df_in[(ins_loc[1]+1):(ins_loc[1]+nrow(df_ins)), 1:ncol(df_in)]
   
    if ((ins_loc[1]+nrow(df_ins)) < nrow(df_in)){

        df_pre3 <- df_in[(ins_loc[1] + nrow(df_ins) + 1):nrow(df_in), 1:ncol(df_in)]
    
    }else {

        df_pre3 <- df_in[0:0, 1:ncol(df_in)]

    }

    row_end <- nrow(df_ins)
    
  }
  
  t = 1
  
  for (i in 1:ncol(df_ins)){
    
    df_pre2[, (ins_loc[2]+i)] <- df_ins[1:row_end, t] 
    
    t = t + 1
    
  }
  
  rtnl <- rbind(df_pre1, df_pre2, df_pre3)
  
  return(rtnl)
  
}

#' pattern_generator
#'
#' Allow to create patterns which have a part that is varying randomly each time.
#' @param base_ is the pattern that will be kept
#' @param from_ is the vector from which the elements of the random part will be generated
#' @param hmn is how many of varying pattern from the same base will be created
#' @param after is set to 1 by default, it means that the varying part will be after the fixed part, set to 0 if you want the varying part to be before 
#' @param nb is the number of random pattern chosen for the varying part
#' @param sep is the separator between all patterns in the returned value
#' @examples
#'print(pattern_generator(base_="oui", from_=c("er", "re", "ere"), nb=1, hmn=3))
#'
#' [1] "ouier" "ouire" "ouier"
#'
#'print(pattern_generator(base_="oui", from_=c("er", "re", "ere"), nb=2, hmn=3, after=0, sep="-"))
#'
#' [1] "er-re-o-u-i"  "ere-re-o-u-i" "ere-er-o-u-i"
#'
#' @export

pattern_generator <- function(base_, from_, nb, hmn=1, after=1, sep=""){
  
  rtnl <- c()
  
  base_ <- unlist(str_split(base_, ""))
  
  base2_ <- base_
  
  for (I in 1:hmn){
    
    for (i in 1:nb){
      
      idx <- round(runif(1, 1, length(from_)), 0)
      
      if (after == 1){
        
        base_ <- append(base_, from_[idx])
        
      }else{
        
        base_ <- append(base_, from_[idx], after=0)
        
      }
      
    }
    
    base_ <- stri_paste(base_, collapse=sep)
    
    rtnl <- append(rtnl, base_)
    
    base_ <- base2_
    
  }
  
  return(rtnl)
  
}

#' pattern_tuning
#'
#' Allow to tune a pattern very precisely and output a vector containing its variations n times.
#' @param pattrn is the character that will be tuned 
#' @param spe_nb is the number of new character that will be replaced
#' @param spe_l is the source vector from which the new characters will replace old ones
#' @param exclude_type is character that won't be replaced
#' @param hmn is how many output the function will return
#' @param rg is a vector with two parameters (index of the first letter that will be replaced, index of the last letter that will be replaced) default is set to all the letters from the source pattern
#' @examples
#' print(pattern_tuning(pattrn="oui", spe_nb=2, spe_l=c("e", "r", "T", "O"), exclude_type="o", hmn=3))
#' 
#' [1] "orT" "oTr" "oOi"
#' @export

pattern_tuning <- function(pattrn, spe_nb, spe_l, exclude_type, hmn=1, rg=c(1, nchar(pattrn))){
  
  lngth <- nchar(pattrn)
  
  rtnl <- c()
  
  if (spe_nb <= lngth & rg[1] > -1 & rg[2] < (lngth+1)){
    
    pattrn_l <- unlist(strsplit(pattrn, ""))
    
    pattrn <- pattrn_l
    
    b_l <- c()
    
    for (I in 1:hmn){ 
       
        cnt = 0

        while (cnt <= spe_nb){
          
          if (rg[2] == lngth & rg[1] == 1){
            
            idx <- round(runif(1, 1, lngth), 0)
            
          }else{
            
            idx <- round(runif(1, rg[1], rg[2]), 0)
            
          }
          
          if (sum(grepl(pattrn[idx], exclude_type)) == 0){
            
            pattrn[idx] <- spe_l[round(runif(1, 1, length(spe_l)), 0)]
            
            cnt = cnt + 1

          }
          
        }
        
        pattrn <- paste(pattrn, collapse="")
        
        rtnl <- append(rtnl, pattrn)
        
        pattrn <- pattrn_l 
        
    }
      
    return(rtnl)
    
  }else{
    
    print("word too short for your arguments, see the documentation")
    
  }
  
}

#' can_be_num
#'
#' Return TRUE if a variable can be converted to a number and FALSE if not (supports float)
#' @param x is the input value
#' @examples
#' print(can_be_num("34.677"))
#' [1] TRUE
#' print(can_be_num("34"))
#' [1] TRUE
#' print(can_be_num("3rt4"))
#' [1] FALSE
#' print(can_be_num(34))
#' [1] TRUE
#' @export

can_be_num <- function(x){

    if (typeof(x) == "double"){

            return(T)

    }else{

        vec_bool <- c()

        v_ref <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", ".")    

        v_wrk <- unlist(str_split(x, ""), v_ref)

        for (i in 1:length(v_wrk)){ 

                if (v_wrk[i] == "."){ 

                        vec_bool <- append(vec_bool, sum(grepl("\\.", v_ref))) 

                }else{

                        vec_bool <- append(vec_bool, sum(grepl(v_wrk[i], v_ref))) 

                }

        }

        if (sum(vec_bool) == length(vec_bool)){

                return(T)

        }else{

                return(F)

        }

    }

}

#' unique_pos
#'
#' Allow to find indexes of the unique values from a vector. 
#' @param vec is the input vector
#' @examples
#' print(unique_pos(c(3, 4, 3, 5, 6)))
#' [1] 1 2 4 5
#' @export

unique_pos <- function(vec){

        u_vec <- unique(vec)

        return(match(u_vec, vec))

}

#' data_gen
#'
#' Allo to generate in a csv all kind of data you can imagine according to what you provide
#' @param type_ is a vector. Its arguments designates a column, a column can be made of numbers ("number"), string ("string") or both ("mixed")
#' @param strt_l is a vector containing for each column the row from which the data will begin to be generated
#' @param nb_r is a vector containing for each column, the number of row full from generated data  
#' @param output is the name of the output csv file, defaults to NA so no csv will be outputed by default
#' @param type_distri is a vector which, for each column, associate a type of distribution ("random", "poisson", "gaussian"), it meas that non only the number but also the length of the string will be randomly generated according to these distribution laws
#' @param properties is linked to type_distri because it is the parameters ("min_val-max_val") for "random type", ("u-x") for the poisson distribution, ("u-d") for gaussian distribution
#' @param str_source is the source (vector) from which the character creating random string are (default set to the occidental alphabet)
#' @param round_l is a vector which, for each column containing number, associate a round value, if the type of the value is numeric
#' @param sep_ is the separator used to write data in the csv
#' @return new generated data in addition to saving it in the output
#' @examples
#' 
#' print(data_gen())
#' 
#'   X1   X2    X3
#' 1   4    2  <NA>
#' 2   2    4  <NA>
#' 3   5    2  <NA>
#' 4   2 abcd  <NA>
#' 5   4 abcd  <NA>
#' 6   2    4  <NA>
#' 7   2  abc  <NA>
#' 8   4  abc  <NA>
#' 9   4    3  <NA>
#' 10  4  abc  abcd
#' 11  5 <NA>   abc
#' 12  4 <NA>   abc
#' 13  1 <NA>    ab
#' 14  1 <NA> abcde
#' 15  2 <NA>   abc
#' 16  4 <NA>     a
#' 17  1 <NA>  abcd
#' 18  4 <NA>    ab
#' 19  2 <NA>  abcd
#' 20  3 <NA>    ab
#' 21  3 <NA>  abcd
#' 22  2 <NA>     a
#' 23  4 <NA>   abc
#' 24  1 <NA>  abcd
#' 25  4 <NA>   abc
#' 26  4 <NA>    ab
#' 27  2 <NA>   abc
#' 28  5 <NA>    ab
#' 29  3 <NA>   abc
#' 30  5 <NA>  abcd
#' 31  2 <NA>   abc
#' 32  2 <NA>   abc
#' 33  1 <NA>    ab
#' 34  5 <NA>     a
#' 35  4 <NA>    ab
#' 36  1 <NA>    ab
#' 37  1 <NA> abcde
#' 38  5 <NA>   abc
#' 39  4 <NA>    ab
#' 40  5 <NA> abcde
#' 41  2 <NA>    ab
#' 42  3 <NA>    ab
#' 43  2 <NA>    ab
#' 44  4 <NA>  abcd
#' 45  5 <NA>  abcd
#' 46  3 <NA>  abcd
#' 47  2 <NA>  abcd
#' 48  3 <NA>  abcd
#' 49  3 <NA>  abcd
#' 50  4 <NA>     a
#'
#' print(data_gen(strt_l=c(0, 0, 0), nb_r=c(5, 5, 5)))
#' 
#'   X1    X2   X3
#' 1  2     a  abc
#' 2  3 abcde   ab
#' 3  4 abcde    a
#' 4  1     3  abc
#' 5  3     a abcd
#' @export

data_gen <- function(type_=c("number", "mixed", "string"), strt_l=c(0, 0, 10), nb_r=c(50, 10, 40), output=NA, 
                     properties=c("1-5", "1-5", "1-5"), type_distri=c("random", "random", "random"), 
                     str_source=c("a", "b", "c", "d", "e", "f", "g", 
                                  "h", "i", "j", "k", "l", "m", 
                                  "n", "o", "p", "q", "r", "s", "t", "u", "w", "x", "y", "z"), 
                     round_l=c(0, 0, 0), sep_=","){
  
  v_get1 <- c()
  
  v_get2 <- c()
  
  delta_ <- c()
  
  for (i in 1:length(properties)){
    
    v_get1 <- append(v_get1, unlist(str_split(properties[i], "-"))[1])
    
    v_get2 <- append(v_get2, unlist(str_split(properties[i], "-"))[2])
    
    delta_ <- append(delta_, (strt_l[i] + nb_r[i]))
    
  }
  
  v_get1 <- as.numeric(v_get1)
  
  v_get2 <- as.numeric(v_get2)
  
  rtnl <- data.frame(matrix(NA, nrow=max(delta_), ncol=length(type_)))
  
  for (I in 1:length(type_)){
    
    if (type_[I] == "mixed"){
      
      for (i in strt_l[I]:(nb_r[I]+strt_l[I])){
        
        str_ <- round(runif(1, 0, 1), 0)
        
        if (str_ == 1){
          
          if (type_distri[I] == "random"){
            
            add_ <- round(runif(1, v_get1[I], v_get2[I]), 0)
            
            if (add_ > length(str_source)){
              
              add_ <- length(str_source)
              
            }
            
            rtnl[i, I] <- stri_paste(str_source[1:add_], collapse="")
            
          }
          
          if (type_distri[I] == "poisson"){
            
            add_ <- round(dpois(v_get1[I], v_get2[I]), 0)
            
            if (add_ > length(str_source)){
              
              add_ <- length(str_source)
              
            }
            
            rtnl[i, I] <- stri_paste(str_source[1:add_], collapse="")
            
          }
          
          if (type_distri[I] == "gaussian"){
            
            add_ <- round(runif(1, v_get1[I], v_get2[I]), 0)
            
            if (add_ > length(str_source)){
              
              add_ <- length(str_source)
              
            }
            
            rtnl[i, I] <- stri_paste(str_source[1:add_], collapse="")
            
          }
          
        }else{
          
          
          if (type_distri[I] == "random"){
            
            add_ <- round(runif(1, v_get1[I], v_get2[I]), round_l[I])
            
            rtnl[i, I] <- add_
            
          }
          
          if (type_distri[I] == "poisson"){
            
            add_ <- round(dpois(v_get1[I], v_get2[I]), round_l[I]) 
            
            rtnl[i, I] <- add_
            
          }
          
          if (type_distri[I] == "gaussian"){
            
            add_ <- round(dnorm(v_get1[I], v_get2[I]), round_l[I])
            
            rtnl[i, I] <- add_
            
          }
          
          
        }
        
      }
      
    }
    
    if (type_[I] == "string"){
      
      for (i in strt_l[I]:(nb_r[I]+strt_l[I])){
        
        if (type_distri[I] == "random"){
          
          add_ <- round(runif(1, v_get1[I], v_get2[I]), 0)
          
          if (add_ > length(str_source)){
            
            add_ <- length(str_source)
            
          }
          
          rtnl[i, I] <- stri_paste(str_source[1:add_], collapse="")
          
        }
        
        if (type_distri[I] == "poisson"){
          
          add_ <- round(dpois(v_get1[I], v_get2[I]), 0)
          
          if (add_ > length(str_source)){
            
            add_ <- length(str_source)
            
          }
          
          rtnl[i, I] <- stri_paste(str_source[1:add_], collapse="")
          
        }
        
        if (type_distri[I] == "gaussian"){
          
          add_ <- round(runif(1, v_get1[I], v_get2[I]), 0)
          
          if (add_ > length(str_source)){
            
            add_ <- length(str_source)
            
          }
          
          rtnl[i, I] <- stri_paste(str_source[1:add_], collapse="")
          
        }
        
      }
      
    }
    
    if (type_[I] == "number"){
      
      for (i in strt_l[I]:(nb_r[I]+strt_l[I])){
        
        if (type_distri[I] == "random"){
          
          add_ <- round(runif(1, v_get1[I], v_get2[I]), round_l[I])
          
          rtnl[i, I] <- add_
          
        }
        
        if (type_distri[I] == "poisson"){
          
          add_ <- round(dpois(v_get1[I], v_get2[I]), round_l[I]) 
          
          rtnl[i, I] <- add_
          
        }
        
        if (type_distri[I] == "gaussian"){
          
          add_ <- round(dnorm(v_get1[I], v_get2[I]), round_l[I])
          
          rtnl[i, I] <- add_
          
        }
        
      }
      
    }
    
  }
  
  if (is.na(output) == F){
    
    write.table(rtnl, output, sep=sep_, row.names=F, col.names=F)
    
  }
  
  return(rtnl)
  
}

#' data_meshup
#'
#' Allow to automatically arrange 1 dimensional data according to vector and parameters
#' @param data is the data provided (vector) each column is separated by a unic separator and each dataset from the same column is separated by another unic separator (ex: c("_", c("d", "-", "e", "-", "f"), "_", c("a", "a1", "-", "b", "-", "c", "c1"), "_")
#' @param cols are the colnames of the data generated in a csv
#' @param file_ is the file to which the data will be outputed, defaults to NA which means that the functio will return the dataframe generated and won't write it to a csv file
#' @param sep_ is the separator of the csv outputed
#' @param organisation is the way variables include themselves, for instance ,resuming precedent example, if organisation=c(1, 0) so the data output will be:
#' d, a
#' d, a1
#' e, c
#' f, c
#' f, c1
#' @param unic_sep1 is the unic separator between variables (default is "_")
#' @param unic_sep2 is the unic separator between datasets (default is "-")
#' @examples
#' print(data_meshup(data=c("_", c("-", "d", "-", "e", "-", "f"), "_", c("-", "a", "a1", "-", "B", "r", "uy", "-", "c", "c1"), "_"), organisation=c(1, 0)))
#'   X1 X2
#' 1  d  a
#' 2  d a1
#' 3  e  B
#' 4  e  r
#' 5  e uy
#' 6  f  c
#' 7  f c1
#' @export

data_meshup <- function(data, cols=NA, file_=NA, sep_=";", 
                        organisation=c(2, 1, 0), unic_sep1="_", 
                        unic_sep2="-"){
 
  l_l <- c()
  
  l_lngth <- c()
  
  old_max_row <- -1
  
  sep_dd <- str_detect(data, unic_sep1)
  
  jsq <- sum(sep_dd[!is.na(sep_dd)]) - 1 #numb of var
  
  sep_dd <- str_detect(data, unic_sep2)
  
  hmn <- (sum(sep_dd[!is.na(sep_dd)]) / jsq) #numb of datasets
 
  val_nb <- length(data) - (hmn * jsq + (jsq + 1))  
  
  dataset_l <- which(str_detect(unic_sep2, data))
  
  df <- data.frame(matrix(nrow = val_nb, ncol = jsq))
  
  for (I in 1:hmn){
    
    idx_s = 0
    
    seq_l <- seq(I, length(dataset_l), hmn)
   
    for (i in 1:jsq){
      
      idx <- dataset_l[seq_l[i]]
      
      t = 1
      
      sep_dd <- grepl(data[idx + 1], c(unic_sep2, unic_sep1))
     
      while(sum(sep_dd[!is.na(sep_dd)]) == 0 & (idx + t <= length(data))){
       
        l_l <- append(l_l, data[idx + t])
        
        t = t + 1
        
        sep_dd <- grepl(data[idx + t], c(unic_sep2, unic_sep1))
        
      }
      
      l_lngth <- append(l_lngth, (t - 1))
      
      df[1:(t-1), i] <- l_l
      
      l_l <- c()
      
    }
    
    if (old_max_row == -1){
      
      df2 <- data.frame(matrix(nrow=0, ncol=jsq))
      
    }
   
    old_max_row <- max(l_lngth, na.rm=T)
    
    l_lngth <- c()
  
    for (i in 1:jsq){
      
      v_rel <- df[, i]
      
      var_ = 1
      
      x = 1
     
      while (x <= organisation[i]){
       
        v_relb <- df[, i + x]
        
        val_ <- v_rel[var_] # val to be added
        
        for (t in 1:length(v_relb[!is.na(v_relb)])){
          
          df[t, i] <- val_ 
          
          if (t + 1 <= length(v_rel)){
            
            if (is.na(v_rel[t + 1]) == F){
              
              var_ = var_ + 1 
              
              while (is.na(v_rel[var_]) == T){
                
                var_ = var_ + 1 
                
              }
              
              val_ <- v_rel[var_]  
              
            }
            
          }
          
        }
        
        x = x + 1
        
      }
      
    }
   
    df2 <- rbind(df2, df[1:old_max_row, 1:jsq])
    
    df[1:nrow(df), 1:ncol(df)] <- NA
    
  }
  
  if (all(is.na(cols)) == F){
    
    colnames(df2) <- cols
    
  }
  
  if (is.na(file_)){
    
    return(df2)
    
  }else{
    
    write.table(df2, file_, sep=sep_, row.names=F)
    
  }
  
}

#' letter_to_nb
#'
#' Allow to get the number of a spreadsheet based column by the letter ex: AAA = 703
#' @param letter is the letter (name of the column)
#' @examples
#' print(letter_to_nb("rty"))
#'
#' [1] 12713
#' @export

letter_to_nb <- function(letter){
  
  l <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", 
         "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
  
  nb = 0
  
  nch <- nchar(letter) - 1
  
  for (i in 0:nch){
    
    x <- str_sub(letter, nchar(letter) - i, nchar(letter) - i)
    
    x <- tolower(x)
    
    nb <- nb + match(x, l) * 26 ** i
    
  }
  
  return(nb)
  
}

#' nb_to_letter
#'
#' Allow to get the letter of a spreadsheet based column by the number ex: 703 = AAA
#' @param x is the number of the column 
#' @examples
#' print(nb_to_letter(12713))
#'
#' [1] "rty"
#' @export

nb_to_letter <- function(x){
  
  l <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", 
         "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
  
  rtnl  <- c()
  
  x_l <- c()
  
  r_l <- c()
  
  abc_l <- c()
  
  t = 1
  
  add_ <- 1
  
  reste <- 0
  
  while (x %/% add_ > 0){
    
    r_l <- append(r_l, reste)
    
    x_l <- append(x_l, add_)
    
    add_ <- 26 ** t
    
    reste <- x %% add_
    
    t = t + 1
    
  }
  
  for (i in 1:length(x_l)){
    
    idx <- length(x_l) - (i - 1)
    
    add_ <- x %/% x_l[idx]
    
    rtnl <- append(rtnl, l[add_])
    
    x <- r_l[idx]
    
  }
  
  rtnl <- paste(rtnl, collapse="")
  
  return(rtnl)
  
}

#' cost_and_taxes
#'
#' Allow to calculate basic variables related to cost and taxes from a bunch of products (elements)
#' So put every variable you know in the following order:
#' @param qte is the quantity of elements
#' @param pu is the price of a single elements without taxes
#' @param prix_ht is the duty-free price of the whole set of elements
#' @param tva is the percentage of all taxes
#' @param prix_ttc is the price of all the elements with taxes
#' @param prix_tva is the cost of all the taxes
#' @param pu_ttc is the price of a single element taxes included
#' @param adjust is the discount percentage
#' @param prix_d_ht is the free-duty price of an element after discount
#' @param prix_d_ttc is the price with taxes of an element after discount
#' @param pu_d is the price of a single element after discount and without taxes
#' @param pu_d_ttc is the free-duty price of a single element after discount
#' the function return a vector with the previous variables in the same order
#' those that could not be calculated will be represented with NA value
#' @examples
#' print(cost_and_taxes(pu=45, prix_ttc=21, qte=3423))
#' 
#' [1]  3.423000e+03  4.500000e+01  4.500000e+01 -9.998637e-01  2.100000e+01
#' [6] -1.540140e+05  4.500000e+01            NA            NA            NA
#' [11]            NA            NA
#' @export

cost_and_taxes <- function(qte=NA, pu=NA, prix_ht=NA, tva=NA, prix_ttc=NA,
                           prix_tva=NA, pu_ttc=NA, adjust=NA, prix_d_ht=NA,
                           prix_d_ttc=NA, pu_d=NA, pu_d_ttc=NA){
  
  val_l <- c(qte, pu, prix_ht, tva, prix_ttc, prix_tva, pu_ttc, adjust,
             prix_d_ht, prix_d_ttc, pu_d, pu_d_ttc)
  
  already <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  
  rtnl <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  
  for (i in 1:length(already)){
    
    if (is.na(val_l[i]) == F){
      
      already[i] <- 1
      
      rtnl[i] <- val_l[i]
      
    }
    
  }
  
  for (i in 1:16){
    
    if (is.na(prix_ttc) == F & is.na(prix_d_ttc) == F & already[8] == 0){
      
      adjust <- prix_ttc / prix_d_ttc - 1
      
      already[8] <- 1
      
      rtnl[8] <- adjust
      
    }

    if (is.na(prix_ht) == F & is.na(prix_d_ht) == F & already[8] == 0){
      
      adjust <- prix_ht / prix_d_ht - 1
      
      already[8] <- 1
      
      rtnl[8] <- adjust
      
    }
    
    if (is.na(pu_ttc) == F & is.na(pu_d_ttc) == F & already[8] == 0){
      
      adjust <- pu_ttc / pu_d_tcc - 1
      
      already[8] <- 1
      
      rtnl[8] <- adjust
      
    }

    if (is.na(pu) == F & is.na(pu_d) == F & already[8] == 0){
      
      adjust <- pu / pu_d - 1
      
      already[8] <- 1
      
      rtnl[8] <- adjust
      
    }

    if (is.na(qte) == F & is.na(pu_d) == 0 & already[9] == 0){
      
      prix_d_ht <- qte * pu_d
      
      already[9] <- 0
      
      rtnl[9] <- prix_d_ht
      
    }
    
    if (is.na(qte) == F & is.na(pu_d_ttc) == 0 & already[10] == 0){
      
      prix_d_ttc <- qte * pu_d_ttc
      
      already[10] <- 0
      
      rtnl[10] <- prix_d_ht
      
    }
    
    if (is.na(prix_d_ttc) == F & is.na(qte) == F & already[12] == 0){
      
      pu_d_ttc <- prix_d_ttc / qte
      
      already[12] <- 1
      
      rtnl[12] <- pu_d_ttc
      
    }
    
    if (is.na(prix_d_ht) == F & is.na(qte) == F & already[11] == 0){
      
      pu_d <- prix_d_ht / qte
      
      already[11] <- 1
      
      rtnl[11] <- pu_d
      
    }
    
    if (is.na(adjust) == F & is.na(prix_ttc) == F & already[10] == 0){
      
      prix_d_ttc <- prix_ttc * (1 - adjust)
      
      already[10] <- 1
      
      rtnl[10] <- prix_d_ttc
      
    }
    
    if (is.na(adjust) == F & is.na(prix_ht) == F & already[9] == 0){
      
      prix_d_ht <- prix_ht * (1 - adjust)
      
      already[9] <- 1
      
      rtnl[9] <- prix_d_ht
      
    }
    
    if (is.na(adjust) == F & is.na(prix_d_ht) == F & already[3] == 0){
      
      prix_ht <- prix_d_ht * (1 / (1 - adjust))
      
      already[3] <- 1
      
      rtnl[3] <- prix_ht
      
    }
    
    if (is.na(adjust) == F & is.na(prix_d_ttc) == F & already[5] == 0){
      
      prix_ttc <- prix_d_ttc * (1 / (1 - adjust))
      
      already[5] <- 1
      
      rtnl[5] <- prix_ttc
      
    }
    
    if (is.na(pu) == F & is.na(pu_ttc) == F & already[4] == 0){
      
      tva <- pu_ttc / pu - 1
      
      already[4] <- 1
      
      rtnl[4] <- tva
      
    }

    if (is.na(pu_d_ttc) == F & is.na(pu_d) == F & already[4] == 0){
      
      tva <- pu_d_ttc / pu_d - 1
      
      already[4] <- 1
      
      rtnl[4] <- tva
      
    }

    if (is.na(prix_d_ttc) == F & is.na(prix_d_ht) == F & already[4] == 0){
      
      tva <- prix_d_ttc / prix_d_ht - 1
      
      already[4] <- 1
      
      rtnl[4] <- tva
      
    }
    
    if (is.na(prix_ht) == F & is.na(pu) == F & already[1] == 0){
      
      qte <- prix_ht / pu
      
      rtnl[1] <- as.integer(qte)
      
      already[1] <- 1
      
    }
    
    if (is.na(prix_ttc) == F & is.na(pu_ttc) == F & already[1] == 0){
      
      qte <- prix_ttc / pu_ttc
      
      rtnl[1] <- as.integer(qte)
      
      already[1] <- 1
      
    }
    
    if (is.na(prix_d_ht) == F & is.na(pu_d) == F & already[1] == 0){
      
      qte <- prix_d_ht / pu_d
      
      rtnl[1] <- as.integer(qte)
      
      already[1] <- 1
      
    }
    
    if (is.na(prix_d_ttc) == F & is.na(pu_d_ttc) == F & already[1] == 0){
      
      qte <- prix_d_ttc / pu_d_ttc
      
      rtnl[1] <- as.integer(qte)
      
      already[1] <- 1
      
    }
    
    if (is.na(prix_ht) == F & is.na(qte) == F & already[2] == 0){
      
      pu <- prix_ht / qte
      
      rtnl[2] <- pu
      
      already[2] <- 1
      
    }
    
    if (is.na(prix_ttc) == F & is.na(qte) == F & already[7] == 0){
      
      pu_ttc <- prix_ttc / qte
      
      rtnl[7] <- pu
      
      already[7] <- 1
      
    }
    
    if (is.na(pu) == F & is.na(qte) == F & already[3] == 0){
      
      prix_ht <- pu * qte
      
      rtnl[3] <- pu
      
      already[3] <- 1
      
    }
    
    if (is.na(pu_ttc) == F & is.na(qte) == F & already[5] == 0){
      
      prix_ttc <- pu_ttc * qte
      
      rtnl[5] <- pu
      
      already[5] <- 1
      
    }
    
    if (is.na(pu) == F & is.na(qte) == F & already[3] == 0){
      
      prix_ht <- pu * qte
      
      rtnl[3] <- prix_ht
      
      already[3] <- 1
      
    }
    
    if (is.na(prix_ht) == F & is.na(prix_ttc) == F & already[4] == 0){
      
      tva <- prix_ttc / prix_ht - 1
      
      rtnl[4] <- tva
      
      already[4] <- 1
      
    }
    
    if (is.na(tva) == F & is.na(prix_ttc) == F & already[3] == 0){
      
      prix_ht <- prix_ttc / (1 + tva)
      
      rtnl[3] <- prix_ht
      
      already[3] <- 1
      
    }
    
    if (is.na(tva) == F & is.na(prix_ht) == F & already[5] == 0){
      
      prix_ttc <- prix_ht * (1 + tva) 
      
      rtnl[5] <- prix_ttc
      
      already[5] <- 1
      
    }  
    
    if (is.na(prix_ht) == F & is.na(prix_ttc) == F & already[6] == 0){
      
      prix_tva <- prix_ttc - prix_ht
      
      rtnl[6] <- prix_tva
      
      already[6] <- 1
      
    }
    
    if (is.na(tva) == F & is.na(prix_ttc) == F & already[6] == 0){
      
      prix_tva <- tva * prix_ht
      
      rtnl[6] <- prix_tva
      
      already[6] <- 1
      
    }
    
  }
  
  return(rtnl)
  
}

#' format_date
#'
#' Allow to convert xx-month-xxxx date type to xx-xx-xxxx
#' @param f_dialect are the months from the language of which the month come
#' @param sentc is the date to convert
#' @param sep_in is the separator of the dat input (default is "-")
#' @param sep_out is the separator of the converted date (default is "-")
#' @examples
#' print(format_date(f_dialect=c("janvier", "février", "mars", "avril", "mai", "juin",
#'                           "juillet", "aout", "septembre", "octobre", "novembre", "décembre"), sentc="11-septembre-2023"))
#'
#' [1] "11-09-2023"
#' @export

format_date <- function(f_dialect, sentc, sep_in="-", sep_out="-"){
  
  traduct <- f_dialect
  
  x <- unlist(str_split(sentc, sep_in))
  
  x2 <- match(x[2], f_dialect)
  
  x2 <- as.character(x2)

  if (nchar(x2) == 1){

    x2 <- paste0("0", x2)
  
  }

  x <- paste0(x[1], sep_out, x2, sep_out, x[3]) 
  
  return(x)
  
}

#' until_stnl
#'
#' Maxes a vector to a chosen length 
#' ex: if i want my vector c(1, 2) to be 5 of length this function will return me: c(1, 2, 1, 2, 1) 
#' @param vec1 is the input vector
#' @param goal is the length to reach
#' @examples
#' print(until_stnl(vec1=c(1, 3, 2), goal=56))
#'
#'  [1] 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3
#' [39] 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3
#' @export

until_stnl <- function(vec1, goal){

  max_ = 0

  ld <- length(vec1)

  for (i in (length(vec1)+1):goal){

        if (max_ < ld){

                max_ = max_ + 1 

        }else{

                max_ = 1

        }

        vec1 <- append(vec1, vec1[max_])

  }

  return(vec1)

}

#' vlookup_df
#'
#' Alow to perform a vlookup on a dataframe
#' @param df is the input dataframe
#' @param v_id is a vector containing the ids
#' @param col_id is the column that contains the ids (default is equal to 1)
#' @param included_col_id is if the result should return the col_id (default set to yes)
#' @examples
#' df1 <- data.frame(c("az1", "az3", "az4", "az2"), c(1:4), c(4:1))
#' 
#' print(vlookup_df(df=df1, v_id=c("az1", "az2", "az3", "az4")))
#'
#'    c..az1....az3....az4....az2.. c.1.4. c.4.1.
#' 2                            az1      1      4
#' 4                            az2      4      1
#' 21                           az3      2      3
#' 3                            az4      3      2
#' @export

vlookup_df <- function(df, v_id, col_id=1, included_col_id="yes"){
  
  rtnl <- df[1, ]
  
  for (i in 1:length(v_id)){

    idx = match(v_id[i], df[, col_id])
    
    rtnl <- rbind(rtnl, df[idx,])
    
    df <- df[-idx, ]
    
  }
  
  if (included_col_id == "yes"){
  
    return(rtnl[-1, ])
  
  }else{
    
    return(rtnl[-1, -col_id])
    
  }
    
}

#' multitud
#'
#' From a list containing vectors allow to generate a vector following this rule:
#' list(c("a", "b"), c("1", "2"), c("A", "Z", "E")) --> c("a1A", "b1A", "a2A", "b2A", a1Z, ...)
#' @param l is the list
#' @param sep_ is the separator between elements (default is set to "" as you see in the example)
#' @examples
#' print(multitud(l=list(c("a", "b"), c("1", "2"), c("A", "Z", "E"), c("Q", "F")), sep_="/"))
#' 
#' [1] "a/1/A/Q" "b/1/A/Q" "a/2/A/Q" "b/2/A/Q" "a/1/Z/Q" "b/1/Z/Q" "a/2/Z/Q"
#' [8] "b/2/Z/Q" "a/1/E/Q" "b/1/E/Q" "a/2/E/Q" "b/2/E/Q" "a/1/A/F" "b/1/A/F"
#' [15] "a/2/A/F" "b/2/A/F" "a/1/Z/F" "b/1/Z/F" "a/2/Z/F" "b/2/Z/F" "a/1/E/F"
#' [22] "b/1/E/F" "a/2/E/F" "b/2/E/F"
#' @export

multitud <- function(l, sep_=""){
  
  rtnl <- unlist(l[1])

  for (I in 2:length(l)){
    
    rtnl2 <- c()
  
    cur_ <- unlist(l[I])
    
    for (i in 1:length(cur_)){
      
      for (t in 1:length(rtnl)){
        
        rtnl2 <- append(rtnl2, paste(rtnl[t], cur_[i], sep=sep_))

      }

    }
    
    rtnl <- rtnl2
    
  }

  return(rtnl)
  
}

#' save_untl
#'
#' Get the elements in each vector from a list that are located before certain values
#'
#' @param inpt_l is the input list containing all the vectors
#' @param val_to_stop is a vector containing the values that marks the end of the vectors returned in the returned list, see the examples
#' 
#' @examples
#' print(save_untl(inpt_l=list(c(1:4), c(1, 1, 3, 4), c(1, 2, 4, 3)), val_to_stop_v=c(3, 4)))
#'
#' [[1]]
#' [1] 1 2
#' 
#' [[2]]
#' [1] 1 1
#' 
#' [[3]]
#' [1] 1 2
#'
#' print(save_untl(inpt_l=list(c(1:4), c(1, 1, 3, 4), c(1, 2, 4, 3)), val_to_stop_v=c(3)))
#' 
#' [[1]]
#' [1] 1 2
#' 
#' [[2]]
#' [1] 1 1
#' 
#' [[3]]
#' [1] 1 2 4
#' @export

save_untl <- function(inpt_l=list(), val_to_stop_v=c()){

        rtn_l <- list()

        for (vec in inpt_l){

                t = 1

                cur_v <- c()

                while (!(vec[t] %in% val_to_stop_v) & t <= length(vec)){

                        cur_v <- c(cur_v, vec[t])

                        t = t + 1

                }

                rtn_l <- append(x=rtn_l, values=list(cur_v))

        }

        return(rtn_l)

}

#' see_df
#' 
#' Allow to return a dataframe with special value cells (ex: TRUE) where the condition entered are respected and another special value cell (ex: FALSE) where these are not
#' @param df is the input dataframe
#' @param condition_l is the vector of the possible conditions ("==", ">", "<", "!=", "%%", "reg", "not_reg", "sup_nchar", "inf_nchar", "nchar") (equal to some elements in a vector, greater than, lower than, not equal to, is divisible by, the regex condition returns TRUE, the regex condition returns FALSE, the length of the elements is strictly superior to X, the length of the element is strictly inferior to X, the length of the element is equal to one element in a vector), you can put the same condition n times. 
#' @param val_l is the list of vectors containing the values or vector of values related to condition_l (so the vector of values has to be placed in the same order)
#' @param conjunction_l contains the and or conjunctions, so if the length of condition_l is equal to 3, there will be 2 conjunctions. If the length of conjunction_l is inferior to the length of condition_l minus 1, conjunction_l will match its goal length value with its last argument as the last arguments. For example, c("&", "|", "&") with a goal length value of 5 --> c("&", "|", "&", "&", "&")
#' @param rt_val is a special value cell returned when the conditions are respected
#' @param f_val is a special value cell returned when the conditions are not respected
#' @details This function will return an error if number only comparative conditions are given in addition to having character values in the input dataframe.
#' @examples
#' 
#' df1 <- data.frame(c(1, 2, 4), c("a", "a", "zu"))
#' 
#' print(see_df(df=df1, condition_l=c("nchar"), val_l=list(c(1))))
#' 
#'     X1    X2
#' 1 TRUE  TRUE
#' 2 TRUE  TRUE
#' 3 TRUE FALSE
#' 
#' print(see_df(df=df1, condition_l=c("=="), val_l=list(c("a", 1))))
#' 
#'     X1    X2
#' 1  TRUE  TRUE
#' 2 FALSE  TRUE
#' 3 FALSE FALSE
#'
#' 
#' print(see_df(df=df1, condition_l=c("nchar"), val_l=list(c(1, 2))))
#' 
#'     X1   X2
#' 1 TRUE TRUE
#' 2 TRUE TRUE
#' 3 TRUE TRUE
#'
#' print(see_df(df=df1, condition_l=c("not_reg"), val_l=list("[a-z]")))
#' 
#'     X1    X2
#' 1 TRUE FALSE
#' 2 TRUE FALSE
#' 3 TRUE FALSE
#' @export

see_df <- function(df, condition_l, val_l, conjunction_l=c(), rt_val=T, f_val=F){

        if (length(condition_l) > 1 & length(conjunction_l) < (length(condition_l) - 1)){

                for (i in (length(conjunction_l)+1):length(condiction_l)){

                        conjunction_l <- append(conjunction_l, conjunction_l[length(conjunction_l)])

                }

        }

        df_rtnl <- data.frame(matrix(f_val, ncol=ncol(df), nrow=nrow(df)))

        all_op <- c("==", ">", "<", "!=", "%%", "reg", "not_reg", "sup_nchar", "inf_nchar", "nchar")

        for (I in 1:ncol(df)){

                for (i in 1:nrow(df)){

                        checked_l <- c()

                        previous = 1

                        for (t in 1:length(condition_l)){

                                already <- 0

                                if (condition_l[t] == "==" & already == 0){

                                        if (df[i, I] %in% unlist(val_l[t])){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == ">" & already == 0){

                                        if (all(df[i, I] > unlist(val_l[t])) == T){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == "<" & already == 0){

                                        if (all(df[i, I] < unlist(val_l[t]))){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == "!=" & already == 0){

                                        if (!(df[i, I] %in% unlist(val_l[t])) == T){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == "%%" & already == 0){

                                        if (sum(df[i, I] %% unlist(val_l[t])) == 0){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == "reg" & already == 0){

                                        if (str_detect(df[i, I], unlist(val_l[t]))){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }  else if (condition_l[t] == "not_reg" & already == 0){

                                        if ((str_detect(df[i, I], unlist(val_l[t]))) == F ){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }  else if (condition_l[t] == "sup_nchar" & already == 0){

                                        if (nchar(as.character(df[i, I])) > unlist(val_l[t])){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }  else if (condition_l[t] == "inf_nchar" & already == 0){

                                        if (nchar(as.character(df[i, I])) < unlist(val_l[t])){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }

                                if (condition_l[t] == "nchar" & already == 0){

                                        if (nchar(as.character(df[i, I])) %in% unlist(val_l[t])){

                                                checked_l <- append(checked_l, T)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        df_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        df_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                df_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }

                        }
                        
                }

        }

  return(df_rtnl)

}

#' bsx_year 
#'
#' Get if the year is leap
#'
#' @param year is the input year
#' 
#' @examples
#'
#' print(leap_yr(year=2024))
#' 
#' [1] TRUE
#' @export

leap_yr <- function(year){

  if (year == 0){ return(F) }

  if (year %% 4 == 0){
    
    if (year %% 100 == 0){
      
      if (year %% 400 == 0){
        
        bsx <- T
        
      }else{
        
        bsx <- F
        
      }
      
    }else{
      
      bsx <- T
      
    }
    
  }else{
    
    bsx <- F
    
  }

  return(bsx)

}

#' is_divisible
#'
#' Takes a vector as an input and returns all the elements that are divisible by all choosen numbers from another vector.
#'
#' @param inpt_v is the input vector
#' @param divisible_v is the vector containing all the numbers that will try to divide those contained in inpt_v
#' @examples
#'
#'  print(is_divisible(inpt_v=c(1:111), divisible_v=c(2, 4, 5)))
#'
#'  [1]  20  40  60  80 100
#'
#' @export

is_divisible <- function(inpt_v=c(), divisible_v=c()){

        cnt = 1

        while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) == 0]

                cnt = cnt + 1

        }

        return(inpt_v)

}

#' isnt_divisible
#'
#' Takes a vector as an input and returns all the elements that are not divisible by all choosen numbers from another vector.
#'
#' @param inpt_v is the input vector
#' @param divisible_v is the vector containing all the numbers that will try to divide those contained in inpt_v
#' @examples
#'
#'  print(isnt_divisible(inpt_v=c(1:111), divisible_v=c(2, 4, 5)))
#'
#'  [1]   1   3   7   9  11  13  17  19  21  23  27  29  31  33  37  39  41  43  47
#' [20]  49  51  53  57  59  61  63  67  69  71  73  77  79  81  83  87  89  91  93
#' [39]  97  99 101 103 107 109 111
#'
#' @export

isnt_divisible <- function(inpt_v=c(), divisible_v=c()){

        cnt = 1

        while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) != 0]

                cnt = cnt + 1

        }

        return(inpt_v)

}

#' dcr_untl 
#' 
#' Allow to get the final value of a incremental or decremental loop. 
#'
#' @param strt_val is the start value
#' @param cr_val is the incremental (or decremental value)
#' @param stop_val is the value where the loop has to stop
#' @examples
#'
#' print(dcr_untl(strt_val=50, cr_val=-5, stop_val=5))
#'
#' [1] 9
#'
#' print(dcr_untl(strt_val=50, cr_val=5, stop_val=450))
#'
#' [1] 80
#' 
#' @export

dcr_untl <- function(strt_val, cr_val, stop_val=0){

        cnt = 1

        if (cr_val < 0){

            while ((strt_val + cr_val) > (stop_val)){

                strt_val = strt_val + cr_val

                cnt = cnt + 1

            }

        }else{

            while ((strt_val + cr_val) < (stop_val)){

                strt_val = strt_val + cr_val

                cnt = cnt + 1

            }

        }

        return(cnt)

}

#' dcr_val
#'
#' Allow to get the end value after an incremental (or decremental loop)
#' 
#' @param strt_val is the start value
#' @param cr_val is the incremental or decremental value
#' @param stop_val is the value the loop has to stop
#' @examples
#' @examples
#'
#' print(dcr_val(strt_val=50, cr_val=-5, stop_val=5))
#'
#' [1] 5
#' 
#' print(dcr_val(strt_val=47, cr_val=-5, stop_val=5))
#' 
#' [1] 7
#' 
#' print(dcr_val(strt_val=50, cr_val=5, stop_val=450))
#' 
#' [1] 450
#' 
#' print(dcr_val(strt_val=53, cr_val=5, stop_val=450))
#' 
#' [1] 448
#' 
#' @export

dcr_val <- function(strt_val, cr_val, stop_val=0){

        cnt = 1

        if (cr_val < 0){

            while ((strt_val + cr_val) > (stop_val + cr_val / 2)){

                strt_val = strt_val + cr_val

                cnt = cnt + 1

            }

        }else{

            while ((strt_val + cr_val) < (stop_val + cr_val / 2)){

                strt_val = strt_val + cr_val

                cnt = cnt + 1

            }

        }

        return(strt_val)

}

#' converter_date
#' 
#' Allow to convert any date like second/minute/hour/day/month/year to either second, minute...year. The input date should not necessarily have all its time units (second, minute...) but all the time units according to a format. Example: "snhdmy" is for second, hour, minute, day, month, year. And "mdy" is for month, day, year.
#'
#' @param inpt_date is the input date
#' @param convert_to is the time unit the input date will be converted ("s", "n", "h", "d", "m", "y")
#' @param frmt is the format of the input date
#' @param sep_n is the separator of the input date. For example this input date "12-07-2012" has "-" as a separator
#' @examples 
#'
#' print(converter_date(inpt_date="14-04-11-2024", sep_="-", frmt="hdmy", convert_to="m"))
#' 
#' [1] 24299.15
#' 
#' print(converter_date(inpt_date="14-04-11-2024", sep_="-", frmt="hdmy", convert_to="y"))
#' 
#' [1] 2024.929
#'
#' print(converter_date(inpt_date="14-04-11-2024", sep_="-", frmt="hdmy", convert_to="s"))
#' 
#' [1] 63900626400
#'
#' print(converter_date(inpt_date="63900626400", sep_="-", frmt="s", convert_to="y"))
#'
#' [1] 2024.929
#'
#' print(converter_date(inpt_date="2024", sep_="-", frmt="y", convert_to="s"))
#'
#' [1] 63873964800
#' 
#' @export

converter_date <- function(inpt_date, convert_to, frmt="snhdmy", sep_="-"){

  is_divisible <- function(inpt_v=c(), divisible_v=c()){

        cnt = 1

        while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) == 0]

                cnt = cnt + 1

        }

        return(inpt_v)

  }

  leap_yr <- function(year){

          if (year == 0){ return(F) }

          if (year %% 4 == 0){
            
            if (year %% 100 == 0){
              
              if (year %% 400 == 0){
                
                bsx <- T
                
              }else{
                
                bsx <- F
                
              }
              
            }else{
              
              bsx <- T
              
            }
            
          }else{
            
            bsx <- F
            
          }

          return(bsx)

  }

  inpt_date <- unlist(strsplit(x=inpt_date, split=sep_)) 

  stay_date_v <- c("s", "n", "h", "d", "m", "y")

  stay_date_val <- c(0, 0, 0, 0, 0, 0)

  frmt <- unlist(strsplit(x=frmt, split=""))

  for (el in 1:length(frmt)){

          stay_date_val[grep(pattern=frmt[el], x=stay_date_v)] <- as.numeric(inpt_date[el]) 

  }

  if (stay_date_val[6] != 0 & stay_date_val[5] == 0){ stay_date_val[5] <- 1 }

  if (stay_date_val[5] != 0 & stay_date_val[4] == 0){ stay_date_val[4] <- 1 }
  
  l_dm1 <- c(31, 28, 31, 30, 31, 30, 31, 31,
            30, 31, 30, 31)

  l_dm2 <- c(31, 29, 31, 30, 31, 30, 31, 31,
            30, 31, 30, 31)

  if (!(leap_yr(year=stay_date_val[6])) & stay_date_val[6] != 0){

        l_dm <- l_dm1

  }else if (stay_date_val[6] == 0){

        l_dm <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  }else{

        l_dm <- l_dm2

  }

  may_bsx_v <- c(1:stay_date_val[6])

  may_bsx_v <- may_bsx_v[may_bsx_v > 0] 

  may_bsx_v <- may_bsx_v[(may_bsx_v %% 4) == 0]

  val_mult <- c((1 / 86400), (1 / 1440), (1/24), 1, 0, 365)

  day_val = 0

  for (dt in length(stay_date_val):1){

        day_val = day_val + stay_date_val[dt] * val_mult[dt] 

  }

  day_val = day_val + length(may_bsx_v[(may_bsx_v %% 100) != 0]) + length(is_divisible(inpt_v=may_bsx_v, divisible_v=c(100, 400))) 

  if (str_detect(string=stay_date_val[5], pattern="\\.")){

          all_part <- as.numeric(unlist(strsplit(x=as.character(stay_date_val[5]), split="\\.")))

          int_part <- all_part[1]

          if (int_part != 0){

                day_val = day_val + sum(l_dm[1:int_part])

          }else{ int_part <- 1 }

          day_val = day_val + l_dm1[int_part] * as.numeric(paste("0.", all_part[2], sep=""))

  }else if (stay_date_val[5] != 0){

        day_val = day_val + l_dm1[stay_date_val[5]] 

  }

  val_mult2 <- c(60, 60, 24, 1)

  idx_convert <- grep(pattern=convert_to, x=stay_date_v)

  if (idx_convert < 5){

        for (i in 4:idx_convert){

            day_val = day_val * val_mult2[i]

        }

        return(day_val)

  }else{

    year = 0

    l_dm <- l_dm1

    month = 0

    bsx_cnt = 0

    while ((day_val / sum(l_dm)) >= 1 ){

        l_dmb <- l_dm

        day_val2 = day_val

        day_val = day_val - sum(l_dm)

        month = month + 12

        year = year + 1

        if (!(leap_yr(year=year))){

                l_dm <- l_dm1

        }else{

                bsx_cnt = bsx_cnt + 1

                l_dm <- l_dm2

        } 

    }

    if (leap_yr(year=year)){

        day_val = day_val - 1

    }

    cnt = 1

    while ((day_val / l_dm[cnt]) >= 1){

        day_val = day_val - l_dm[cnt]

        month = month + 1

        cnt = cnt + 1 

    }

    month = month + (day_val / l_dm[cnt])

    if (convert_to == "m"){

            return(month)

    }else{

            year = year + ((month - 12 * year) / 12)

            return(year)

    }

  }

}

#' pattern_gettr 
#'
#' Search for pattern(s) contained in a vector in another vector and return a list containing matched one (first index) and their position (second index) according to these rules:
#' First case: Search for patterns strictly, it means that the searched pattern(s) will be matched only if the patterns containded in the vector that is beeing explored by the function are present like this c("pattern_searched", "other", ..., "pattern_searched") and not as c("other_thing pattern_searched other_thing", "other", ..., "pattern_searched other_thing") 
#' Second case: It is the opposite to the first case, it means that if the pattern is partially present like in the first position and the last, it will be considered like a matched pattern. REGEX can also be used as pattern 
#' @param word_ is the vector containing the patterns
#' @param vct is the vector being searched for patterns
#' @param occ a vector containing the occurence of the pattern in word_ to be matched in the vector being searched, if the occurence is 2 for the nth pattern in word_ and only one occurence is found in vct so no pattern will be matched, put "forever" to no longer depend on the occurence for the associated pattern
#' @param strict a vector containing the "strict" condition for each nth vector in word_ ("strict" is the string to activate this option)
#' @param btwn is a vector containing the condition ("yes" to activate this option) meaning that if "yes", all elements between two matched patern in vct will be returned , so the patterns you enter in word_ have to be in the order you think it will appear in vct 
#' @param all_in_word is a value (default set to "yes", "no" to activate this option) that, if activated, won't authorized a previous matched pattern to be matched again
#' @param notatall is a string that you are sure is not present in vct
#' @examples
#'
#' print(pattern_gettr(word_=c("oui", "non", "erer"), vct=c("oui", "oui", "non", "oui", "non", "opp", "opp", "erer", "non", "ok"), occ=c(1, 2, 1), btwn=c("no", "yes", "no"), strict=c("no", "no", "ee")))
#'
#' [[1]]
#' [1] 1 5 8
#' 
#' [[2]]
#' [1] "oui"  "non"  "opp"  "opp"  "erer"
#'
#' @export

pattern_gettr <- function(word_, vct, occ=c(1), strict, btwn, all_in_word="yes", notatall="###"){

  all_occ <- c()

  for (i in 1:length(word_)){ all_occ <- append(all_occ, 0) }

  if (length(btwn) < (length(occ) - 1)){

          to_app <- btwn[length(btwn)]

          for (i in length(btwn):(length(occ) - 2)){

                btwn <- append(btwn, to_app)

          }

  }

  if (length(strict) < length(occ)){

          to_app <- strict[length(strict)]

          for (i in length(strict):(length(occ) - 1)){

                strict <- append(strict, to_app)

          }

  }

  frst_occ <- c()

  occ_idx = 1

  get_ins = 0

  vct2 <- c()

  can_ins <- 0

  for (i in 1:length(vct)){

    to_compare = 0

    if (all_in_word == "yes"){

            if (strict[occ_idx] == "yes"){

                t = 1

                while (to_compare < 1 & t <= length(word_)){

                        if (nchar(word_[t]) == nchar(vct[i])){

                                v_bool <- str_detect(vct[i], word_[t])

                                to_compare = sum(v_bool)

                                if (to_compare > 0){indx <- t}

                        }

                        t = t + 1

                }

            }else{

                    v_bool <- str_detect(vct[i], word_)

                    to_compare =  sum(v_bool)

                    if (to_compare > 0){indx <- match(T, v_bool)}

            }

    }else{

       if (strict[occ_idx] == "yes"){

         t = 1

         while (t <= length(word_) & to_compare < 1){

           if (nchar(word_[t]) == nchar(vct[i])){

                v_bool <- str_detect(vct[i], word_[t])

                to_compare = sum(v_bool)

                if (to_compare > 0){indx <- t}

            }

            t = t + 1

         }

       }else{

        v_bool <- str_detect(vct[i], word_)

        to_compare =  sum(v_bool)

        if (to_compare > 0){indx <- match(T, v_bool)}

       }

    }

    if (to_compare > 0) {

      all_occ <- as.numeric(all_occ)

      all_occ[indx] = all_occ[indx] + 1

      all_occ <- as.character(all_occ)

      if (all_in_word == "no"){

              if (length(word_) >= 2){

                word_ <- word_[-indx]

              }else{

                word_[1] <- notatall

              }

      }

      if (all_occ[indx] == occ[indx] | occ[indx] == "forever"){

        can_ins <- 1
        
        frst_occ <- append(frst_occ, i)

        if (occ_idx <= length(btwn)){

          if (btwn[occ_idx] == "yes"){get_ins <- 1}else{get_ins <- 0}

        }else{

          get_ins <- 0

        }

        if ((occ_idx + 1) <= length(occ)){ occ_idx = occ_idx + 1 }

      }
      
    }

    if (get_ins == 1 | can_ins == 1){

        can_ins <- 0

        vct2 <- append(vct2, vct[i])

    }
    
  }
  
  return(list(frst_occ, vct2))
  
}

#' see_file
#'
#' Allow to get the filename or its extension
#' 
#' @param string_ is the input string
#' @param index_ext is the occurence of the dot that separates the filename and its extension
#' @param ext is a boolean that if set to TRUE, will return the file extension and if set to FALSE, will return filename
#' @examples
#' 
#' print(see_file(string_="file.abc.xyz"))
#'
#' [1] ".abc.xyz"
#'
#'  print(see_file(string_="file.abc.xyz", ext=F))
#'
#' [1] "file"
#'
#' print(see_file(string_="file.abc.xyz", index_ext=2))
#' 
#' [1] ".xyz"
#' 
#' @export

see_file <- function(string_, index_ext=1, ext=T){

        file_as_vec <- unlist(str_split(string_, ""))

        index_point <- grep("\\.", file_as_vec)[index_ext]

        if (ext == T){

                rtnl <- paste(file_as_vec[index_point:length(file_as_vec)], collapse="")

                return(rtnl)

        }else{

                rtnl <- paste(file_as_vec[1:(index_point-1)], collapse="")

                return(rtnl)

        }

}

#' see_inside
#'
#' Return a list containing all the column of the files in the current directory with a chosen file extension and its associated file and sheet if xlsx. For example if i have 2 files "out.csv" with 2 columns and "out.xlsx" with 1 column for its first sheet and 2 for its second one, the return will look like this:
#' c(column_1, column_2, column_3, column_4, column_5, unique_separator, "1-2-out.csv", "3-3-sheet_1-out.xlsx", 4-5-sheet_2-out.xlsx)
#' @param pattern_ is a vector containin the file extension of the spreadsheets ("xlsx", "csv"...)
#' @param path_ is the path where are located the files
#' @param sep_ is a vector containing the separator for each csv type file in order following the operating system file order, if the vector does not match the number of the csv files found, it will assume the separator for the rest of the files is the same as the last csv file found. It means that if you know the separator is the same for all the csv type files, you just have to put the separator once in the vector.
#' @param unique_sep is a pattern that you know will never be in your input files
#' @param rec is a boolean allows to get files recursively if set to TRUE, defaults to TRUE 
#' If x is the return value, to see all the files name, position of the columns and possible sheet name associanted with, do the following: 
#' Examples: 
#' print(x[(grep(unique_sep, x)[1]+1):length(x)]) 
#' #If you just want to see the columns do the following: 
#' print(x[1:(grep(unique_sep, x) - 1)])
#' @export

see_inside <- function(pattern_, path_=".", sep_=c(","), unique_sep="#####", rec=F){

        x <- c()

        for (i in 1:length(pattern_)){ 

                x <- append(x, list.files(path=path_, pattern=pattern_[i], recursive=rec))

        }

        rtnl <- list()

        rtnl2 <- c()

        sep_idx = 1
        
        for (i in 1:length(x)){

                file_as_vec <- unlist(str_split(x[i], ""))

                index_point <- grep("\\.", file_as_vec)[1]

                ext <- paste(file_as_vec[index_point:length(file_as_vec)], collapse="")

                if (ext == ".xlsx"){

                        allname <- getSheetNames(x[i]) 

                        for (t in 1:length(allname)){
                          
                                df <- data.frame(read.xlsx(x[i], sheet=allname[t]))

                                rtnl <- append(rtnl, df)

                                rtnl2 <- append(rtnl2, paste((length(rtnl)+1) , (length(rtnl)+ncol(df)), x[i], allname[t], sep="-"))

                        }

                }else{
                  
                        df <- data.frame(read.table(x[i], fill=T, sep=sep_[sep_idx]))

                        rtnl <- append(rtnl, df)

                        rtnl2 <- append(rtnl2, paste((length(rtnl)+1) , (length(rtnl)+ncol(df)), x[i], sep="-"))

                        sep_idx = sep_idx + 1

                        if (sep_idx > length(sep_)){

                                sep_ <- append(sep_, sep_[length(sep_)])

                        }

                }

        }

        return(c(rtnl, unique_sep, rtnl2))

}

#' val_replacer
#' 
#' Allow to replace value from dataframe to another one.
#'
#' @param df is the input dataframe
#' @param val_replaced is a vector of the value(s) to be replaced
#' @param val_replacor is the value that will replace val_replaced
#' @examples
#'
#' print(val_replacer(df=data.frame(c(1, "oo4", T, F), c(T, F, T, T)), val_replaced=c(T), val_replacor="NA"))
#'
#'   c.1...oo4...T..F. c.T..F..T..T.
#' 1                 1            NA
#' 2               oo4         FALSE
#' 3                NA            NA
#' 4             FALSE            NA
#' 
#' @export

val_replacer <- function(df, val_replaced, val_replacor=T, df_rpt=NA){
  
  for (i in 1:(ncol(df))){
    
      for (i2 in 1:length(val_replaced)){
        
        vec_pos <- grep(val_replaced[i2], df[, i])
          
        df[vec_pos, i] <- val_replacor
    
      }
    
  }
  
  return(df)
  
}

#' see_idx
#'
#' Returns a boolean vector to see if a set of elements contained in v1 is also contained in another vector (v2)
#' @param v1 is the first vector
#' @param v2 is the second vector
#' @examples
#'
#' print(see_idx(v1=c("oui", "non", "peut", "oo"), v2=c("oui", "peut", "oui")))
#'
#' [1]  TRUE FALSE  TRUE  FALSE
#'
#' @export

see_idx <- function(v1, v2, exclude_val="######"){
 
  rtnl <- c()
 
  for (i in 1:length(v1)){

    if (length(grep(pattern=v1[i], x=v2)) > 0){

            r_idx <- T

    }else{

            r_idx <- F

    }

    rtnl <- append(x=rtnl, values=r_idx)
    
  }
 
  return(rtnl)
 
}

#' fold_rec2 
#' 
#' Allow to find the directories and the subdirectories with a specified end and start depth value from a path. This function might be more powerfull than file_rec because it uses a custom algorythm that does not nee to perform a full recursive search before tuning it to only find the directories with a good value of depth. Depth example: if i have dir/dir2/dir3, dir/dir2b/dir3b, i have a depth equal to 3
#' @param xmax is the depth value
#' @param xmin is the minimum value of depth
#' @param pathc is the reference path, from which depth value is equal to 1
#' @export

fold_rec2 <- function(xmax, xmin=1, pathc="."){

        pathc2 <- pathc

        ref <- list.dirs(pathc, recursive=F)

        exclude_temp <- c()

        print(exclude_temp)

        exclude_f <- c("#")

        while (sum(exclude_f == ref) < length(ref)){

                if (length(grep("#", exclude_f)) > 0){

                        exclude_f <- c()

                }

                t = 1

                alf <- c("##")

                while (t <= xmax & length(alf) > 0){

                        alf <- list.dirs(pathc, recursive=F)

                        exclude_idx <- c()

                        if (length(exclude_temp) > 0){

                                for (i in 1:length(exclude_temp)){  

                                        in_t <- match(T, exclude_temp[i] == alf)

                                        if (is.na(in_t) == F){

                                                exclude_idx <- append(exclude_idx, in_t)

                                        }

                                } 

                        }

                        if (length(exclude_idx) > 0){ alf <- alf[-exclude_idx] }

                        if (length(alf) > 0 & t < xmax){

                                pathc <- alf[1]

                        }

                        t = t + 1

                }

                exclude_temp <- append(exclude_temp, pathc)

                ret_pathc <- pathc

                pathc <- paste(unlist(str_split(pathc, "/"))[1:str_count(pathc, "/")], collapse="/")

                if (pathc == pathc2){ exclude_f <- append(exclude_f, ret_pathc) }
                
        }

        ret <- grep(T, (str_count(exclude_temp, "/") < xmin))

        if (length(ret) > 0){

                return(exclude_temp[-ret])

        }else{

                return(exclude_temp)

        }

}

#' fold_rec
#'
#' Allow to get all the files recursively from a path according to an end and start depth value. If you want to have an other version of this function that uses a more sophisticated algorythm (which can be faster), check file_rec2. Depth example: if i have dir/dir2/dir3, dir/dir2b/dir3b, i have a depth equal to 3
#' @param xmax is the end depth value
#' @param xmin is the start depth value
#' @param pathc is the reference path 
#' @export

fold_rec <- function(xmax, xmin=1, pathc="."){

        vec <- list.dirs(pathc, recursive=T)

        rtnl <- c()

        print(vec)

        for (i in 1:length(vec)){

                if (str_count(vec[i], "/") <= xmax & str_count(vec[i], "/") >= xmin){

                        rtnl <- append(rtnl, vec[i])

                }

        }

        return(rtnl)

}

#' get_rec 
#'
#' Allow to get the value of directorie depth from a path.
#'
#' @param pathc is the reference path
#' example: if i have dir/dir2/dir3, dir/dir2b/dir3b, i have a depth equal to 3
#' @export

get_rec <- function(pathc="."){

        vec <- list.dirs(pathc, recursive=T)

        rtnl <- c()

        for (i in 1:length(vec)){

                rtnl <- append(rtnl, str_count(vec[i], "/"))

        }

        return(max(rtnl))

}

#' list_files
#' 
#' A list.files() based function addressing the need of listing the files with extension a or or extension b ...
#'
#' @param pathc is the path, can be a vector of multiple path because list.files() supports it.
#' @param patternc is a vector containing all the exensions you want
#' @export

list_files <- function(patternc, pathc="."){

       rtnl <- c()

       for (i in 1:length(patternc)){

               rtnl <- append(rtnl, list.files(path=pathc, pattern=patternc[i]))

       }

       return(sort(rtnl))

}

#' ptrn_twkr
#'
#' Allow to modify the pattern length of element in a vector according to arguments. What is here defined as a pattern is something like this xx-xx-xx or xx/xx/xxx... So it is defined by the separator
#' @param inpt_l is the input vector
#' @param depth is the number (numeric) of separator it will keep as a result. To keep the number of separator of the element that has the minimum amount of separator do depth="min" and depth="max" (character) for the opposite. This value defaults to "max".
#' @param sep is the separator of the pattern, defaults to "-"
#' @param default_val is the default val that will be placed between the separator, defaults to "00" 
#' @param add_sep defaults to TRUE. If set to FALSE, it will remove the separator for the patterns that are included in the interval between the depth amount of separator and the actual number of separator of the element.
#' @param end_ is if the default_val will be added at the end or at the beginning of each element that lacks length compared to depth
#'
#' @examples
#' 
#' v <- c("2012-06-22", "2012-06-23", "2022-09-12", "2022")
#'
#' ptrn_twkr(inpt_l=v, depth="max", sep="-", default_val="00", add_sep=TRUE)
#'
#' [1] "2012-06-22" "2012-06-23" "2022-09-12" "2022-00-00"
#'
#' ptrn_twkr(inpt_l=v, depth=1, sep="-", default_val="00", add_sep=TRUE)
#'
#' [1] "2012-06" "2012-06" "2022-09" "2022-00"
#' 
#'  ptrn_twkr(inpt_l=v, depth="max", sep="-", default_val="00", add_sep=TRUE, end_=F)
#'
#' [1] "2012-06-22" "2012-06-23" "2022-09-12" "00-00-2022"
#'
#' @export

ptrn_twkr <- function(inpt_l, depth="max", sep="-", 
                      default_val="0", add_sep=T, end_=T){
  
  ln <- length(inpt_l)
  
  if (depth == "min"){
    
    pre_val <- str_count(inpt_l[1], sep)
    
    for (i in 2:ln){
      
      if (str_count(inpt_l[i], sep) < pre_val){
        
        pre_val <- str_count(inpt_l[i], sep)
        
      }
      
    }
    
    depth <- pre_val
    
  }

  if (depth == "max"){
    
    pre_val <- str_count(inpt_l[1], sep)
    
    for (i in 2:ln){
      
      if (str_count(inpt_l[i], sep) > pre_val){
        
        pre_val <- str_count(inpt_l[i], sep)
        
      }
      
    }
    
    depth <- pre_val
    
  }

  if (end_){

          for (I in 1:ln){
           
            hmn <- str_count(inpt_l[I], "-")
            
            if (hmn < depth){
             
              inpt_l[I] <- paste0(inpt_l[I], sep, default_val)

              diff <- depth - hmn - 1

              if (diff > 0){
              
                        if (add_sep == T){
                          
                          for (i in 1:diff){
                          
                            inpt_l[I] <- paste0(inpt_l[I], sep, default_val)
                          
                          }
                        
                        }else{
                          
                          for (i in 1:diff){
                            
                            inpt_l[I] <- paste0(inpt_l[I], default_val)
                            
                          }
                          
                        }

             }
            
            }else if(depth < hmn){

                if (add_sep == T){

                        inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse=sep)

                }else{

                        inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse="")
               
                }

            }

          }
  
  }else{

        for (I in 1:ln){
           
            hmn <- str_count(inpt_l[I], "-")
            
            if (hmn < depth){
             
              inpt_l[I] <- paste0(default_val, sep, inpt_l[I])

              diff <- depth - hmn - 1

              if (diff > 0){
              
                        if (add_sep == T){
                          
                          for (i in 1:diff){
                          
                            inpt_l[I] <- paste0(default_val, sep, inpt_l[I])
                          
                          }
                        
                        }else{
                          
                          for (i in 1:diff){
                            
                            inpt_l[I] <- paste0(default_val, inpt_l[I])
                            
                          }
                          
                        }

             }
            
            }else if(depth < hmn){

                if (add_sep == T){

                        inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse=sep)

                }else{

                        inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse="")
               
                }

            }

          }

  }

  return(inpt_l)
  
}

#' fillr
#' 
#' Allow to fill a vector by the last element n times
#' @param inpt_v is the input vector
#' @param ptrn_fill is the pattern used to detect where the function has to fill the vector by the last element n times. It defaults to "...\\d" where "\\d" is the regex for an int value. So this paramater has to have "\\d" which designates n.
#' @examples
#'
#' print(fillr(c("a", "b", "...3", "c")))
#'
#' [1] "a" "b" "b" "b" "b" "c"
#'
#' @export

fillr <- function(inpt_v, ptrn_fill="...\\d"){
  
  ptrn <- grep(ptrn_fill, inpt_v)

  while (length(ptrn) > 0){
   
    ptrn <- grep(ptrn_fill, inpt_v)

    idx <- ptrn[1] 
    
    untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
   
    pre_val <- inpt_v[(idx - 1)]

    inpt_v[idx] <- pre_val

    if (untl > 0){
    
      for (i in 1:untl){
        
        inpt_v <- append(inpt_v, pre_val, idx)
        
      }
      
    }

  ptrn <- grep(ptrn_fill, inpt_v)
    
  }
  
  return(inpt_v)
  
}

#' ptrn_switchr
#' 
#' Allow to switch, copy pattern for each element in a vector. Here a pattern is the values that are separated by a same separator. Example: "xx-xxx-xx" or "xx/xx/xxxx". The xx like values can be swicthed or copied from whatever index to whatever index. Here, the index is like this 1-2-3 etcetera, it is relative of the separator. 
#' @param inpt_l is the input vector
#' @param f_idx_l is a vector containing the indexes of the pattern you want to be altered.
#' @param t_idx_l is a vector containing the indexes to which the indexes in f_idx_l are related.
#' @param sep is the separator, defaults to "-"
#' @param default_val is the default value , if not set to NA, of the pattern at the indexes in f_idx_l. If it is not set to NA, you do not need to fill t_idx_l because this is the vector containing the indexes of the patterns that will be set as new values relatively to the indexes in f_idx_l. Defaults to NA.
#' @examples
#' 
#' print(ptrn_switchr(inpt_l=c("2022-01-11", "2022-01-14", "2022-01-21", 
#' "2022-01-01"), f_idx_l=c(1, 2, 3), t_idx_l=c(3, 2, 1)))
#'
#' [1] "11-01-2022" "14-01-2022" "21-01-2022" "01-01-2022"
#'
#' print(ptrn_switchr(inpt_l=c("2022-01-11", "2022-01-14", "2022-01-21", 
#' "2022-01-01"), f_idx_l=c(1), default_val="ee"))
#' 
#' [1] "ee-01-11" "ee-01-14" "ee-01-21" "ee-01-01"
#'
#' @export

ptrn_switchr <- function(inpt_l, f_idx_l=c(), t_idx_l=c(), sep="-", default_val=NA){

        if (is.na(default_val) == T){

                for (I in 1:length(inpt_l)){

                        pre_val <- unlist(strsplit(inpt_l[I], split=sep))

                        pre_val2 <- pre_val

                        for (i in 1:length(f_idx_l)){

                               pre_val2[f_idx_l[i]] <- pre_val[t_idx_l[i]]

                        }

                        inpt_l[I] <- paste(pre_val2, collapse=sep)

                }

        }else{

                for (I in 1:length(inpt_l)){

                        pre_val <- unlist(strsplit(inpt_l[I], split=sep))

                        for (i in 1:length(f_idx_l)){

                               pre_val[f_idx_l[i]] <- default_val

                        }

                        inpt_l[I] <- paste(pre_val, collapse=sep)

                }

        }

        return(inpt_l)

}

#' globe
#'
#' Allow to calculate the distances between a set of geographical points and another established geographical point. If the altitude is not filled, so the result returned won't take in count the altitude.
#' @param lat_f is the latitude of the established geographical point
#' @param long_f is the longitude of the established geographical point
#' @param alt_f is the altitude of the established geographical point, defaults to NA
#' @param lat_n is a vector containing the latitude of the set of points
#' @param long_n is a vector containing the longitude of the set of points
#' @param alt_n is a vector containing the altitude of the set of points, defaults to NA
#' @examples
#' 
#' print(globe(lat_f=23, long_f=112, alt_f=NA, lat_n=c(2, 82), long_n=c(165, -55), alt_n=NA)) 
#'
#' [1] 6342.844 7059.080
#'
#' print(globe(lat_f=23, long_f=112, alt_f=8, lat_n=c(2, 82), long_n=c(165, -55), alt_n=c(8, -2)))
#'
#' [1] 6342.844 7059.087
#'
#' @export

globe <- function(lat_f, long_f, alt_f=NA, lat_n, long_n, alt_n=NA){

        rtn_l <- c()

        for (i in 1:length(lat_n)){

               sin_comp <- abs(sin(pi * ((lat_n[i] + 90) / 180)))

               if (abs(long_f - long_n[i]) != 0){

                       delta_long <- (40075 / (360 / abs(long_f - long_n[i]))) * sin_comp

               }else{

                       delat_long <- 0

               }

               if (abs(lat_f - lat_n[i]) != 0){

                        delta_lat <- 20037.5 / (180 / abs(lat_f - lat_n[i]))

               }else{

                        delta_lat <- 0

               }

               delta_f <- (delta_lat ** 2 + delta_long ** 2) ** 0.5

               if (is.na(alt_n[i]) == F & is.na(alt_f) == F){

                        delta_f <- ((alt_n[i] - alt_f) ** 2 + delta_f ** 2) ** 0.5

               }

               rtn_l <- append(rtn_l, delta_f, after=length(rtn_l))

        }

        return(rtn_l)

}

#' geo_min
#' 
#' Return a dataframe containing the nearest geographical points (row) according to established geographical points (column).
#' @param inpt_df is the input dataframe of the set of geographical points to be classified, its firts column is for latitude, the second for the longitude and the third, if exists, is for the altitude. Each point is one row.
#' @param established_df is the dataframe containing the coordiantes of the established geographical points
#' @examples
#' 
#' in_ <- data.frame(c(11, 33, 55), c(113, -143, 167))
#' 
#' in2_ <- data.frame(c(12, 55), c(115, 165))
#' 
#' print(geo_min(inpt_df=in_, established_df=in2_))
#'
#'          X1       X2
#' 1   245.266       NA
#' 2 24200.143       NA
#' 3        NA 127.7004
#' 
#' in_ <- data.frame(c(51, 23, 55), c(113, -143, 167), c(6, 5, 1))
#' 
#' in2_ <- data.frame(c(12, 55), c(115, 165), c(2, 5))
#' 
#' print(geo_min(inpt_df=in_, established_df=in2_))
#'
#'         X1       X2
#' 1       NA 4343.720
#' 2 26465.63       NA
#' 3       NA 5825.517
#' 
#' @export
geo_min <- function(inpt_df, established_df){

       globe <- function(lat_f, long_f, alt_f=NA, lat_n, long_n, alt_n=NA){

               sin_comp <- abs(sin(pi * ((lat_n + 90) / 180)))

               if (abs(long_f - long_n) != 0){

                       delta_long <- (40075 / (360 / abs(long_f - long_n))) * sin_comp

               }else{

                       delat_long <- 0

               }

               if (abs(lat_f - lat_n) != 0){

                        delta_lat <- 20037.5 / (180 / abs(lat_f - lat_n))

               }else{

                        delta_lat <- 0

               }

               delta_f <- (delta_lat ** 2 + delta_long ** 2) ** 0.5

               if (is.na(alt_n) == F & is.na(alt_f) == F){

                        delta_f <- ((alt_n - alt_f) ** 2 + delta_f ** 2) ** 0.5

               }

               return(delta_f)

       }

      flag_delta_l <- c()

      rtn_df <- data.frame(matrix(nrow=nrow(inpt_df), ncol=nrow(established_df)))

      if (ncol(inpt_df) == 3){

              for (i in 1:nrow(inpt_df)){

                      flag_delta_l <- c(flag_delta_l, globe(lat_f=established_df[1, 1], long_f=established_df[1, 2], alt_f=established_df[1, 3], lat_n=inpt_df[i, 1], long_n=inpt_df[i, 2], alt_n=inpt_df[i, 3]))

              }

              rtn_df[,1] <- flag_delta_l

              if (nrow(established_df) > 1){

                      for (I in 2:nrow(established_df)){

                               for (i in 1:nrow(inpt_df)){

                                        idx <- which(is.na(rtn_df[i,]) == F)

                                        res <- globe(lat_f=established_df[I, 1], long_f=established_df[I, 2], alt_f=established_df[I, 3], lat_n=inpt_df[i, 1], long_n=inpt_df[i, 2], alt_n=inpt_df[i, 3])

                                        if (rtn_df[i, 1:(I-1)][idx] > res){

                                               rtn_df[i, I] <- rtn_df[i, idx] 

                                               rtn_df[i, idx] <- NA 

                                        }

                                }

                        }

              }

      }else{

              for (i in 1:nrow(inpt_df)){

                      flag_delta_l <- c(flag_delta_l, globe(lat_f=established_df[1, 1], long_f=established_df[1, 2], lat_n=inpt_df[i, 1], long_n=inpt_df[i, 2]))

              }

              rtn_df[,1] <- flag_delta_l

              if (nrow(established_df) > 1){

                      for (I in 2:nrow(established_df)){

                               for (i in 1:nrow(inpt_df)){

                                        idx <- which(is.na(rtn_df[i,]) == F)

                                        res <- globe(lat_f=established_df[I, 1], long_f=established_df[I, 2], lat_n=inpt_df[i, 1], long_n=inpt_df[i, 2])

                                        if (rtn_df[i, 1:(I-1)][idx] > res){

                                               rtn_df[i, I] <- res 

                                               rtn_df[i, idx] <- NA 

                                        }

                               }

                        }

              }

      }

      return(rtn_df)

}

#' nestr_df2
#'
#' Allow to write a special value (1a) in the cells of a dataframe (1b) that correspond (row and column) to whose of another dataframe (2b) that return another special value (2a). The cells whose coordinates do not match the coordinates of the dataframe (2b), another special value can be written (3a) if not set to NA. 
#' @param inptf_df is the input dataframe (1b)
#' @param rtn_pos is the special value (1a)
#' @param rtn_neg is the special value (3a) 
#' @param nestr_df is the dataframe (2b)
#' @param yes_val is the special value (2a) 
#' @examples
#'
#' print(nestr_df2(inptf_df=data.frame(c(1, 2, 1), c(1, 5, 7)), rtn_pos="yes", 
#' rtn_neg="no", nestr_df=data.frame(c(TRUE, FALSE, TRUE), c(FALSE, FALSE, TRUE)), yes_val=TRUE)) 
#'
#'   c.1..2..1. c.1..5..7.
#' 1        yes         no
#' 2         no         no
#' 3        yes        yes
#' 
#' @export

nestr_df2 <- function(inptf_df, rtn_pos, rtn_neg=NA, nestr_df, yes_val=T){

        if (is.na(rtn_neg) == T){

                for (I in 1:ncol(nestr_df)){

                        for (i in 1:nrow(nestr_df)){

                                if (nestr_df[i, I] == yes_val){

                                        inptf_df[i, I] <- rtn_pos

                                }

                        }

                }

        }else{

                for (I in 1:ncol(nestr_df)){

                        for (i in 1:nrow(nestr_df)){

                                if (nestr_df[i, I] == yes_val){

                                        inptf_df[i, I] <- rtn_pos

                                }else{

                                        inptf_df[i, I] <- rtn_neg

                                }

                        }

                }

        }

    return(inptf_df)

}

#' nestr_df1
#'
#' Allow to write a value (1a) to a dataframe (1b) to its cells that have the same coordinates (row and column) than the cells whose value is equal to a another special value (2a), from another another dataframe (2b). The value (1a) depends of the cell  value coordinates of the third dataframe (3b). If a cell coordinates (1c) of the first dataframe (1b) does not correspond to the coordinates of a good returning cell value (2a) from the dataframe (2b), so this cell (1c) can have its value changed to the same cell coordinates value (3a) of a third dataframe (4b), if (4b) is not set to NA.
#' @param inptf_df is the input dataframe (1b)
#' @param inptt_pos_df is the dataframe (2b) that corresponds to the (1a) values
#' @param inptt_neg_df is the dataframe (4b) that has the (3a) values, defaults to NA
#' @param nestr_df is the dataframe (2b) that has the special value (2a)
#' @param yes_val is the special value (2a)
#' @examples
#'
#' print(nestr_df1(inptf_df=data.frame(c(1, 2, 1), c(1, 5, 7)), 
#' inptt_pos_df=data.frame(c(4, 4, 3), c(2, 1, 2)), 
#' inptt_neg_df=data.frame(c(44, 44, 33), c(12, 12, 12)), 
#' nestr_df=data.frame(c(TRUE, FALSE, TRUE), c(FALSE, FALSE, TRUE)), yes_val=TRUE)) 
#'
#'   c.1..2..1. c.1..5..7.
#' 1          4         12
#' 2         44         12
#' 3          3          2
#'
#' print(nestr_df1(inptf_df=data.frame(c(1, 2, 1), c(1, 5, 7)), 
#' inptt_pos_df=data.frame(c(4, 4, 3), c(2, 1, 2)), 
#' inptt_neg_df=NA, 
#' nestr_df=data.frame(c(TRUE, FALSE, TRUE), c(FALSE, FALSE, TRUE)), yes_val=TRUE))
#'
#'    c.1..2..1. c.1..5..7.
#' 1          4          1
#' 2          2          5
#' 3          3          2
#' 
#' @export

nestr_df1 <- function(inptf_df, inptt_pos_df, nestr_df, yes_val=T, inptt_neg_df=NA){

        if (all(is.na(inptt_neg_df)) == T){

                for (I in 1:ncol(nestr_df)){

                        for (i in 1:nrow(nestr_df)){

                                if (nestr_df[i, I] == yes_val){

                                        inptf_df[i, I] <- inptt_pos_df[i, I]

                                }

                        }

                }

        }else{

                for (I in 1:ncol(nestr_df)){

                        for (i in 1:nrow(nestr_df)){

                                if (nestr_df[i, I] == yes_val){

                                        inptf_df[i, I] <- inptt_pos_df[i, I]

                                }else{

                                        inptf_df[i, I] <- inptt_neg_df[i, I]

                                }

                        }

                }

        }

    return(inptf_df)

}

#' groupr_df
#' 
#' Allow to create groups from a dataframe. Indeed, you can create conditions that lead to a flag value for each cell of the input dataframeaccording to the cell value. This function is based on see_df and nestr_df2 functions.
#' @param inpt_df is the input dataframe
#' @param condition_lst is a list containing all the condition as a vector for each group
#' @param val_lst is a list containing all the values associated with condition_lst as a vector for each group
#' @param conjunction_lst is a list containing all the conjunctions associated with condition_lst and val_lst as a vector for each group
#' @param rtn_val_pos is a vector containing all the group flag value like this ex: c("flag1", "flag2", "flag3") 
#' @export
#' @examples interactive()
#' 
#' df1 <- data.frame(c(1, 2, 1), c(45, 22, 88), c(44, 88, 33))
#'                                                                       
#' val_lst <- list(list(c(1), c(1)), list(c(2)), list(c(44, 88)))
#' 
#' condition_lst <- list(c(">", "<"), c("%%"), c("==", "=="))
#' 
#' conjunction_lst <- list(c("|"), c(), c("|"))
#' 
#' rtn_val_pos <- c("+", "++", "+++")
#' 
#' print(groupr_df(inpt_df=df1, val_lst=val_lst, condition_lst=condition_lst, 
#' conjunction_lst=conjunction_lst, rtn_val_pos=rtn_val_pos))
#' 
#'     X1  X2  X3
#' 1 <NA>   + +++
#' 2   ++  ++ +++
#' 3 <NA> +++   +
#' 
#' @export

groupr_df <- function(inpt_df, condition_lst, val_lst, conjunction_lst, rtn_val_pos=c()){
 
        nestr_df2 <- function(inptf_df, rtn_pos, rtn_neg=NA, nestr_df, yes_val=T){

                if (is.na(rtn_neg) == T){

                        for (I in 1:ncol(nestr_df)){

                                for (i in 1:nrow(nestr_df)){

                                        if (nestr_df[i, I] == yes_val){

                                                inptf_df[i, I] <- rtn_pos

                                        }

                                }

                        }

                }else{

                        for (I in 1:ncol(nestr_df)){

                                for (i in 1:nrow(nestr_df)){

                                        if (nestr_df[i, I] == yes_val){

                                                inptf_df[i, I] <- rtn_pos

                                        }else{

                                                inptf_df[i, I] <- rtn_neg

                                        }

                                }

                        }

                }

            return(inptf_df)

        }
 
        see_df <- function(df, condition_l, val_l, conjunction_l=c(), rt_val=T, f_val=F){

                if (length(condition_l) > 1 & length(conjunction_l) < (length(condition_l) - 1)){

                        for (i in (length(conjunction_l)+1):length(condiction_l)){

                                conjunction_l <- append(conjunction_l, conjunction_l[length(conjunction_l)])

                        }

                }

                df_rtnl <- data.frame(matrix(f_val, ncol=ncol(df), nrow=nrow(df)))

                all_op <- c("==", ">", "<", "!=", "%%")

                for (I in 1:ncol(df)){

                        for (i in 1:nrow(df)){

                                checked_l <- c()

                                previous = 1

                                for (t in 1:length(condition_l)){

                                        already <- 0

                                        if (condition_l[t] == "==" & already == 0){

                                                if (df[i, I] %in% unlist(val_l[t])){

                                                        checked_l <- append(checked_l, T)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                df_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        df_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                        if (condition_l[t] == ">" & already == 0){

                                                if (all(df[i, I] > unlist(val_l[t])) == T){

                                                        checked_l <- append(checked_l, T)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                df_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        df_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                        if (condition_l[t] == "<" & already == 0){

                                                if (all(df[i, I] < unlist(val_l[t]))){

                                                        checked_l <- append(checked_l, T)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                df_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        df_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                        if (condition_l[t] == "!=" & already == 0){

                                                if (!(df[i, I] %in% unlist(val_l[t])) == T){

                                                        checked_l <- append(checked_l, T)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                df_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        df_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                        if (condition_l[t] == "%%" & already == 0){

                                                if (sum(df[i, I] %% unlist(val_l[t])) == 0){

                                                        checked_l <- append(checked_l, T)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                df_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                df_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        df_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                }
                                
                        }

                }

          return(df_rtnl)

        }
              
        rtn_df <- data.frame(matrix(nrow=nrow(inpt_df), ncol=ncol(inpt_df)))

        for (I in 1:length(condition_lst)){

                pre_df <- see_df(df=inpt_df, condition_l=unlist(condition_lst[I]), val_l=unlist(val_lst[I]), conjunction_l=unlist(conjunction_lst[I])) 

                rtn_df <- nestr_df2(inptf_df=rtn_df, nestr_df=pre_df, rtn_pos=rtn_val_pos[I], rtn_neg=NA)  

        }

        return(rtn_df)

}

#' occu
#'
#' Allow to see the occurence of each variable in a vector. Returns a datafame with, as the first column, the all the unique variable of the vector and , in he second column, their occurence respectively.
#' 
#' @param inpt_v the input dataframe
#' @examples
#'
#' print(occu(inpt_v=c("oui", "peut", "peut", "non", "oui")))
#'
#'    var occurence
#' 1  oui         2
#' 2 peut         2
#' 3  non         1
#' 
#' @export

occu <- function(inpt_v){

    presence <- which(inpt_v == "")

    if (length(presence) > 0){ inpt_v <- inpt_v[-presence] }

    occu_v <- c()
    
    modal_v <- c()

    for (el in inpt_v){
      
      if (length(grep(el, modal_v)) == 1){
        
        idx <- which(modal_v == el)
        
        occu_v[idx] = occu_v[idx] + 1
        
      }else{
        
        occu_v <- append(x=occu_v, values=1, after=length(occu_v))
        
        modal_v <- append(x=modal_v, values=el, after=length(occu_v))
       
      }
    
    }

    return(data.frame("var"=modal_v, "occurence"=occu_v))
 
}

#' all_stat
#'
#' Allow to see all the main statistics indicators (mean, median, variance, standard deviation, sum, max, min, quantile) of variables in a dataframe by the modality of a variable in a column of the input datarame. In addition to that, you can get the occurence of other qualitative variables by your chosen qualitative variable, you have just to precise it in the vector "stat_var" where all the statistics indicators are given with "occu-var_you_want/".
#' @param inpt_v is the modalities of the variables 
#' @param var_add is the variables you want to get the stats from
#' @param stat_var is the stats indicators you want
#' @param inpt_df is the input dataframe
#' @examples
#'
#' df <- data.frame("mod"=c("first", "seco", "seco", "first", "first", "third", "first"), 
#'                 "var1"=c(11, 22, 21, 22, 22, 11, 9), 
#'                "var2"=c("d", "d", "z", "z", "z", "d", "z"), 
#'                "var3"=c(45, 44, 43, 46, 45, 45, 42),
#'               "var4"=c("A", "A", "A", "A", "B", "C", "C"))
#'
#' print(all_stat(inpt_v=c("first", "seco"), var_add = c("var1", "var2", "var3", "var4"), 
#'  stat_var=c("sum", "mean", "median", "sd", "occu-var2/", "occu-var4/", "variance", "quantile-0.75/"), 
#'  inpt_df=df))
#'
#'    modal_v var_vector occu sum mean  med standard_devaition         variance
#' 1    first                                                                  
#' 2                var1       64   16 16.5   6.97614984548545 48.6666666666667
#' 3              var2-d    1                                                  
#' 4              var2-z    3                                                  
#' 5                var3      178 44.5   45   1.73205080756888                3
#' 6              var4-A    2                                                  
#' 7              var4-B    1                                                  
#' 8              var4-C    1                                                  
#' 9     seco                                                                  
#' 10               var1       43 21.5 21.5  0.707106781186548              0.5
#' 11             var2-d    1                                                  
#' 12             var2-z    1                                                  
#' 13               var3       87 43.5 43.5  0.707106781186548              0.5
#' 14             var4-A    2                                                  
#' 15             var4-B    0                                                  
#' 16             var4-C    0                                                  
#'    quantile-0.75
#' 1               
#' 2             22
#' 3               
#' 4               
#' 5          45.25
#' 6               
#' 7               
#' 8               
#' 9               
#' 10         21.75
#' 11              
#' 12              
#' 13         43.75
#' 14              
#' 15              
#' 16              
#'
#' @export

all_stat <- function(inpt_v, var_add=c(), stat_var=c(), inpt_df){
 
  presence <- which(inpt_v == "")
    
  if (length(presence) > 0){ inpt_v <- inpt_v[-presence] }
  
  fillr <- function(inpt_v, ptrn_fill="...\\d"){
    
    ptrn <- grep(ptrn_fill, inpt_v)
    
    while (length(ptrn) > 0){
      
      ptrn <- grep(ptrn_fill, inpt_v)
      
      idx <- ptrn[1]
      
      untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
      
      pre_val <- inpt_v[(idx - 1)]
      
      inpt_v[idx] <- pre_val
      
      if (untl > 0){
        
        for (i in 1:untl){
          
          inpt_v <- append(inpt_v, pre_val, idx)
          
        }
        
      }
      
      ptrn <- grep(ptrn_fill, inpt_v)
      
    }
    
    return(inpt_v)
    
  }
 
  pre_var <- grep("occu-", stat_var)

  col_ns <- colnames(inpt_df)

  if (length(pre_var) > 0){ 

          u_val <- c()

          mod_idx <- c()

          idx_col <- c()

          for (idx in pre_var){

                  col_ <- unlist(strsplit(stat_var[idx], split=""))

                  end_beg <- str_locate(stat_var[idx], "-(.*?)/")

                  col_2 <- paste(col_[(end_beg[1]+1):(end_beg[2]-1)], collapse="")

                  col_ <- which(col_ns == col_2)[1] 

                  un_v <- unique(df[, col_])

                  for (i in 1:length(un_v)){ idx_col <- c(idx_col, col_) }

                  pre_occu <- paste(col_2, un_v, sep="-")

                  u_val <- c(u_val, un_v)

                  idx_vd <- which(var_add == col_2)

                  var_add[idx_vd] <- pre_occu[1] 

                  var_add <- append(x=var_add, values=pre_occu[2:length(pre_occu)], after=idx_vd)

                  mod_idx <- c(mod_idx, c(idx_vd:(idx_vd+length(un_v)-1)))

          }

  }

  extend <- paste("...", as.character(length(var_add) - 1))

  if (length(var_add) > 0){
  
    list_stat <- list()
    
    modal_v <- c()
    
    var_vector <- c()
    
    for (el in inpt_v){
      
      modal_v <- c(modal_v, el, fillr(inpt_v=c("", extend)))
      
      var_vector <- c(var_vector, "", var_add)
      
    }

    rtn_df <- data.frame(modal_v, var_vector)

    pre_length_var_add <- length(var_add)

    if (length(mod_idx) > 0){

        vec_cur <- c(matrix(nrow=length(var_vector), ncol=1, data=""))

        for (vr in 1:length(inpt_v)){

            for (idx in 1:length(mod_idx)){

                cur_col <- df[, idx_col[idx]]

                vec_cur[length(var_add) * (vr - 1) + mod_idx[idx] + vr] <- sum(cur_col[df[, 1] == inpt_v[vr]] == u_val[idx])

            }

        }

        stat_var <- stat_var[-grep("occu-", stat_var)]

        rtn_df <- cbind(rtn_df, "occu"=vec_cur)

        var_add <- var_add[-mod_idx]

    }

    mod_idx <- c(1:pre_length_var_add)[-mod_idx]

    for (st in stat_var){

        vec_cur <- c(matrix(nrow=length(var_vector), ncol=1, data=""))

        if (st == "max"){

            for (vr in 1:length(inpt_v)){

                for (idx in 1:length(var_add)){

                    cur_col <- df[, which(col_ns == var_add[idx])]

                    vec_cur[pre_length_var_add * (vr - 1) + mod_idx[idx] + vr] <- max(cur_col[df[,1] == inpt_v[vr]]) 

                }

            }

            rtn_df <- cbind(rtn_df, "max"=vec_cur)

        }

        if (st == "min"){

            for (vr in 1:length(inpt_v)){

                for (idx in 1:length(var_add)){

                    cur_col <- df[, which(col_ns == var_add[idx])]

                    vec_cur[pre_length_var_add * (vr - 1) + mod_idx[idx] + vr] <- min(cur_col[df[,1] == inpt_v[vr]]) 

                }

            }

            rtn_df <- cbind(rtn_df, "min"=vec_cur)

        }

        if (st == "variance"){

            for (vr in 1:length(inpt_v)){

                for (idx in 1:length(var_add)){

                    cur_col <- df[, which(col_ns == var_add[idx])]

                    vec_cur[pre_length_var_add * (vr - 1) + mod_idx[idx] + vr] <- var(cur_col[df[,1] == inpt_v[vr]]) 

                }

            }

            rtn_df <- cbind(rtn_df, "variance"=vec_cur)

        }
        
        if (st == "sd"){

            for (vr in 1:length(inpt_v)){

                for (idx in 1:length(var_add)){

                    cur_col <- df[, which(col_ns == var_add[idx])]

                    vec_cur[pre_length_var_add * (vr - 1) + mod_idx[idx] + vr] <- sd(cur_col[df[,1] == inpt_v[vr]]) 

                }

            }

            rtn_df <- cbind(rtn_df, "standard_devaition"=vec_cur)

        }
        
        if (st == "sum"){

            for (vr in 1:length(inpt_v)){

                for (idx in 1:length(var_add)){

                    cur_col <- df[, which(col_ns == var_add[idx])]

                    vec_cur[pre_length_var_add * (vr - 1) + mod_idx[idx] + vr] <- sum(cur_col[df[,1] == inpt_v[vr]]) 

                }

            }

            rtn_df <- cbind(rtn_df, "sum"=vec_cur)

        }

        if (st == "median"){

            for (vr in 1:length(inpt_v)){

                for (idx in 1:length(var_add)){

                    cur_col <- df[, which(col_ns == var_add[idx])]

                    vec_cur[pre_length_var_add * (vr - 1) + mod_idx[idx] + vr] <- median(cur_col[df[,1] == inpt_v[vr]]) 

                }

            }

            rtn_df <- cbind(rtn_df, "med"=vec_cur)

        }

        if (length(grep("quantile", st)) > 0){

            idx_v <- str_locate(st, "-(.*?)/")

            nb_quant <- as.numeric(paste(unlist(strsplit(x=st, split=""))[(idx_v[1]+1):(idx_v[2]-1)], collapse=""))

            for (vr in 1:length(inpt_v)){

                for (idx in 1:length(var_add)){

                    cur_col <- df[, which(col_ns == var_add[idx])]

                    vec_cur[pre_length_var_add * (vr - 1) + mod_idx[idx] + vr] <- quantile(cur_col[df[,1] == inpt_v[vr]], 
                    probs=nb_quant) 

                }

            }

        rtn_df <- cbind(rtn_df, "X"=vec_cur)

        colnames(rtn_df)[length(colnames(rtn_df))] <- paste("quantile-", as.character(nb_quant), sep="")

        }

        if (st == "mean"){

            for (vr in 1:length(inpt_v)){

                for (idx in 1:length(var_add)){

                    cur_col <- df[, which(col_ns == var_add[idx])]

                    vec_cur[pre_length_var_add * (vr - 1) + mod_idx[idx] + vr] <- mean(cur_col[df[,1] == inpt_v[vr]]) 

                }

            }

        rtn_df <- cbind(rtn_df, "mean"=vec_cur)

        }

    }
    
  }else{ df <- data.frame(inpt_v) }

  return(rtn_df)

}

#' inter_min
#'
#' Takes as input a list of vectors composed of ints or floats ascendly ordered (intervals) that can have a different step to one of another element ex: list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3))
#' This function will return the list of vectors with the same steps preserving the begin and end value of each interval.
#' The way the algorythmn searches the common step of all the sub-lists is also given by the user as a parameter, see `how_to` paramaters.
#' @param inpt_l is the input list containing all the intervals
#' @param  min_ is a value you are sure is superior to the maximum step value in all the intervals
#' @param sensi is the decimal accuracy of how the difference between each value n to n+1 in an interval is calculated
#' @param sensi2 is the decimal accuracy of how the value with the common step is calculated in all the intervals
#' @param how_to_op is a vector containing the operations to perform to the pre-common step value, defaults to only "divide". The operations can be "divide", "substract", "multiply" or "add". All type of operations can be in this parameter.
#' @param how_to_val is a vector containing the value relatives to the operations in `hot_to_op`, defaults to 3
#' output from ex:
#' @examples
#'
#' print(inter_min(inpt_l=list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3))))
#'
#'  [[1]]
#'  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8
#' [20] 1.9 2.0 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3.0 3.1 3.2 3.3 3.4 3.5 3.6 3.7
#' [39] 3.8 3.9 4.0
#' 
#' [[2]]
#'  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8
#' [20] 1.9 2.0 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3.0 3.1 3.2 3.3 3.4 3.5 3.6 3.7
#' [39] 3.8 3.9 4.0
#' 
#' [[3]]
#'  [1] 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3
#' 
#' @export

inter_min <- function(inpt_l, min_=1000, sensi=3, sensi2=3, how_to_op=c("divide"),
                      how_to_val=c(3)){

        fillr <- function(inpt_v, ptrn_fill="...\\d"){
  
          ptrn <- grep(ptrn_fill, inpt_v)

          while (length(ptrn) > 0){
           
            ptrn <- grep(ptrn_fill, inpt_v)

            idx <- ptrn[1] 
            
            untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
           
            pre_val <- inpt_v[(idx - 1)]

            inpt_v[idx] <- pre_val

            if (untl > 0){
            
              for (i in 1:untl){
                
                inpt_v <- append(inpt_v, pre_val, idx)
                
              }
              
            }

          ptrn <- grep(ptrn_fill, inpt_v)
            
          }
          
          return(inpt_v)
          
        }

        diff_v2 <- c()

        diff_v <- c()

        for (lst in 1:length(inpt_l)){

            pre_v <- unlist(inpt_l[lst])

            for (idx in 1:(length(pre_v)-1)){

                diff_v <- c(diff_v, round((pre_v[idx+1] - pre_v[idx]), sensi))

            }

            diff_v2 <- c(diff_v2, diff_v)

            if (min(diff_v) < min_){ 

                min_ <- min(diff_v)

                diff_v <- c()

            }

        }

        verify <- function(diff_v2, min_){

            for (delta in diff_v2){

                pre_val <- delta / min_ %% 1

                all_eq <- 1

                if (length(grep("\\.", as.character(pre_val))) > 0){ 

                        pre_val_str <- unlist(strsplit(as.character(pre_val), split="\\."))[2]

                        pre_val_str <- unlist(strsplit(pre_val_str, split=""))[1:sensi]

                        if (length(grep(NA, pre_val_str)) > 0){

                                untl <- length(grep(NA, pre_val_str))

                                pre_val_str <- c(pre_val_str[which(is.na(pre_val_str) == F)], "0")

                                pre_val_str <- fillr(inpt_v=c(pre_val_str, paste0("...", untl))) 

                        }

                        if (pre_val_str[length(pre_val_str)] != "9"){

                            all_eq <- 0

                        }else{

                                all_eq <- 1

                                for (i in 1:(length(pre_val_str)-1)){

                                    if (pre_val_str[i+1] != pre_val_str[i] | pre_val_str[i] != "9"){

                                            all_eq <- 0

                                    }

                                }

                        }

                }

                if (round(pre_val * (10 ** sensi), 0) != 0 & all_eq != 1){

                    ht <- how_to_op[1]

                    nb <- how_to_val[1]

                    if (length(how_to_op) > 1){

                        how_to_op <- how_to_op[2:length(how_to_op)]

                    }

                    if (length(how_to_val) > 1){

                        how_to_val <- how_to_val[2:length(how_to_op)]

                    }
                
                    if (ht == "divide"){

                        min_ <- round((min_ / nb), sensi)

                    }else if (ht == "add"){

                        min_ <- min_ + nb

                    }else if (ht == "multiply"){

                        min_ <- min * nb

                    }else{

                        min_ <- min_ - nb

                    }

                }

            }

            cnt <- 0

            for (lst in inpt_l){

                pre_v <- c()

                add_val <- lst[1]

                inpt_l[1] <- c()

                while (add_val <= lst[length(lst)]){

                    pre_v <- c(pre_v, add_val)

                    add_val <- round(add_val + min_, sensi2)

                }

                inpt_l <- append(x=inpt_l, values=list(pre_v))

                cnt <- cnt + 1

            }

            return(inpt_l)

        }

        rtn_l <- verify(diff_v2=diff_v2, min_=min_)

        return(rtn_l)

}

#' inter_max
#'
#' Takes as input a list of vectors composed of ints or floats ascendly ordered (intervals) that can have a different step to one of another element ex: list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3))
#' The function will return the list of lists altered according to the maximum step found in the input list.
#' @param inpt_l is the input list
#' @param max_ is a value you are sure is the minimum step value of all the sub-lists
#' @param get_lst is the parameter that, if set to True, will keep the last values of vectors in the return value if the last step exceeds the end value of the vector.
#' @examples
#'
#' print(inter_max(inpt_l=list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3)), get_lst=T))
#'  
#' [[1]]
#' [1] 0 4
#' 
#' [[2]]
#' [1] 0 4
#' 
#' [[3]]
#' [1] 1.0 2.3
#' 
#' print(inter_max(inpt_l=list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3)), get_lst=F))
#'
#'  [[1]]
#' [1] 0 4
#' 
#' [[2]]
#' [1] 0 4
#' 
#' [[3]]
#' [1] 1
#'
#' @export

inter_max <- function(inpt_l, max_=-1000, get_lst=T){

    for (lst in 1:length(inpt_l)){

            diff_v <- c()

            cur_v <- unlist(inpt_l[lst])

            for (el in 1:(length(cur_v) - 1)){

                diff_v <- c(diff_v, (cur_v[el + 1] - cur_v[el]))

            }

            if (max(diff_v) > max_){

                max_ <- max(diff_v)

            }

    }

    cnt <- 0

    for (lst in inpt_l){

        cur_lst <- unlist(lst)

        add_val <- cur_lst[1]

        pre_v <- c()

        inpt_l[1] <- c()

        while (add_val <= cur_lst[length(cur_lst)]){

            pre_v <- c(pre_v, add_val)

            add_val <- add_val + max_

        }

        if (get_lst & cur_lst[length(cur_lst)] != pre_v[length(pre_v)]){

            pre_v <- c(pre_v, cur_lst[length(cur_lst)])

        }

        inpt_l <- append(x=inpt_l, values <- list(pre_v), after=length(inpt_l))

        cnt <- cnt + 1

    }

    return(inpt_l)

}

#' incr_fillr
#' 
#' Take a vector uniquely composed by double and sorted ascendingly, a step, another vector of elements whose length is equal to the length of the first vector, and a default value. If an element of the vector is not equal to its predecessor minus a user defined step, so these can be the output according to the parameters (see example):
#' @param inpt_v is the asending double only composed vector
#' @param wrk_v is the other vector (size equal to inpt_v), defaults to NA
#' @param default_val is the default value put when the difference between two following elements of inpt_v is greater than step, defaults to NA
#' @param step is the allowed difference between two elements of inpt_v
#' @examples
#'
#' print(incr_fillr(inpt_v=c(1, 2, 4, 5, 9, 10), 
#'                 wrk_v=NA, 
#'                 default_val="increasing"))
#'
#' [1]  1  2  3  4  5  6  7  8  9 10
#'
#' print(incr_fillr(inpt_v=c(1, 1, 2, 4, 5, 9), 
#'                 wrk_v=c("ok", "ok", "ok", "ok", "ok"), 
#'                 default_val=NA))
#'
#' [1] "ok" "ok" "ok" NA   "ok" "ok" NA   NA   NA  
#'
#' print(incr_fillr(inpt_v=c(1, 2, 4, 5, 9, 10), 
#'                 wrk_v=NA, 
#'                 default_val="NAN"))
#'
#' [1] "1"   "2"   "NAN" "4"   "5"   "NAN" "NAN" "NAN" "9"   "10" 
#'
#' @export

incr_fillr <- function(inpt_v, wrk_v=NA, default_val=NA, step=1){

    if (all(is.na(wrk_v))){

        rtn_v <- inpt_v

    }else{

        rtn_v <- wrk_v

    }

    if (is.na(default_val)){

        i = 2

        while (i <= length(inpt_v)){

            if (is.na(inpt_v[(i-1)]) == F){

                if ((inpt_v[(i-1)] + step) < inpt_v[i]){

                    rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                    inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                    bf_val = inpt_v[(i-1)] + 1

                }

            }else if ((bf_val + step) < inpt_v[i]){

                    rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                    inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                    bf_val = bf_val + 1

            }

            i = i + 1

        }

    }else if (default_val != "increasing"){

        i = 2

        while (i <= length(inpt_v)){

            if (inpt_v[(i-1)] != default_val){

                if ((as.numeric(inpt_v[(i-1)]) + step) < as.numeric(inpt_v[i])){

                    rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                    inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                    bf_val = as.numeric(inpt_v[(i-1)]) + 1

                }

            }else if ((bf_val + step) < as.numeric(inpt_v[i])){

                    inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                    rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                    bf_val = bf_val + 1

            }

            i = i + 1

        }

    }else{

        i = 2

        while (i <= length(rtn_v)){

            if ((inpt_v[(i-1)] + step) < inpt_v[i]){

                rtn_v <- append(x=rtn_v, values=(inpt_v[(i-1)]+1), after=(i-1))

                inpt_v <- append(x=inpt_v, values=(inpt_v[(i-1)]+1), after=(i-1))

            }

            i = i + 1

        }

    }

    return(rtn_v)

}

#' paste_df
#' 
#' Return a vector composed of pasted elements from the input dataframe at the same index.
#' @param inpt_df is the input dataframe
#' @param sep is the separator between pasted elements, defaults to ""
#' @examples
#' 
#' print(paste_df(inpt_df=data.frame(c(1, 2, 1), c(33, 22, 55))))
#'
#' [1] "133" "222" "155"
#'
#' @export

paste_df <- function(inpt_df, sep=""){

    if (ncol(as.data.frame(inpt_df)) == 1){ 

        return(inpt_df) 

    }else {

        rtn_df <- inpt_df[,1]

        for (i in 2:ncol(inpt_df)){

            rtn_df <- paste(rtn_df, inpt_df[,i], sep=sep)

        }

        return(rtn_df)

    }

}

#' nest_v
#' 
#' Nest two vectors according to the following parameters.
#' @param f_v is the vector that will welcome the nested vector t_v
#' @param t_v is the imbriquator vector
#' @param step defines after how many elements of f_v the next element of t_v can be put in the output
#' @param after defines after how many elements of f_v, the begining of t_v can be put 
#' @examples
#' 
#' print(nest_v(f_v=c(1, 2, 3, 4, 5, 6), t_v=c("oui", "oui2", "oui3", "oui4", "oui5", "oui6"), step=2, after=2))
#'
#' [1] "1"    "2"    "oui"  "3"    "4"    "oui2" "5"    "6"    "oui3" "oui4"
#' 
#' @export

nest_v <- function(f_v, t_v, step=1, after=1){

    cnt = after

    for (i in 1:length(t_v)){

        f_v <- append(x=f_v, values=t_v[i], after=cnt)

        cnt = cnt + step + 1

    }

    return(f_v)

}

#' fixer_nest_v
#'
#' Retur the elements of a vector "wrk_v" (1) that corresponds to the pattern of elements in another vector "cur_v" (2) according to another vector "pttrn_v" (3) that contains the patterof eleemnts.
#' @examples
#'print(fixer_nest_v(cur_v=c("oui", "non", "peut-etre", "oui", "non", "peut-etre"), pttrn_v=c("oui", "non", "peut-etre"), 
#'                   wrk_v=c(1, 2, 3, 4, 5, 6)))
#'
#'[1] 1 2 3 4 5 6
#'
#'print(fixer_nest_v(cur_v=c("oui", "non", "peut-etre", "oui", "non", "peut-etre"), pttrn_v=c("oui", "non"), 
#'                   wrk_v=c(1, 2, 3, 4, 5, 6)))
#'
#'[1]  1  2 NA  4  5 NA
#' @export

fixer_nest_v <- function(cur_v, pttrn_v, wrk_v){

    cnt = 1

    cnt2 = 0

    for (i in 1:length(cur_v)){

        if (pttrn_v[cnt] != cur_v[i]){

            if (cnt2 == 0){

                idx <- (cnt2*length(pttrn_v)-1) + match(T, str_detect(pttrn_v, paste0("\\b(", cur_v[i], ")\\b"))) 

            }else{

                idx <- cnt2*length(pttrn_v) + match(T, str_detect(pttrn_v, paste0("\\b(", cur_v[i], ")\\b"))) 

            }

            rtain_val <- wrk_v[idx]

            wrk_v[idx] <- wrk_v[i] 

            wrk_v[i] <- rtain_val

            rtain_val <- cur_v[idx]

            cur_v[idx] <- cur_v[i]

            cur_v[i] <- rtain_val

            if (cnt == length(pttrn_v)){ cnt = 1; cnt2 = cnt2 + 1 }else if (cnt > 1) { cnt = cnt + 1 }

        }else{

            if (cnt == length(pttrn_v)){ cnt = 1; cnt2 = cnt2 + 1 }else { cnt = cnt + 1 }

        }

    }

    return(wrk_v)

}

#' lst_flatnr
#'
#' Flatten a list to a vector
#' 
#' @param lst_flatnr is the input list
#'
#' @examples
#'print(lst_flatnr(inpt_l=list(c(1, 2), c(5, 3), c(7, 2, 7))))
#'
#'[1] 1 2 5 3 7 2 7
#' @export

lst_flatnr <- function(inpt_l){

    rtn_v <- c()

    for (el in inpt_l){

        rtn_v <- c(rtn_v, el)

    }

    return(rtn_v)

}

#' extrt_only_v
#' 
#' return the elements from a vector "inpt_v" that are in another vector "pttrn_v"
#' @param inpt_v is the input vector 
#' @param pttrn_v is the vector contining all the elements that can be in inpt_v 
#' @examples
#'print(extrt_only_v(inpt_v=c("oui", "non", "peut", "oo", "ll", "oui", "non", "oui", "oui"), pttrn_v=c("oui")))
#'
#'[1] "oui" "oui" "oui" "oui"
#' @export

extrt_only_v <- function(inpt_v, pttrn_v){

    rtn_v <- c()

    for (el in inpt_v){

        if (el %in% pttrn_v){ rtn_v <- c(rtn_v, el) }

    }

    return(rtn_v)

}

#' new_ordered
#'
#' Returns the indexes of elements contained in "w_v" according to "f_v"
#' 
#' @param f_v is the input vector
#' @param w_v is the vector containing the elements that can be in f_v
#' @param nvr_here is a value you are sure is not present in f_v
#' @examples
#'
#' print(fittr_v(f_v=c("non", "non", "non", "oui"), w_v=c("oui", "non", "non")))
#'
#' [1] 4 1 2
#' 
#' @export

new_ordered <- function(f_v, w_v, nvr_here=NA){

    rtn_v <- c()

    for (el in w_v){

        idx <- match(el, f_v)

        rtn_v <- c(rtn_v, idx)

        f_v[idx] <- nvr_here

    }

    return(rtn_v)

}

#' appndr
#'
#' Append to a vector "inpt_v" a special value "val" n times "mmn". The appending begins at "strt" index.
#' @param inpt_v is the input vector
#' @param val is the special value
#' @param hmn is the number of special value element added
#' @param strt is the index from which appending begins, defaults to max which means the end of "inpt_v"
#' @examples
#'
#' print(appndr(inpt_v=c(1:3), val="oui", hmn=5))
#'
#' [1] "1"   "2"   "3"   "oui" "oui" "oui" "oui" "oui"
#'
#' print(appndr(inpt_v=c(1:3), val="oui", hmn=5, strt=1))
#'
#' [1] "1"   "oui" "oui" "oui" "oui" "oui" "2"   "3" 
#' 
#' @export

appndr <- function(inpt_v, val=NA, hmn, strt="max"){

    if (strt == "max"){

        strt <- length(inpt_v)

    }

    if (hmn > 0){

        for (i in 1:hmn){ inpt_v <- append(x=inpt_v, values=val, after=strt) }

    }

    return(inpt_v)

}

#' any_join_df
#'
#' Allow to perform SQL joints with more features
#' @param inpt_df_l is a list containing all the dataframe
#' @param join_type is the joint type. Defaults to inner but can be changed to a vector containing all the dataframes you want to take their ids to don external joints.
#' @param join_spe can be equal to a vector to do an external joints on all the dataframes. In this case, join_type should not be equal to "inner"
#' @param id_v is a vector containing all the ids name of the dataframes. The ids names can be changed to number of their columns taking in count their position in inpt_df_l. It means that if my id is in the third column of the second dataframe and the first dataframe have 5 columns, the column number of the ids is 5 + 3 = 8
#' @param excl_col is a vector containing the column names to exclude, if this vector is filled so "rtn_col" should not be filled. You can also put the column number in the manner indicated for "id_v". Defaults to c()
#' @param rtn_col is a vector containing the column names to retain, if this vector is filled so "excl_col" should not be filled. You can also put the column number in the manner indicated for "id_v". Defaults to c()
#' @param d_val is the default val when here is no match 
#' @examples
#'
#'df1 <- data.frame("val"=c(1, 1, 2, 4), "ids"=c("e", "a", "z", "a"), 
#'"last"=c("oui", "oui", "non", "oui"),
#'"second_ids"=c(13, 11, 12, 8))
#'
#'df2 <- data.frame("val"=c(3, 7, 2, 4, 1, 2), "ids"=c("a", "z", "z", "a", "a", "a"), 
#'"bool"=c(T, F, F, F, T, T),
#'"second_ids"=c(13, 12, 8, 34, 22, 12))
#'
#'df3 <- data.frame("val"=c(1, 9, 2, 4), "ids"=c("a", "a", "z", "a"), 
#'"last"=c("oui", "oui", "non", "oui"),
#'"second_ids"=c(13, 11, 12, 8))
#'
#'print(any_join_df(inpt_df_l=list(df1, df2, df3), join_type="inner", 
#'id_v=c("ids", "second_ids"), 
#'                  excl_col=c(), rtn_col=c()))
#'  ids val ids last second_ids val ids  bool second_ids val ids last second_ids
#'3 z12   2   z  non         12   7   z FALSE         12   2   z  non         12
#'
#'print(any_join_df(inpt_df_l=list(df1, df2, df3), join_type="inner", id_v=c("ids"),
#'excl_col=c(), rtn_col=c()))
#'
#'  ids val ids last second_ids val ids  bool second_ids val ids last second_ids
#'2   a   1   a  oui         11   3   a  TRUE         13   1   a  oui         13
#'3   z   2   z  non         12   7   z FALSE         12   2   z  non         12
#'4   a   4   a  oui          8   4   a FALSE         34   9   a  oui         11
#'
#'print(any_join_df(inpt_df_l=list(df1, df2, df3), join_type=c(1), id_v=c("ids"), 
#'                  excl_col=c(), rtn_col=c()))
#'
#'  ids val ids last second_ids  val  ids  bool second_ids  val  ids last
#'1   e   1   e  oui         13 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>
#'2   a   1   a  oui         11    3    a  TRUE         13    1    a  oui
#'3   z   2   z  non         12    7    z FALSE         12    2    z  non
#'4   a   4   a  oui          8    4    a FALSE         34    9    a  oui
#'  second_ids
#'1       <NA>
#'2         13
#'3         12
#'4         11
#'
#'print(any_join_df(inpt_df_l=list(df2, df1, df3), join_type=c(1, 3), id_v=c("ids", "second_ids"), 
#'                  excl_col=c(), rtn_col=c()))
#'   ids  val  ids  bool second_ids  val  ids last second_ids  val  ids last
#'1  a13    3    a  TRUE         13 <NA> <NA> <NA>       <NA>    1    a  oui
#'2  z12    7    z FALSE         12    2    z  non         12    2    z  non
#'3   z8    2    z FALSE          8 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'4  a34    4    a FALSE         34 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'5  a22    1    a  TRUE         22 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'6  a12    2    a  TRUE         12 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'7  a13 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'8  a11 <NA> <NA>  <NA>       <NA>    1    a  oui         11    9    a  oui
#'9  z12 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'10  a8 <NA> <NA>  <NA>       <NA>    4    a  oui          8    4    a  oui
#'   second_ids
#'1          13
#'2          12
#'3        <NA>
#'4        <NA>
#'5        <NA>
#'6        <NA>
#'7        <NA>
#'8          11
#'9        <NA>
#'10          8
#'
#'print(any_join_df(inpt_df_l=list(df1, df2, df3), join_type=c(1), id_v=c("ids"), 
#'                  excl_col=c(), rtn_col=c()))
#'
#'ids val ids last second_ids  val  ids  bool second_ids  val  ids last
#'1   e   1   e  oui         13 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>
#'2   a   1   a  oui         11    3    a  TRUE         13    1    a  oui
#'3   z   2   z  non         12    7    z FALSE         12    2    z  non
#'4   a   4   a  oui          8    4    a FALSE         34    9    a  oui
#'  second_ids
#'1       <NA>
#'2         13
#'3         12
#'4         11
#' @export

any_join_df <- function(inpt_df_l, join_type="inner", join_spe=NA, id_v=c(),  
                    excl_col=c(), rtn_col=c(), d_val=NA){

    incr_fillr <- function(inpt_v, wrk_v=NA, default_val=NA, step=1){

            if (all(is.na(wrk_v))){

                rtn_v <- inpt_v

            }else{

                rtn_v <- wrk_v

            }

            if (is.na(default_val)){

                i = 2

                while (i <= length(inpt_v)){

                    if (is.na(inpt_v[(i-1)]) == F){

                        if ((inpt_v[(i-1)] + step) < inpt_v[i]){

                            rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                            inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                            bf_val = inpt_v[(i-1)] + 1

                        }

                    }else if ((bf_val + step) < inpt_v[i]){

                            rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                            inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                            bf_val = bf_val + 1

                    }

                    i = i + 1

                }

            }else if (default_val != "increasing"){

                i = 2

                while (i <= length(inpt_v)){

                    if (inpt_v[(i-1)] != default_val){

                        if ((as.numeric(inpt_v[(i-1)]) + step) < as.numeric(inpt_v[i])){

                            rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                            inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                            bf_val = as.numeric(inpt_v[(i-1)]) + 1

                        }

                    }else if ((bf_val + step) < as.numeric(inpt_v[i])){

                            inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                            rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                            bf_val = bf_val + 1

                    }

                    i = i + 1

                }

            }else{

                i = 2

                while (i <= length(rtn_v)){

                    if ((inpt_v[(i-1)] + step) < inpt_v[i]){

                        rtn_v <- append(x=rtn_v, values=(inpt_v[(i-1)]+1), after=(i-1))

                        inpt_v <- append(x=inpt_v, values=(inpt_v[(i-1)]+1), after=(i-1))

                    }

                    i = i + 1

                }

            }

            return(rtn_v)

    }

    fixer_nest_v <- function(cur_v, pttrn_v, wrk_v){

            cnt = 1

            cnt2 = 0

            for (i in 1:length(cur_v)){

                if (pttrn_v[cnt] != cur_v[i]){

                    if (cnt2 == 0){

                        idx <- (cnt2*length(pttrn_v)-1) + match(T, str_detect(pttrn_v, paste0("\\b(", cur_v[i], ")\\b"))) 

                    }else{

                        idx <- cnt2*length(pttrn_v) + match(T, str_detect(pttrn_v, paste0("\\b(", cur_v[i], ")\\b"))) 

                    }

                    rtain_val <- wrk_v[idx]

                    wrk_v[idx] <- wrk_v[i] 

                    wrk_v[i] <- rtain_val

                    rtain_val <- cur_v[idx]

                    cur_v[idx] <- cur_v[i]

                    cur_v[i] <- rtain_val

                    if (cnt == length(pttrn_v)){ cnt = 1; cnt2 = cnt2 + 1 }else if (cnt > 1) { cnt = cnt + 1 }

                }else{

                    if (cnt == length(pttrn_v)){ cnt = 1; cnt2 = cnt2 + 1 }else { cnt = cnt + 1 }

                }

            }

            return(wrk_v)

        }

    appndr <- function(inpt_v, val=NA, hmn, strt="max"){

        if (strt == "max"){

            strt <- length(inpt_v)

        }

        if (hmn > 0){

            for (i in hmn){ inpt_v <- append(x=inpt_v, values=val, after=strt) }

        }

        return(inpt_v)

    }

    calc_occu_v <- function(f_v, w_v, nvr_here=NA){

            rtn_v <- c()

            idx_status <- c()

            f_v2 <- f_v

            for (el in 1:length(w_v)){

                cur_ids <- match(w_v[el], f_v)

                f_v[cur_ids] <- nvr_here

                idx_status <- c(idx_status, cur_ids)

            }

            for (i in sort(idx_status)){

                idx <- match(f_v2[i], w_v)

                rtn_v <- c(rtn_v, idx)

                w_v[idx] <- nvr_here

            }

            return(rtn_v)

    }

    extrt_only_v <- function(inpt_v, pttrn_v){

            rtn_v <- c()

            for (el in inpt_v){

                if (el %in% pttrn_v){ rtn_v <- c(rtn_v, el) }

            }

            return(rtn_v)

    }

    nest_v <- function(f_v, t_v, step=1, after=1){

        cnt = after

        for (i in 1:length(t_v)){

            f_v <- append(x=f_v, values=t_v[i], after=cnt)

            cnt = cnt + step + 1

        }

        return(f_v)

    }

    paste_df <- function(inpt_df, sep=""){

        if (ncol(as.data.frame(inpt_df)) == 1){ 

            return(inpt_df) 

        }else {

            rtn_df <- inpt_df[,1]

            for (i in 2:ncol(inpt_df)){

                rtn_df <- paste(rtn_df, inpt_df[,i], sep=sep)

            }

            return(rtn_df)

        }

    }

    n_row <- 1

    col_intel <- c()

    for (df_ in inpt_df_l){ 

        if (nrow(df_) > n_row){ n_row <- nrow(df_) }

        col_intel <- c(col_intel, (sum(col_intel) + ncol(df_)))

    }

    cl_nms <- colnames(as.data.frame(inpt_df_l[1]))

    if (length(inpt_df_l) > 1){

            for (i in 2:length(inpt_df_l)){

                cl_nms <- c(cl_nms, colnames(as.data.frame(inpt_df_l[i])))

            }

    }

    if (length(excl_col) > 0 & length(rtn_col) == 0){

            pre_col <- c(1:sum(mapply(function(x) return(ncol(x)), inpt_df_l)))

            if (typeof(excl_col) == "character"){

                excl_col2 <- c() 

                for (el in excl_col){ excl_col2 <- c(excl_col2, match(el, cl_nms)) }

                pre_col <- pre_col[-excl_col2]

            }else{

                pre_col <- pre_col[-excl_col]

            }

    }else if ((length(excl_col) + length(rtn_col)) == 0){

        pre_col <- c(1:sum(mapply(function(x) return(ncol(x)), inpt_df_l)))

    }else{

        if (typeof(rtn_col) == "character"){

            pre_col <- c()

            for (el in rtn_col){ pre_col <- c(pre_col, match(el, cl_nms)) }

        }else{

            pre_col <- rtn_col

        }

    }

    if (typeof(id_v) == "character"){

        id_v2 <- which(cl_nms == id_v[1])

        if (length(id_v) > 1){

            for (i in 2:length(id_v)){ id_v2 <- nest_v(f_v=id_v2, t_v=which(cl_nms == id_v[i]), after=(i-1)) }

        }

        id_v2 <- fixer_nest_v(cur_v=extrt_only_v(inpt_v=cl_nms, pttrn_v=id_v), pttrn_v=id_v, wrk_v=id_v2)

    }

    col_intel_cnt = 1 

    id_v_cnt = 1

    pre_col <- sort(pre_col)

    substrct <- 0

    ids_val_func <- function(x){

            lst_el <- length(which(lst_ids == x))

            if (length(which(cur_ids == x)) > lst_el){ 

                    return(which(cur_ids == x)[1:lst_el]) 

            }else {

                    return(which(cur_ids == x)[1:length(which(cur_ids == x))])

            }

    }

    if (all(join_type == "inner") & all(is.na(join_spe)) == T){

        cur_df <- as.data.frame(inpt_df_l[1])

        cur_id_v <- id_v2[1:length(id_v)]

        rtn_df <- cur_df[, cur_id_v]

        cur_ids <- paste_df(cur_df[, cur_id_v])

        rtn_df <- data.frame(cur_ids)

        cur_ids_val <- c(1:nrow(cur_df))

        calc_ids <- c(1:nrow(rtn_df))

        for (cur_col in pre_col){

            while (col_intel[col_intel_cnt] < cur_col){

                lst_ids <- cur_ids[cur_ids_val]

                id_v_cnt = id_v_cnt + length(id_v)

                col_intel_cnt = col_intel_cnt + 1

                cur_df <- as.data.frame(inpt_df_l[col_intel_cnt])

                cur_id_v <- id_v2[id_v_cnt:(id_v_cnt+length(id_v)-1)] 

                cur_ids <- paste_df(cur_df[, 
                    cur_id_v-(sum(mapply(function(x) return(ncol(x)), inpt_df_l[1:(col_intel_cnt-1)])))])

                cur_ids_val2 <- sort(lst_flatnr(mapply(function(x) return(which(lst_ids == x)), unique(cur_ids))))

                rtn_df <- rtn_df[cur_ids_val2, ]

                cur_ids_val <- sort(lst_flatnr(mapply(function(x) return(ids_val_func(x)), unique(lst_ids[cur_ids_val2]))))

                substrct <- sum(mapply(function(x) return(ncol(x)), inpt_df_l[1:(col_intel_cnt-1)]))

                calc_ids <- calc_occu_v(f_v=lst_ids, w_v=cur_ids[cur_ids_val])

                calc_ids <- calc_ids[is.na(calc_ids)==F]

            }

            pre_rtn_df <- cur_df[cur_ids_val, (cur_col - substrct)]

            pre_rtn_df <- pre_rtn_df[calc_ids]

            rtn_df <- cbind(rtn_df, pre_rtn_df)

            colnames(rtn_df)[length(colnames(rtn_df))] <- cl_nms[cur_col]

        }

        colnames(rtn_df)[1] <- "ids"

        return(rtn_df)

    }else{

        spe_match <- function(f_v, w_v, nvr_here=NA){

            rtn_v <- c()

            for (i in 1:length(w_v)){

                idx <- match(w_v[i], f_v)

                rtn_v <- c(rtn_v, idx)

                f_v[idx] <- nvr_here

            }

            return(rtn_v)

        }

        if (is.na(join_spe)){

                strt_id <- 1

                cur_df <- as.data.frame(inpt_df_l[join_type[1]])

                cur_id_v <- id_v2[strt_id:length(id_v)]

                cur_ids <- paste_df(cur_df[, cur_id_v])

                if (length(join_type) > 1){

                        join_type <- join_type[2:length(join_type)]

                        for (df in join_type){

                                    strt_id <- length(id_v) * (df-1) + 1

                                    cur_df <- as.data.frame(inpt_df_l[df])

                                    cur_id_v <- id_v2[strt_id:(strt_id+length(id_v)-1)]

                                    cur_ids <- c(cur_ids, paste_df(cur_df[, 
                                cur_id_v - sum(mapply(function(x) return(ncol(x)), inpt_df_l[1:(df-1)]))]))

                        }

                        cur_df <- as.data.frame(inpt_df_l[1])

                }

                lst_ids <- cur_ids

                cur_ids_val <- sort(lst_flatnr(mapply(function(x) return(which(lst_ids == x)), unique(cur_ids))))

        }else{

                lst_ids <- cur_ids

                cur_df <- as.data.frame(inpt_df_l[1])

                cur_id_v <- id_v2[1:length(id_v)]

                cur_ids <- paste_df(cur_df[, cur_id_v])

                cur_ids_val <- sort(lst_flatnr(mapply(function(x) return((which(lst_ids == x))), unique(cur_ids))))

        }

        rtn_df <- data.frame(cur_ids)

        cur_ids_val2 <- c(1:nrow(rtn_df)) 

        calc_ids <- c(1:length(lst_ids))

        for (cur_col in pre_col){

            while (col_intel[col_intel_cnt] < cur_col){

                col_intel_cnt = col_intel_cnt + 1

                substrct <- sum(mapply(function(x) return(ncol(x)), inpt_df_l[1:(col_intel_cnt-1)]))

                cur_df <- as.data.frame(inpt_df_l[col_intel_cnt])

                id_v_cnt = id_v_cnt + length(id_v)

                cur_id_v <- id_v2[id_v_cnt:(id_v_cnt+(length(id_v)-1))]

                cur_ids <- paste_df(cur_df[, (cur_id_v - substrct)])

                cur_ids_val2 <- lst_flatnr(mapply(function(x) return(ids_val_func(x)), unique(lst_ids)))

                cur_ids_val2 <- cur_ids_val2[is.na(cur_ids_val2)==F]

                cur_ids_val <- sort(spe_match(f_v=lst_ids, w_v=cur_ids[cur_ids_val2]))

                cur_ids_val <- c(0, cur_ids_val)

                calc_ids <- calc_occu_v(f_v=lst_ids, w_v=cur_ids[cur_ids_val2])

                calc_ids <- calc_ids[(is.na(calc_ids)==F)]

            }

            pre_rtn_df <- cur_df[cur_ids_val2, 
                (cur_col - substrct)]

            pre_rtn_df <- pre_rtn_df[calc_ids]

            pre_rtn_df <- incr_fillr(inpt_v=unique(c(cur_ids_val, length(lst_ids))), wrk_v=c("NA", pre_rtn_df), 
                                     default_val=d_val)

            pre_rtn_df <- appndr(inpt_v=pre_rtn_df, val=d_val, hmn=(length(lst_ids) - (length(pre_rtn_df) - 1)), strt="max")

            rtn_df <- cbind(rtn_df, pre_rtn_df[2:length(pre_rtn_df)])

            colnames(rtn_df)[length(colnames(rtn_df))] <- cl_nms[cur_col]

        }

        colnames(rtn_df)[1] <- "ids"

        return(rtn_df)
        
    }

}

#' equalizer_v
#'
#' Takes a vector of character as an input and returns a vector with the elements at the same size. The size can be chosen via depth parameter.
#'
#' @param inpt_v is the input vector containing all the characters
#' @param depth is the depth parameter, defaults to "max" which means that it is equal to the character number of the element(s) in inpt_v that has the most 
#' @param default_val is the default value that will be added to the output characters if those has an inferior length (characters) than the value of depth 
#' @examples 
#'  print(equalizer_v(inpt_v=c("aa", "zzz", "q"), depth=2))
#'  [1] "aa" "zz" "q?"
#'
#'  print(equalizer_v(inpt_v=c("aa", "zzz", "q"), depth=12))
#'  [1] "aa??????????" "zzz?????????" "q???????????"
#' @export

equalizer_v <- function(inpt_v, depth="max", default_val="?"){

        if (depth == "min"){ 

           depth <- nchar(inpt_v[1]) 

            if (length(inpt_v) > 1){

                    for (ptrn in inpt_v[2:length(inpt_v)]){

                        if (nchar(ptrn) < depth){ depth <- nchar(ptrn) }

                    }

            }

        }

        if (depth == "max"){ 

           depth <- nchar(inpt_v[1]) 

            if (length(inpt_v) > 1){

                    for (ptrn in inpt_v[2:length(inpt_v)]){

                        if (nchar(ptrn) > depth){ depth <- nchar(ptrn) }

                    }

            }

        }

        rtn_v <- c()

        for (ptrn in inpt_v){

                if (nchar(ptrn) < depth){ 

                        for (i in 1:(depth-nchar(ptrn))){ ptrn <- paste0(ptrn, default_val) }
                       
                        rtn_v <- c(rtn_v, ptrn)

                }else{

                        rtn_v <- c(rtn_v, paste(unlist(strsplit(x=ptrn, split=""))[1:depth], collapse=""))

                }

        }


        return(rtn_v)

}

#' rearangr_v
#'
#' Reanranges a vector "w_v" according to another vector "inpt_v". inpt_v contains a sequence of number. inpt_v and w_v have the same size and their indexes are related. The output will be a vector containing all the elements of w_v rearanges in descending or asending order according to inpt_v
#'
#' @param inpt_v is the vector that contains the sequance of number
#' @param w_v is the vector containing the elements related to inpt_v
#' @param how is the way the elements of w_v will be outputed according to if inpt_v will be sorted ascendigly or descendingly
#' @examples 
#' print(rearangr_v(inpt_v=c(23, 21, 56), w_v=c("oui", "peut", "non"), how="decreasing"))
#' [1] "non"  "oui"  "peut"
#' @export

rearangr_v <- function(inpt_v, w_v, how="increasing"){

    rtn_v <- c()

    pre_v <- inpt_v

    if (how == "increasing"){

        inpt_v <- sort(inpt_v)

    }else {

        inpt_v <- sort(inpt_v, decreasing=T)

    }

    for (el in inpt_v){

        idx <- match(el, pre_v)

        rtn_v <- c(rtn_v, w_v[idx])

        pre_v[idx] <- NA

    }

    return(rtn_v)

}

#' clusterizer_v
#' 
#' Allow to output clusters of elements. Takes as input a vector "inpt_v" containing a sequence of number. Can also take another vector "w_v" that has the same size of inpt_v because its elements are related to it. The way the clusters are made is related to an accuracy value which is "c_val". It means that if the difference between the values associated to 2 elements is superior to c_val, these two elements are in distinct clusters. The second element of the outputed list is the begin and end value of each cluster.
#' 
#' @param inpt_v is the vector containing the sequence of number
#' @param w_v is the vector containing the elements related to inpt_v, defaults to NA
#' @param c_val is the accuracy of the clusterization
#' 
#' @examples
#'  print(clusterizer_v(inpt_v=sample.int(20, 26, replace=T), w_v=NA, c_val=0.9))
#' 
#'  [[1]]
#' [[1]][[1]]
#' [1] 1
#' 
#' [[1]][[2]]
#' [1] 2
#' 
#' [[1]][[3]]
#' [1] 3
#' 
#' [[1]][[4]]
#' [1] 4
#' 
#' [[1]][[5]]
#' [1] 5 5
#' 
#' [[1]][[6]]
#' [1] 6 6 6 6
#' 
#' [[1]][[7]]
#' [1] 7 7 7
#' 
#' [[1]][[8]]
#' [1] 8 8 8
#' 
#' [[1]][[9]]
#' [1] 9
#' 
#' [[1]][[10]]
#' [1] 10
#' 
#' [[1]][[11]]
#' [1] 12
#' 
#' [[1]][[12]]
#' [1] 13 13 13
#' 
#' [[1]][[13]]
#' [1] 18 18 18
#' 
#' [[1]][[14]]
#' [1] 20
#' 
#' 
#' [[2]]
#'  [1] "1"  "1"  "-"  "2"  "2"  "-"  "3"  "3"  "-"  "4"  "4"  "-"  "5"  "5"  "-" 
#' [16] "6"  "6"  "-"  "7"  "7"  "-"  "8"  "8"  "-"  "9"  "9"  "-"  "10" "10" "-" 
#' [31] "12" "12" "-"  "13" "13" "-"  "18" "18" "-"  "20" "20"
#' 
#' print(clusterizer_v(inpt_v=sample.int(40, 26, replace=T), w_v=letters, c_val=0.29))
#'
#' [[1]]
#' [[1]][[1]]
#' [1] "a"
#' 
#' [[1]][[2]]
#' [1] "b"
#' 
#' [[1]][[3]]
#' [1] "c" "d"
#' 
#' [[1]][[4]]
#' [1] "e" "f"
#' 
#' [[1]][[5]]
#' [1] "g" "h" "i" "j"
#' 
#' [[1]][[6]]
#' [1] "k"
#' 
#' [[1]][[7]]
#' [1] "l"
#' 
#' [[1]][[8]]
#' [1] "m" "n"
#' 
#' [[1]][[9]]
#' [1] "o"
#' 
#' [[1]][[10]]
#' [1] "p"
#' 
#' [[1]][[11]]
#' [1] "q" "r"
#' 
#' [[1]][[12]]
#' [1] "s" "t" "u"
#' 
#' [[1]][[13]]
#' [1] "v"
#' 
#' [[1]][[14]]
#' [1] "w"
#' 
#' [[1]][[15]]
#' [1] "x"
#' 
#' [[1]][[16]]
#' [1] "y"
#' 
#' [[1]][[17]]
#' [1] "z"
#' 
#' 
#' [[2]]
#'  [1] "13" "13" "-"  "14" "14" "-"  "15" "15" "-"  "16" "16" "-"  "17" "17" "-" 
#' [16] "19" "19" "-"  "21" "21" "-"  "22" "22" "-"  "23" "23" "-"  "25" "25" "-" 
#' [31] "27" "27" "-"  "29" "29" "-"  "30" "30" "-"  "31" "31" "-"  "34" "34" "-" 
#' [46] "35" "35" "-"  "37" "37"
#' 
#'
#' @export

clusterizer_v <- function(inpt_v, w_v=NA, c_val){

    rearangr_v <- function(inpt_v, w_v, how="increasing"){

            rtn_v <- c()

            pre_v <- inpt_v

            if (how == "increasing"){

                inpt_v <- sort(inpt_v)

            }else {

                inpt_v <- sort(inpt_v, decreasing=T)

            }

            for (el in inpt_v){

                idx <- match(el, pre_v)

                rtn_v <- c(rtn_v, w_v[idx])

                pre_v[idx] <- NA

            }

            return(rtn_v)

    }

    inpt_v <- sort(inpt_v)

    idx_v <- c()

    rtn_l <- list() 

    if (all(is.na(w_v)) == F){

            w_v <- rearangr_v(inpt_v=inpt_v, w_v=w_v)

            pre_v <- c(w_v[1])

            pre_idx <- inpt_v[1]

            if (length(inpt_v) > 1){

                    for (i in 2:length(inpt_v)){

                        if ((inpt_v[i] - inpt_v[i - 1]) > c_val){

                                rtn_l <- append(rtn_l, list(pre_v))

                                idx_v <- c(idx_v, "-", pre_idx, inpt_v[i-1])

                                pre_idx <- inpt_v[i]

                                pre_v <- c()

                        }

                        pre_v <- c(pre_v, w_v[i])

                    }

                    rtn_l <- append(rtn_l, list(pre_v))

                    idx_v <- c(idx_v, "-", pre_idx, inpt_v[length(inpt_v)])

            }else{

                rtn_l <- append(rtn_l, pre_v[1])

            }

    }else{

            pre_v <- c(inpt_v[1])

            pre_idx <- inpt_v[1]

            if (length(inpt_v) > 1){

                    for (i in 2:length(inpt_v)){

                        if ((inpt_v[i] - inpt_v[i - 1]) > c_val){

                                rtn_l <- append(rtn_l, list(pre_v))

                                idx_v <- c(idx_v, "-", pre_idx, inpt_v[i-1])

                                pre_idx <- inpt_v[i]

                                pre_v <- c()

                        }
                                
                        pre_v <- c(pre_v, inpt_v[i])

                    }

                    rtn_l <- append(rtn_l, list(pre_v))

                    idx_v <- c(idx_v, "-", pre_idx, inpt_v[length(inpt_v)])

            }else{

                rtn_l <- append(rtn_l, pre_v[1])

            }

    }

    return(list(rtn_l, idx_v[2:length(idx_v)]))

}

#' closer_ptrn_adv
#' 
#' Allow to find how patterns are far or near between each other relatively to a vector containing characters at each index ("base_v"). The function gets the sum of the indexes of each pattern letter relatively to the characters in base_v. So each pattern can be compared.
#' 
#' @param inpt_v is the input vector containing all the patterns to be analyzed
#' @param res is a parameter controling the result. If set to "raw_stat", each word in inpt_v will come with its score (indexes of its letters relatively to base_v). If set to something else, so "c_word" parameter must be filled.
#' @param c_word is a pattern from which the nearest to the farest pattern in inpt_v will be compared 
#' @param base_v is the vector from which all pattern get its result (letters indexes for each pattern relatively to base_v), defaults to c("default_val", letters). "default_val" is another parameter and letters is all the western alphabetic letters in a vector
#' @param default_val is the value that will be added to all patterns that do not equal the length of the longest pattern in inpt_v. Those get this value added to make all patterns equal in length so they can be compared, defaults to "?"
#' 
#' @examples
#' print(closer_ptrn_adv(inpt_v=c("aurevoir", "bonnour", "nonnour", "fin", "mois", "bonjour"), res="word", c_word="bonjour"))
#' 
#'[[1]]
#'[1]  1  5 15 17 38 65
#'
#'[[2]]
#'[1] "bonjour"  "bonnour"  "aurevoir" "nonnour"  "mois"     "fin"     
#' 
#' print(closer_ptrn_adv(inpt_v=c("aurevoir", "bonnour", "nonnour", "fin", "mois")))
#' 
#'[[1]]
#'[1] 117 107 119  37  64
#'
#'[[2]]
#'[1] "aurevoir" "bonnour"  "nonnour"  "fin"      "mois"    
#' @export

closer_ptrn_adv <- function(inpt_v, res="raw_stat", default_val="?", base_v=c(default_val, letters), c_word=NA){

        chr_removr <- function(inpt_v, ptrn_v){

                rm_fun <- function(x){

                    rm_ids <- c()

                    cur_chr <- unlist(strsplit(x, split=""))

                    for (ptrn in ptrn_v){

                            rm_ids <- c(rm_ids, which(cur_chr == ptrn))

                    }

                    if (length(rm_ids) == 0){

                            return(x)

                    }else {

                            cur_chr <- cur_chr[-rm_ids]

                            return(paste(cur_chr, collapse=""))

                    }

                }
              
                rtn_v <- mapply(function(x) return(rm_fun(x)), inpt_v) 

                return(as.vector(rtn_v))

        }

        rearangr_v <- function(inpt_v, w_v, how="increasing"){

            rtn_v <- c()

            pre_v <- inpt_v

            if (how == "increasing"){

                inpt_v <- sort(inpt_v)

            }else {

                inpt_v <- sort(inpt_v, decreasing=T)

            }

            for (el in inpt_v){

                idx <- match(el, pre_v)

                rtn_v <- c(rtn_v, w_v[idx])

                pre_v[idx] <- NA

            }

            return(rtn_v)

        }

        equalizer_v <- function(inpt_v, depth="max", default_val="?"){

                if (depth == "min"){ 

                   depth <- nchar(inpt_v[1]) 

                    if (length(inpt_v) > 1){

                            for (ptrn in inpt_v[2:length(inpt_v)]){

                                if (nchar(ptrn) < depth){ depth <- nchar(ptrn) }

                            }

                    }

                }

                if (depth == "max"){ 

                   depth <- nchar(inpt_v[1]) 

                    if (length(inpt_v) > 1){

                            for (ptrn in inpt_v[2:length(inpt_v)]){

                                if (nchar(ptrn) > depth){ depth <- nchar(ptrn) }

                            }

                    }

                }

                rtn_v <- c()

                for (ptrn in inpt_v){

                        if (nchar(ptrn) < depth){ 

                                for (i in 1:(depth-nchar(ptrn))){ ptrn <- paste0(ptrn, default_val) }
                               
                                rtn_v <- c(rtn_v, ptrn)

                        }else{

                                rtn_v <- c(rtn_v, paste(unlist(strsplit(x=ptrn, split=""))[1:depth], collapse=""))

                        }

                }


                return(rtn_v)

    }

    inpt_v <- equalizer_v(inpt_v=inpt_v, default_val=default_val)

    ref_v <- base_v

    res_v <- c()

    for (ptrn in inpt_v){

        cur_delta = 0

        ptrn <- unlist(strsplit(ptrn, split=""))

        for (ltr in ptrn){

            cur_delta = cur_delta + match(ltr, base_v)

        }

        res_v <- c(res_v, cur_delta)

    }

    if (res == "raw_stat"){

         return(list(res_v, chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val))))

    }else if (is.na(c_word) == F){

        cur_delta = 0

        for (ltr in unlist(strsplit(c_word, split=""))){

            cur_delta = cur_delta + match(ltr, base_v)

        }

        cur_delta <- abs(res_v - cur_delta)

        inpt_v <- rearangr_v(inpt_v=cur_delta, w_v=inpt_v, how="increasing")

        return(list(sort(cur_delta, decreasing=F), chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val))))

    }

}

#' unique_ltr_from_v 
#'
#' Returns the unique characters contained in all the elements from an input vector "inpt_v"
#'
#' @param inpt_v is the input vector containing all the elements
#' @param keep_v is the vector containing all the characters that the elements in inpt_v may contain
#'
#' @examples
#' print(unique_ltr_from_v(inpt_v=c("bonjour", "lpoerc", "nonnour", "bonnour", "nonjour", "aurevoir")))
#'  [1] "b" "o" "n" "j" "u" "r" "l" "p" "e" "c" "a" "v" "i" 
#' @export

unique_ltr_from_v <- function(inpt_v, keep_v=c("?", "!", ":", "&", ",", ".", letters)){

    cnt = 1

    add_v <- c(1)

    rtn_v <- c()

    while (length(keep_v) > 0 & cnt <= length(inpt_v)){

            add_v <- as.vector(mapply(function(x) return(match(x, keep_v)), unlist(strsplit(inpt_v[cnt], split=""))))

            if (all(is.na(add_v)) == F){

                add_v <- add_v[(is.na(add_v)==F)]

                rtn_v <- c(rtn_v, keep_v[unique(add_v)])

                keep_v <- keep_v[-add_v]

            }

            cnt = cnt + 1

    }

    return(rtn_v)

}

#' closer_ptrn
#'
#' Take a vector of patterns as input and output each chosen word with their closest patterns from chosen patterns. 
#' 
#' @param inpt_v is the input vector containing all the patterns
#' @param excl_v is the vector containing all the patterns from inpt_v to exclude for comparing them to others patterns. If this parameter is filled, so "rtn_v" must be empty.
#' @param rtn_v is the vector containing all the patterns from inpt_v to keep for comparing them to others patterns. If this parameter is filled, so "rtn_v" must be empty.
#' @param sub_excl_v is the vector containing all the patterns from inpt_v to exclude for using them to compare to another pattern. If this parameter is filled, so "sub_rtn_v" must be empty.
#' @param sub_rtn_v is the vector containing all the patterns from inpt_v to retain for using them to compare to another pattern. If this parameter is filled, so "sub_excl_v" must be empty.
#' @param base_v must contain all the characters that the patterns are succeptible to contain, defaults to c("?", letters). "?" is necessary because it is internaly the default value added to each element that does not have a suffiient length compared to the longest pattern in inpt_v. If set to NA, the function will find by itself the elements to be filled with but it may takes an extra time 
#' @examples
#' 
#' print(closer_ptrn(inpt_v=c("bonjour", "lpoerc", "nonnour", "bonnour", "nonjour", "aurevoir")))
#'
#'[[1]]
#'[1] "bonjour"
#'
#'[[2]]
#'[1] "lpoerc"   "nonnour"  "bonnour"  "nonjour"  "aurevoir"
#'
#'[[3]]
#'[1] 1 1 2 7 8
#'
#'[[4]]
#'[1] "lpoerc"
#'
#'[[5]]
#'[1] "bonjour"  "nonnour"  "bonnour"  "nonjour"  "aurevoir"
#'
#'[[6]]
#'[1] 7 7 7 7 7
#'
#'[[7]]
#'[1] "nonnour"
#'
#'[[8]]
#'[1] "bonjour"  "lpoerc"   "bonnour"  "nonjour"  "aurevoir"
#'
#'[[9]]
#'[1] 1 1 2 7 8
#'
#'[[10]]
#'[1] "bonnour"
#'
#'[[11]]
#'[1] "bonjour"  "lpoerc"   "nonnour"  "nonjour"  "aurevoir"
#'
#'[[12]]
#'[1] 1 1 2 7 8
#'
#'[[13]]
#'[1] "nonjour"
#'
#'[[14]]
#'[1] "bonjour"  "lpoerc"   "nonnour"  "bonnour"  "aurevoir"
#'
#'[[15]]
#'[1] 1 1 2 7 8
#'
#'[[16]]
#'[1] "aurevoir"
#'
#'[[17]]
#'[1] "bonjour" "lpoerc"  "nonnour" "bonnour" "nonjour"
#'
#'[[18]]
#'[1] 7 8 8 8 8
#' print(closer_ptrn(inpt_v=c("bonjour", "lpoerc", "nonnour", "bonnour", "nonjour", "aurevoir"), excl_v=c("nonnour", "nonjour"),
#'                  sub_excl_v=c("nonnour")))
#'
#'[1] 3 5
#'[[1]]
#'[1] "bonjour"
#'
#'[[2]]
#'[1] "lpoerc"   "bonnour"  "nonjour"  "aurevoir"
#'
#'[[3]]
#'[1] 1 1 7 8
#'
#'[[4]]
#'[1] "lpoerc"
#'
#'[[5]]
#'[1] "bonjour"  "bonnour"  "nonjour"  "aurevoir"
#'
#'[[6]]
#'[1] 7 7 7 7
#'
#'[[7]]
#'[1] "bonnour"
#'
#'[[8]]
#'[1] "bonjour"  "lpoerc"   "bonnour"  "nonjour"  "aurevoir"
#'
#'[[9]]
#'[1] 0 1 2 7 8
#'
#'[[10]]
#'[1] "aurevoir"
#'
#'[[11]]
#'[1] "bonjour"  "lpoerc"   "nonjour"  "aurevoir"
#'
#'[[12]]
#'[1] 0 7 8 8
#' @export

closer_ptrn <- function(inpt_v, base_v=c("?", letters), excl_v=c(), rtn_v=c(), 
                        sub_excl_v=c(), sub_rtn_v=c()){

        unique_ltr_from_v <- function(inpt_v, keep_v=c("?", "!", ":", "&", ",", ".", letters)){

            cnt = 1

            add_v <- c(1)

            rtn_v <- c()

            while (length(keep_v) > 0 & cnt <= length(inpt_v)){

                    add_v <- as.vector(mapply(function(x) return(match(x, keep_v)), unlist(strsplit(inpt_v[cnt], split=""))))

                    if (all(is.na(add_v)) == F){

                        add_v <- add_v[(is.na(add_v)==F)]

                        rtn_v <- c(rtn_v, keep_v[unique(add_v)])

                        keep_v <- keep_v[-add_v]

                    }

                    cnt = cnt + 1

            }

            return(rtn_v)

        }

        default_val <- "?"

        if (all(is.na(base_v))){

            base_v <- unique_ltr_from_v(inpt_v=inpt_v)

        }

        if (("?" %in% base_v) == F) { base_v <- c(base_v, "?") }

        print(base_v)

        rearangr_v <- function(inpt_v, w_v, how="increasing"){

            rtn_v <- c()

            pre_v <- inpt_v

            if (how == "increasing"){

                inpt_v <- sort(inpt_v)

            }else {

                inpt_v <- sort(inpt_v, decreasing=T)

            }

            for (el in inpt_v){

                idx <- match(el, pre_v)

                rtn_v <- c(rtn_v, w_v[idx])

                pre_v[idx] <- NA

            }

            return(rtn_v)

        }

        chr_removr <- function(inpt_v, ptrn_v){

                rm_fun <- function(x){

                    rm_ids <- c()

                    cur_chr <- unlist(strsplit(x, split=""))

                    for (ptrn in ptrn_v){

                            rm_ids <- c(rm_ids, which(cur_chr == ptrn))

                    }

                    if (length(rm_ids) == 0){

                            return(x)

                    }else {

                            cur_chr <- cur_chr[-rm_ids]

                            return(paste(cur_chr, collapse=""))

                    }

                }
              
                rtn_v <- mapply(function(x) return(rm_fun(x)), inpt_v) 

                return(as.vector(rtn_v))

        }

        equalizer_v <- function(inpt_v, depth="max", default_val="?"){

                if (depth == "min"){ 

                   depth <- nchar(inpt_v[1]) 

                    if (length(inpt_v) > 1){

                            for (ptrn in inpt_v[2:length(inpt_v)]){

                                if (nchar(ptrn) < depth){ depth <- nchar(ptrn) }

                            }

                    }

                }

                if (depth == "max"){ 

                   depth <- nchar(inpt_v[1]) 

                    if (length(inpt_v) > 1){

                            for (ptrn in inpt_v[2:length(inpt_v)]){

                                if (nchar(ptrn) > depth){ depth <- nchar(ptrn) }

                            }

                    }

                }

                rtn_v <- c()

                for (ptrn in inpt_v){

                        if (nchar(ptrn) < depth){ 

                                for (i in 1:(depth-nchar(ptrn))){ ptrn <- paste0(ptrn, default_val) }
                               
                                rtn_v <- c(rtn_v, ptrn)

                        }else{

                                rtn_v <- c(rtn_v, paste(unlist(strsplit(x=ptrn, split=""))[1:depth], collapse=""))

                        }

                }


                return(rtn_v)

    }

    inpt_v <- equalizer_v(inpt_v=inpt_v, default_val=default_val)

    ref_v <- base_v

    res_l <- list()

    for (ptrn in inpt_v){

        cur_delta = c()

        ptrn <- unlist(strsplit(ptrn, split=""))

        for (ltr in ptrn){

            cur_delta = c(cur_delta, match(ltr, base_v))

        }

        res_l <- append(res_l, list(cur_delta))

    }

    print(res_l)

    rtn_l <- list()

    rmids <- c()

    sub_rmids <- c()

    if (length(excl_v) > 0){

        rmids <- as.vector(mapply(function(x) return(match(x, chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val)))), excl_v))     

    }

    if (length(rtn_v) > 0){

        rmids <- c(1:length(inpt_v))[-as.vector(mapply(function(x) return(match(x, chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val)))), rtn_v))]

    }

    if (length(sub_excl_v) > 0){

        sub_rmids <-  as.vector(mapply(function(x) return(match(x, chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val)))), sub_excl_v))     

    }

    if (length(sub_rtn_v) > 0){

        sub_rmids <- c(1:length(inpt_v))[-as.vector(mapply(function(x) return(match(x, c(inpt_v=inpt_v, ptrn_v=c(default_val)))), sub_rtn_v))]

    }

    if (length(rmids) > 0){

        inpt_v2 <- inpt_v[-rmids] 

        res_l2 <- res_l[-rmids]

    }else{

        inpt_v2 <- inpt_v

        res_l2 <- res_l

    }

    for (f_ptrn in 1:length(res_l2)){

            pre_l <- list(chr_removr(inpt_v=inpt_v2[f_ptrn], ptrn_v=default_val))

            pre_v <- c()

            f_ptrn_v <- unlist(res_l2[f_ptrn])

            for (cur_ptrn in res_l[-c(sub_rmids, (match(inpt_v[f_ptrn], inpt_v)))]){

                    diff_val = 0

                    for (pos in 1:length(cur_ptrn)){

                        if (cur_ptrn[pos] != f_ptrn_v[pos]){

                            diff_val = diff_val + 1 

                        }

                    }

                    pre_v <- c(pre_v, diff_val)

            }

            pre_ptrn <- chr_removr(inpt_v=inpt_v[-c(sub_rmids, 
                                        (match(inpt_v[f_ptrn], inpt_v)))], ptrn_v=c(default_val))

            pre_l <- append(x=pre_l, values=list(pre_ptrn))

            pre_l <- append(x=pre_l, values=list(sort(pre_v)))

            rtn_l <- append(x=rtn_l, values=pre_l)

    }
  
    return(rtn_l)

}

#' v_to_df
#'
#' Allow to convert a vector to a dataframe according to a separator.
#' 
#' @param inpt_v is the input vector
#' @param sep_ is the separator of the elements in inpt_v, defaults to ""
#' 
#' @examples
#' print(cut_v(inpt_v=c("oui", "non", "oui", "non")))
#' 
#'     X.o. X.u. X.i.
#' oui "o"  "u"  "i" 
#' non "n"  "o"  "n" 
#' oui "o"  "u"  "i" 
#' non "n"  "o"  "n" 
#' 
#' print(cut_v(inpt_v=c("ou-i", "n-on", "ou-i", "n-on"), sep_="-"))
#' 
#'      X.ou. X.i.
#' ou-i "ou"  "i" 
#' n-on "n"   "on"
#' ou-i "ou"  "i" 
#' n-on "n"   "on"
#' 
#' @export

cut_v <- function(inpt_v, sep_=""){

        rtn_df <- data.frame(matrix(data=NA, nrow=0, ncol=length(unlist(strsplit(inpt_v[1], split=sep_)))))

        for (el in inpt_v){ rtn_df <- rbind(rtn_df, unlist(strsplit(el, split=sep_))) }

        return(rtn_df)

}

#' wider_df
#'
#' Takes a dataframe as an input and the column to split according to a seprator.
#'
#' @param inpt_df is the input dataframe
#' @param col_to_splt is a vector containing the number or the colnames of the columns to split according to a separator
#' @param sep_ is the separator of the elements to split to new columns in the input dataframe 
#' @examples
#' df1 <- data.frame(c(1:5), c("o-y", "hj-yy", "er-y", "k-ll", "ooo-mm"), c(5:1))
#' 
#' df2 <- data.frame(c(1:5), c("o-y", "hj-yy", "er-y", "k-ll", "ooo-mm"))
#'  
#' print(wider_df(inpt_df=df1, col_to_splt=c(2), sep_="-"))
#'
#'        pre_df X.o.  X.y.  
#' o-y    1      "o"   "y"  5
#' hj-yy  2      "hj"  "yy" 4
#' er-y   3      "er"  "y"  3
#' k-ll   4      "k"   "ll" 2
#' ooo-mm 5      "ooo" "mm" 1
#'
#' print(wider_df(inpt_df=df2, col_to_splt=c(2), sep_="-"))
#' 
#'        pre_df X.o.  X.y.
#' o-y    1      "o"   "y" 
#' hj-yy  2      "hj"  "yy"
#' er-y   3      "er"  "y" 
#' k-ll   4      "k"   "ll"
#' ooo-mm 5      "ooo" "mm"
#' @export

wider_df <- function(inpt_df, col_to_splt=c(), sep_="-"){

        cut_v <- function(inpt_v, sep_=""){

                rtn_df <- data.frame(matrix(data=NA, nrow=0, ncol=length(unlist(strsplit(inpt_v[1], split=sep_)))))

                rtn_df <- t(mapply(function(x) return(rbind(rtn_df, unlist(strsplit(x, split=sep_)))), inpt_v))

                return(rtn_df)

        }

        if (typeof(col_to_splt) == "character"){

            for (i in 1:length(col_to_splt)){

               col_to_splt[i] <- match(col_to_splt[i], colnames(inpt_df))

            }

            col_to_splt <- as.numeric(v)

        }

        for (cl in col_to_splt){

            pre_df <- inpt_df[,1:(cl-1)]

            cur_df <- cut_v(inpt_v=inpt_df[, cl], sep_=sep_) 

            if (cl < ncol(inpt_df)){

                    w_df <- cbind(pre_df, cur_df, inpt_df[, ((cl+1):ncol(inpt_df))])

            }else{

                    w_df <- cbind(pre_df, cur_df)

            }

        }

    return(w_df)

}

#' colins_df
#'
#' Allow to insert vectors into a dataframe.
#' 
#' @param inpt_df is the dataframe where vectors will be inserted
#' @param target_col is a list containing all the vectors to be inserted
#' @param target_pos is a list containing the vectors made of the columns names or numbers where the associated vectors from target_col will be inserted after
#'
#' @examples
#'
#' df1 <- data.frame("frst_col"=c(1:5), "scd_col"=c(5:1))
#' 
#' print(colins_df(inpt_df=df1, target_col=list(c("oui", "oui", "oui", "non", "non"), c("u", "z", "z", "z", "u")), 
#'                 target_pos=list(c("frst_col", "scd_col"), c("scd_col"))))
#' 
#'   frst_col cur_col scd_col cur_col.1 cur_col
#' 1        1     oui       5       oui       u
#' 2        2     oui       4       oui       z
#' 3        3     oui       3       oui       z
#' 4        4     non       2       non       z
#' 5        5     non       1       non       u
#'
#' print(colins_df(inpt_df=df1, target_col=list(c("oui", "oui", "oui", "non", "non"), c("u", "z", "z", "z", "u")), 
#'                 target_pos=list(c(1, 2), c("frst_col"))))
#' 
#'   frst_col cur_col scd_col cur_col cur_col
#' 1        1     oui       5       u     oui
#' 2        2     oui       4       z     oui
#' 3        3     oui       3       z     oui
#' 4        4     non       2       z     non
#' 5        5     non       1       u     non
#'
#' @export

colins_df <- function(inpt_df, target_col=list(), target_pos=list()){

    cl_nms <- colnames(inpt_df)

    for (id_vec in 1:length(target_pos)){

            vec <- unlist(target_pos[id_vec])

            if (typeof(vec) == "character"){

                    pre_v <- c()

                    for (el in vec){

                        pre_v <- c(pre_v, match(el, cl_nms))

                    }

                    target_pos <- append(x=target_pos, values=list(pre_v), after=id_vec)

                    target_pos <- target_pos[-id_vec]

            }

    }

    for (cl in 1:length(target_col)){

        cur_col <- unlist(target_col[cl])

        cur_pos_v <- unlist(target_pos[cl])

        for (pos in 1:length(cur_pos_v)){

            idx <- cur_pos_v[pos]

            if (idx == 0){

                inpt_df <- cbind(cur_col, inpt_df[(idx+1):ncol(inpt_df)])

            }else if (idx < ncol(inpt_df)){

                inpt_df <- cbind(inpt_df[1:idx], cur_col, inpt_df[(idx+1):ncol(inpt_df)])

            }else{

                inpt_df <- cbind(inpt_df[1:idx], cur_col)

            }

            if (pos < length(cur_pos_v)){

                cur_pos_v[(pos+1):length(cur_pos_v)] = cur_pos_v[(pos+1):length(cur_pos_v)] + 1 
         
            }

            if (cl < length(target_pos)){

                    for (i in (cl+1):length(target_pos)){

                        target_pos <- append(x=target_pos, values=(unlist(target_pos[i])+1), after=i)

                        target_pos <- target_pos[-i]
                    
                    } 

            }

        }

    }

  return(inpt_df)

}

#' id_keepr_df
#'
#' Allow to get the original indexes after multiple equality comparaison according to the original number of row
#'
#' @param inpt_df is the input dataframe
#' @param col_v is the vector containing the column numbers or names to be compared to their respective elements in "el_v"
#' @param el_v is a vector containing the elements that may be contained in their respective column described in "col_v" 
#' @param rstr_l is a list containing the vector composed of the indexes of the elements chosen for each comparison. If the length of the list is inferior to the lenght of comparisons, so the last vector of rstr_l will be the same as the last one to fill make rstr_l equal in term of length to col_v and el_v
#' @examples
#' 
#' df1 <- data.frame(c("oui", "oui", "oui", "non", "oui"), c("opui", "op", "op", "zez", "zez"), c(5:1), c(1:5))
#' 
#' print(id_keepr(inpt_df=df1, col_v=c(1, 2), el_v=c("oui", "op")))
#'
#' [1] 2 3
#' 
#' print(id_keepr(inpt_df=df1, col_v=c(1, 2), el_v=c("oui", "op"), rstr_l=list(c(1:5), c(3, 2, 2, 2, 3))))
#'
#' [1] 2 3
#'
#' print(id_keepr(inpt_df=df1, col_v=c(1, 2), el_v=c("oui", "op"), rstr_l=list(c(1:5), c(3))))
#'
#' [1] 3
#'
#' print(id_keepr(inpt_df=df1, col_v=c(1, 2), el_v=c("oui", "op"), rstr_l=list(c(1:5))))
#' 
#' [1] 2 3
#' 
#' @export

id_keepr <- function(inpt_df, col_v=c(), el_v=c(), rstr_l=NA){

    rtn_v <- c(1:nrow(inpt_df))

    if (typeof(col_v) == "character"){

        cl_nms <- colnames(inpt_df)

        for (i in 1:length(col_v)){

                col_v[i] <- match(col_v[i], cl_nms)

        }

        col_v <- as.numeric(col_v)

    }

    if (all(is.na(rstr_l))){

        for (i in 1:length(col_v)){

            rtn_v <- rtn_v[inpt_df[rtn_v, col_v[i]] == el_v[i]]  

        }

        return(rtn_v)

    }else if (length(rstr_l) < length(col_v)){

            lst_v <- unlist(rstr_l[length(rstr_l)])

            for (i in (length(rstr_l)+1):length(col_v)){

                rstr_l <- append(x=rstr_l, values=list(lst_v))

            }

    }

    pre_vec <- c()

    fun <- function() { return(c(pre_vec, F)) }

    for (i in 1:length(col_v)){

        pre_vec2 <- mapply(function(x) return(fun()), c(1:length(rtn_v)))

        interst <- intersect(unlist(rstr_l[i]), rtn_v)

        pre_vec2[interst] <- inpt_df[interst, col_v[i]] == el_v[i]

        rtn_v <- rtn_v[pre_vec2]  

    }

    return(rtn_v)

}

#' unique_df
#' 
#' Returns the input dataframe with the unique columns or rows.
#'
#' @param inpt_df is the input dataframe
#' @param col is a parameter that specifies if the dataframe returned should have unique columns or rows, defaults to F, so the dataframe returned by default has unique rows
#' @examples
#'
#' df1 <- data.frame(c(1, 2, 1, 3), c("a", "z", "a", "p"))
#' 
#' print(unique_df(inpt_df=df1))
#' 
#'    c.1..2..1..3. c..a....z....a....p..
#' 1             1                     a
#' 2             2                     z
#' 4             3                     p
#' 
#' df1 <- data.frame(c(1, 2, 1, 3), c("a", "z", "a", "p"), c(1, 2, 1, 3))
#' 
#' print(unique_df(inpt_df=df1, col=T))
#' 
#'   cur_v cur_v
#' 1     1     a
#' 2     2     z
#' 3     1     a
#' 4     3     p
#' 
#' @export

unique_df <- function(inpt_df, col=F){

        comp_l <- list()

        if (col){

                rtn_df <- data.frame(matrix(data=NA, nrow=nrow(inpt_df), ncol=0))

                for (col in 1:ncol(inpt_df)){

                        cur_v <- inpt_df[, col]

                        if ((list(cur_v) %in% comp_l) == F){ rtn_df <- cbind(rtn_df, cur_v) }

                        comp_l <- append(x=comp_l, values=list(cur_v))

                }

        }else{

                rtn_df <- data.frame(matrix(data=NA, nrow=0, ncol=ncol(inpt_df)))

                for (row in 1:nrow(inpt_df)){

                        cur_v <- inpt_df[row, ]

                        if ((list(cur_v) %in% comp_l) == F){ rtn_df <- rbind(rtn_df, cur_v) }

                        comp_l <- append(x=comp_l, values=list(cur_v))

                }

        }

    return(rtn_df)

}

#' non_unique
#'
#' Returns the element that are not unique from the input vector
#'
#' @param inpt_v  is the input vector containing the elements
#' @param occu is a parameter that specifies the occurence of the elements that must be returned, defaults to ">-1-" it means that the function will return all the elements that are present more than one time in inpt_v. The synthax is the following "comparaison_type-actual_value-". The comparaison type may be "==" or ">". Occu can also be a vector containing all the occurence that must have the elements to be returned.
#' @examples
#' print(non_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non")))
#'
#' [1] "oui" "non"
#'
#' print(non_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non"), occu="==-2-"))
#'
#' [1] "oui"
#'
#' print(non_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non"), occu=">-2-"))
#'
#' [1] "non"
#' 
#' print(non_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non"), occu=c(1, 3)))
#' 
#' [1] "non"   "peut"  "peut1"
#' @export

non_unique <- function(inpt_v, occu=">-1-"){

   rtn_v <- c()

   if (typeof(occu) == "character"){

           pre_vec <- str_locate(occu, "-(.*?)-")

           occu_v <- unlist(strsplit(occu, split=""))

           max_val <- as.numeric(occu_v[(pre_vec[1]+1):(pre_vec[length(pre_vec)]-1)])

           comp_ <- paste(occu_v[1:(pre_vec[1] - 1)], collapse="")

           if (comp_ == "=="){

                for (el in unique(inpt_v)){ if (sum(inpt_v == el) == max_val) { rtn_v <- c(rtn_v, el) } }

           }

           if (comp_ == ">"){

                for (el in unique(inpt_v)){ if (sum(inpt_v == el) > max_val) { rtn_v <- c(rtn_v, el) } }

           }

   }else{

          for (el in unique(inpt_v)){ if (sum(inpt_v == el) %in% occu) { rtn_v <- c(rtn_v, el) } }

   }

   return(rtn_v)

}

r_print <- function(inpt_v, sep_="and", begn="This is", end=", voila!"){

        rtn_val <- ""

       for (el in inpt_v){ rtn_val <- paste(rtn_val, el, sep_, sep=" ") } 

       return(paste(begn, rtn_val, end, sep=" "))

}

#' str_remove_untl
#'
#' Allow to remove pattern within elements from a vector precisely according to their occurence.
#'
#' @param inpt_v is the input vector
#' @param ptrn_rm_v is a vector containing the patterns to remove
#' @param untl is a list containing the occurence(s) of each pattern to remove in the elements.
#' @param nvr_following_ptrn is a sequel of characters that you are sure is not present in any of the elements in inpt_v
#'
#' @examples
#'
#' vec <- c("45/56-/98mm", "45/56-/98mm", "45/56-/98-mm//")
#' 
#' print(str_remove_untl(inpt_v=vec, ptrn_rm_v=c("-", "/"), untl=list(c("max"), c(1))))
#' 
#' [1] "4556/98mm"   "4556/98mm"   "4556/98mm//"
#' 
#' print(str_remove_untl(inpt_v=vec, ptrn_rm_v=c("-", "/"), untl=list(c("max"), c(1:2))))
#' 
#' [1] "455698mm"   "455698mm"   "455698mm//"
#'
#' print(str_remove_untl(inpt_v=vec[1], ptrn_rm_v=c("-", "/"), untl=c("max")))
#'
#' [1] "455698mm" "455698mm" "455698mm"
#' 
#' @export

str_remove_untl <- function(inpt_v, ptrn_rm_v=c(), untl=list(c(1)), nvr_following_ptrn="NA"){

   rtn_v <- c()

   if (length(untl) < length(ptrn_rm_v)){

           for (i in 1:(length(ptrn_rm_v) - (length(untl)))){

                   untl <- append(x=untl, values=list(unlist(untl[length(untl)])))

           }

   }

   for (el in inpt_v){

        pre_el <- el

        cur_el <- el

        for (ptrn in 1:length(ptrn_rm_v)) {

                cur_el <- str_remove(string=cur_el, pattern=ptrn_rm_v[ptrn])

                if (unlist(untl[ptrn])[1] == "max"){

                        while (cur_el != pre_el){

                                pre_el <- cur_el

                                cur_el <- str_remove(string=cur_el, pattern=ptrn_rm_v[ptrn])

                        }

                }else {

                        cur_untl <- unlist(untl[ptrn])

                        cnt = 1

                        pre_cnt <- 1

                        rm_ids <- c()

                        cur_el_vstr <- cur_el

                        cur_untl <- cur_untl - 1

                        cur_untl <- cur_untl[cur_untl > 0]

                        cnt2 = -1

                        while (cur_el_vstr != pre_el & cnt <= length(cur_untl)){

                                for (i in pre_cnt:cur_untl[cnt]){

                                        rm_id <- str_locate(string=cur_el_vstr, pattern=ptrn_rm_v[ptrn])

                                        pre_el <- cur_el_vstr
                                        
                                        cur_el_vstr <- str_remove(string=cur_el_vstr, pattern=ptrn_rm_v[ptrn])

                                        cnt2 = cnt2 + 1

                                }

                                rm_id <- rm_id + cnt2

                                if (all(is.na(rm_id)) == F){  rm_ids <- c(rm_ids, rm_id[1]:rm_id[2]) }

                                pre_cnt = cur_untl[cnt] + 1

                                cnt = cnt + 1

                        }

                        if (length(rm_ids) > 0){

                                cur_el <- unlist(strsplit(x=cur_el, split=""))[-rm_ids]

                                cur_el <- paste(cur_el, collapse="")

                        }

                }

        }

        rtn_v <- c(rtn_v, cur_el)

   }

   return(rtn_v)

}

#' regroupr
#'
#' Allow to sort data like "c(X1/Y1/Z1, X2/Y1/Z2, ...)" to what you want. For example it can be to "c(X1/Y1/21, X1/Y1/Z2, ...)"
#'
#' @param inpt_v is the input vector containing all the data you want to sort in a specific way. All the sub-elements should be separated by a unique separator such as "-" or "/"
#' @param sep_ is the unique separator separating the sub-elements in each elements of inpt_v
#' @param order is a vector describing the way the elements should be sorted. For example if you want this dataset  "c(X1/Y1/Z1, X2/Y1/Z2, ...)" to be sorted by the last element you should have order=c(3:1), for example, and it should returns something like this c(X1/Y1/Z1, X2/Y1/Z1, X1/Y2/Z1, ...) assuming you have only two values for X. 
#' @param l_order is a list containing the vectors of values you want to order first for each sub-elements
#' @examples 
#' vec <- multitud(l=list(c("a", "b"), c("1", "2"), c("A", "Z", "E"), c("Q", "F")), sep_="/")
#' 
#'
#' print(vec)
#' 
#'  [1] "a/1/A/Q" "b/1/A/Q" "a/2/A/Q" "b/2/A/Q" "a/1/Z/Q" "b/1/Z/Q" "a/2/Z/Q"
#'  [8] "b/2/Z/Q" "a/1/E/Q" "b/1/E/Q" "a/2/E/Q" "b/2/E/Q" "a/1/A/F" "b/1/A/F"
#' [15] "a/2/A/F" "b/2/A/F" "a/1/Z/F" "b/1/Z/F" "a/2/Z/F" "b/2/Z/F" "a/1/E/F"
#' [22] "b/1/E/F" "a/2/E/F" "b/2/E/F"
#'
#' print(regroupr(inpt_v=vec, sep_="/"))
#'
#'  [1] "a/1/1/1"   "a/1/2/2"   "a/1/3/3"   "a/1/4/4"   "a/1/5/5"   "a/1/6/6"  
#'  [7] "a/2/7/7"   "a/2/8/8"   "a/2/9/9"   "a/2/10/10" "a/2/11/11" "a/2/12/12"
#' [13] "b/1/13/13" "b/1/14/14" "b/1/15/15" "b/1/16/16" "b/1/17/17" "b/1/18/18"
#' [19] "b/2/19/19" "b/2/20/20" "b/2/21/21" "b/2/22/22" "b/2/23/23" "b/2/24/24"
#'
#'  vec <- vec[-2]
#'
#'  print(regroupr(inpt_v=vec, sep_="/"))
#'
#'  [1] "a/1/1/1"   "a/1/2/2"   "a/1/3/3"   "a/1/4/4"   "a/1/5/5"   "a/1/6/6"  
#'  [7] "a/2/7/7"   "a/2/8/8"   "a/2/9/9"   "a/2/10/10" "a/2/11/11" "a/2/12/12"
#' [13] "b/1/13/13" "b/1/14/14" "b/1/15/15" "b/1/16/16" "b/1/17/17" "b/2/18/18"
#' [19] "b/2/19/19" "b/2/20/20" "b/2/21/21" "b/2/22/22" "b/2/23/23"
#'
#' print(regroupr(inpt_v=vec, sep_="/", order=c(4:1)))
#'
#' [1] "1/1/A/Q"   "2/2/A/Q"   "3/3/A/Q"   "4/4/A/Q"   "5/5/Z/Q"   "6/6/Z/Q"  
#'  [7] "7/7/Z/Q"   "8/8/Z/Q"   "9/9/E/Q"   "10/10/E/Q" "11/11/E/Q" "12/12/E/Q"
#' [13] "13/13/A/F" "14/14/A/F" "15/15/A/F" "16/16/A/F" "17/17/Z/F" "18/18/Z/F"
#' [19] "19/19/Z/F" "20/20/Z/F" "21/21/E/F" "22/22/E/F" "23/23/E/F" "24/24/E/F"
#'
#' @export

regroupr <- function(inpt_v, sep_="-", order=c(1:length(unlist(strsplit(x=inpt_v[1], split=sep_)))), l_order=NA){

        id_keepr <- function(inpt_df, col_v=c(), el_v=c(), rstr_l=NA){

            rtn_v <- c(1:nrow(inpt_df))

            if (typeof(col_v) == "character"){

                cl_nms <- colnames(inpt_df)

                for (i in 1:length(col_v)){

                        col_v[i] <- match(col_v[i], cl_nms)

                }

                col_v <- as.numeric(col_v)

            }

            if (all(is.na(rstr_l))){

                for (i in 1:length(col_v)){

                    rtn_v <- rtn_v[inpt_df[rtn_v, col_v[i]] == el_v[i]]  

                }

                return(rtn_v)

            }else if (length(rstr_l) < length(col_v)){

                    lst_v <- unlist(rstr_l[length(rstr_l)])

                    for (i in (length(rstr_l)+1):length(col_v)){

                        rstr_l <- append(x=rstr_l, values=list(lst_v))

                    }

            }

            pre_vec <- c()

            fun <- function() { return(c(pre_vec, F)) }

            for (i in 1:length(col_v)){

                pre_vec2 <- mapply(function(x) return(fun()), c(1:length(rtn_v)))

                interst <- intersect(unlist(rstr_l[i]), rtn_v)

                pre_vec2[interst] <- (inpt_df[interst, col_v[i]] == el_v[i])

                rtn_v <- rtn_v[pre_vec2]  

            }

            return(rtn_v)

        }

        colins_df <- function(inpt_df, target_col=list(), target_pos=list()){

            cl_nms <- colnames(inpt_df)

            for (id_vec in 1:length(target_pos)){

                    vec <- unlist(target_pos[id_vec])

                    if (typeof(vec) == "character"){

                            pre_v <- c()

                            for (el in vec){

                                pre_v <- c(pre_v, match(el, cl_nms))

                            }

                            target_pos <- append(x=target_pos, values=list(pre_v), after=id_vec)

                            target_pos <- target_pos[-id_vec]

                    }

            }

            for (cl in 1:length(target_col)){

                cur_col <- unlist(target_col[cl])

                cur_pos_v <- unlist(target_pos[cl])

                for (pos in 1:length(cur_pos_v)){

                    idx <- cur_pos_v[pos]

                    if (idx == 0){

                        inpt_df <- cbind(cur_col, inpt_df[(idx+1):ncol(inpt_df)])

                    }else if (idx < ncol(inpt_df)){

                        inpt_df <- cbind(inpt_df[1:idx], cur_col, inpt_df[(idx+1):ncol(inpt_df)])

                    }else{

                        inpt_df <- cbind(inpt_df[1:idx], cur_col)

                    }

                    if (pos < length(cur_pos_v)){

                        cur_pos_v[(pos+1):length(cur_pos_v)] = cur_pos_v[(pos+1):length(cur_pos_v)] + 1 
                 
                    }

                    if (cl < length(target_pos)){

                            for (i in (cl+1):length(target_pos)){

                                target_pos <- append(x=target_pos, values=(unlist(target_pos[i])+1), after=i)

                                target_pos <- target_pos[-i]
                            
                            } 

                    }

                }

            }

          return(inpt_df)

    }
    
    cut_v <- function(inpt_v, sep_=""){

        rtn_df <- data.frame(matrix(data=NA, nrow=0, ncol=length(unlist(strsplit(inpt_v[1], split=sep_)))))

        for (el in inpt_v){ rtn_df <- rbind(rtn_df, unlist(strsplit(el, split=sep_))) }

        return(rtn_df)

    }

    paste_df <- function(inpt_df, sep=""){

            if (ncol(as.data.frame(inpt_df)) == 1){ 

                return(inpt_df) 

            }else {

                rtn_df <- inpt_df[,1]

                for (i in 2:ncol(inpt_df)){

                    rtn_df <- paste(rtn_df, inpt_df[,i], sep=sep)

                }

                return(rtn_df)

            }

  }

  w_df <- cut_v(inpt_v, sep_=sep_) 

  if (all(is.na(l_order))){

          l_order <- list()

          for (i in order){

                  l_order <- append(x=l_order, values=list(unique(w_df[,i])))

          }

  }

  cur_el <- w_df[, order[1]]  

  v_ids <- c(1:nrow(w_df))

  rec_ids = 0

  for (el in unlist(l_order[1])){

    cur_ids <- which(cur_el == el)

    v_ids[(rec_ids + 1):(rec_ids + length(cur_ids))] <- el 

    rec_ids = rec_ids + length(cur_ids)

  }

  w_df <- cbind(w_df[, order[1]], w_df)

  order <- order + 1

  cnt = 2

  for (I in order[2:length(order)]){

        cur_el <- w_df[, I]

        cur_v_ids <- c(1:nrow(w_df))

        pre_bind_v <- c(1:nrow(w_df))

        rec_ids <- c()

        rec_ids2 <- c()

        for (el in unique(v_ids)){ 

                cur_ids_stay <- which(w_df[, 1] == el) 

                rec_ids2 <- c(rec_ids, cur_ids_stay)

                for (el2 in unlist(l_order[cnt])){

                    cur_ids <- id_keepr(inpt_df=w_df, col_v=c(I), el_v=c(el2), rstr_l=list(list(cur_ids_stay))) 

                    if (length(cur_ids) > 0){

                        pre_bind_v[cur_ids] <- el2

                        cur_v_ids[(length(rec_ids)+1):(length(rec_ids)+length(cur_ids))] <- el2

                        rec_ids <- c(rec_ids, cur_ids)

                    }

                }

        }

        if (order[cnt] > order[(cnt-1)]){

                w_df[, 1] <- paste_df(inpt_df=data.frame(w_df[, 1], pre_bind_v), sep="")

                v_ids <- as.vector(mapply(function(x, y) return(paste(x, sep_, y, sep="")), v_ids, cur_v_ids))

        }else{

                w_df[, 1] <- paste_df(inpt_df=data.frame(pre_bind_v, w_df[, 1]), sep="")

                v_ids <- as.vector(mapply(function(x, y) return(paste(y, sep_, x, sep="")), v_ids, cur_v_ids))

        }

        cnt = cnt + 1

  }

  return(v_ids)

}

#' vec_in_df
#'
#' Allow to get if a vector is in a dataframe. Returns the row and column of the vector in the dataframe if the vector is contained in the dataframe.
#'
#' @param inpt_df is the input dataframe
#' @param inpt_vec is the vector that may be in the input dataframe
#' @param coeff is the "slope coefficient" of inpt_vec
#' @param conventinal is if a positive slope coefficient means that the vector goes upward or downward 
#' @param stop_untl is the maximum number of the input vector the function returns, if in the dataframe 
#' @examples
#'
#' df1 <- data.frame(c(1:5), c(5:1), c("a", "z", "z", "z", "a"))
#' 
#' print(df1)
#' 
#'   c.1.5. c.5.1. c..a....z....z....z....a..
#' 1      1      5                          a
#' 2      2      4                          z
#' 3      3      3                          z
#' 4      4      2                          z
#' 5      5      1                          a
#'
#' print(vec_in_df(inpt_df=df1, inpt_vec=c(5, 4, "z"), coeff=1))
#'
#' NULL
#' 
#' print(vec_in_df(inpt_df=df1, inpt_vec=c(5, 2, "z"), coeff=1))
#' 
#' [1] 5 1
#'
#' print(vec_in_df(inpt_df=df1, inpt_vec=c(3, "z"), coeff=1))
#'
#' [1] 3 2
#'
#' print(vec_in_df(inpt_df=df1, inpt_vec=c(4, "z"), coeff=-1))
#' 
#'[1] 2 2
#'
#' print(vec_in_df(inpt_df=df1, inpt_vec=c(2, 3, "z"), coeff=-1))
#' 
#' [1] 2 1
#' 
#' print(vec_in_df(inpt_df=df1, inpt_vec=c(5, 2, "z"), coeff=-1, conventional=T))
#'  
#' [1] 5 1
#'
#' df1[4, 2] <- 1
#' 
#' print(vec_in_df(inpt_df=df1, inpt_vec=c(1, "z"), coeff=-1, conventional=T, stop_untl=4))
#' 
#' [1] 4 2 5 2
#' 
#' @export

vec_in_df <- function(inpt_df, inpt_vec=c(), coeff=0, stop_untl=1, conventional=F){

    if (conventional){ coeff <- coeff * -1 }

    rtn_v <- c()

    encounter_cnt = 0

    if (coeff > -1){

            for (I in 1:(ncol(inpt_df) - length(inpt_vec) + 1)){

                    strt_id = 1 + (length(inpt_vec) * coeff)

                    for (i in strt_id:nrow(inpt_df)){

                        if (inpt_df[i, I] == inpt_vec[1]){

                                cur_row = i

                                cur_col = I 

                                col_cnt = 1

                                while (col_cnt < (length(inpt_vec) + 1) & inpt_df[cur_row, cur_col] == inpt_vec[col_cnt]){

                                    cur_row = cur_row - coeff

                                    if (!(col_cnt) == length(inpt_vec)){

                                        cur_col = cur_col + 1

                                    }

                                    col_cnt = col_cnt + 1

                                }

                                if (cur_col == ncol(inpt_df)){

                                        rtn_v <- c(rtn_v, i, I)

                                        encounter_cnt = encounter_cnt + 1

                                        if (encounter_cnt == stop_untl){

                                                return(rtn_v)

                                        }

                                }

                        }

                    }

            }

    }else{

            for (I in 1:(ncol(inpt_df) - length(inpt_vec) + 1)){

                    strt_id = nrow(inpt_df) - (length(inpt_vec) * abs(coeff))

                    for (i in 1:strt_id){

                        if (inpt_df[i, I] == inpt_vec[1]){

                                cur_row = i 

                                cur_col = I

                                col_cnt = 1

                                while (col_cnt < (length(inpt_vec) + 1) & inpt_df[cur_row, cur_col] == inpt_vec[col_cnt]){

                                    cur_row = cur_row + abs(coeff)

                                    if (!(col_cnt) == length(inpt_vec)){

                                        cur_col = cur_col + 1

                                    }

                                    col_cnt = col_cnt + 1


                                }

                                if (cur_col == ncol(inpt_df)){

                                        rtn_v <- c(rtn_v, i, I)

                                        encounter_cnt = encounter_cnt + 1

                                        if (encounter_cnt == stop_untl){

                                                return(rtn_v)

                                        }

                                }

                        }

                    }

            }

    }

    return(rtn_v)

}

#' date_converter_reverse
#'
#' Allow to convert single date value like 2025.36 year to a date like second/minutehour/day/month/year (snhdmy)
#' 
#' @param inpt_date is the input date 
#' @param convert_to is the date format the input date will be converted
#' @param frmt is the time unit of the input date
#' @param sep_ is the separator of the outputed date
#' @examples
#'
#' print(date_converter_reverse(inpt_date="2024.929", convert_to="hmy", frmt="y", sep_="-"))
#' 
#' [1] "110-11-2024"
#' 
#' print(date_converter_reverse(inpt_date="2024.929", convert_to="dmy", frmt="y", sep_="-"))
#'
#' [1] "4-11-2024"
#' 
#' print(date_converter_reverse(inpt_date="2024.929", convert_to="hdmy", frmt="y", sep_="-"))
#'
#' [1] "14-4-11-2024"
#' 
#' print(date_converter_reverse(inpt_date="2024.929", convert_to="dhym", frmt="y", sep_="-"))
#' 
#' [1] "4-14-2024-11"
#'
#' @export

date_converter_reverse <- function(inpt_date, convert_to="dmy", frmt="y", sep_="-"){
 
        converter_date <- function(inpt_date, convert_to, frmt="snhdmy", sep_="-"){

          is_divisible <- function(inpt_v=c(), divisible_v=c()){

                cnt = 1

                while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                        inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) == 0]

                        cnt = cnt + 1

                }

                return(inpt_v)

          }

          leap_yr <- function(year){

                  if (year == 0){ return(F) }

                  if (year %% 4 == 0){
                    
                    if (year %% 100 == 0){
                      
                      if (year %% 400 == 0){
                        
                        bsx <- T
                        
                      }else{
                        
                        bsx <- F
                        
                      }
                      
                    }else{
                      
                      bsx <- T
                      
                    }
                    
                  }else{
                    
                    bsx <- F
                    
                  }

                  return(bsx)

          }

          inpt_date <- unlist(strsplit(x=inpt_date, split=sep_)) 

          stay_date_v <- c("s", "n", "h", "d", "m", "y")

          stay_date_val <- c(0, 0, 0, 0, 0, 0)

          frmt <- unlist(strsplit(x=frmt, split=""))

          for (el in 1:length(frmt)){

                  stay_date_val[grep(pattern=frmt[el], x=stay_date_v)] <- as.numeric(inpt_date[el]) 

          }

          if (stay_date_val[6] != 0 & stay_date_val[5] == 0){ stay_date_val[5] <- 1 }

          if (stay_date_val[5] != 0 & stay_date_val[4] == 0){ stay_date_val[4] <- 1 }

          l_dm1 <- c(31, 28, 31, 30, 31, 30, 31, 31,
                    30, 31, 30, 31)

          l_dm2 <- c(31, 29, 31, 30, 31, 30, 31, 31,
                    30, 31, 30, 31)

          if (!(leap_yr(year=stay_date_val[6])) & stay_date_val[6] != 0){

                l_dm <- l_dm1

          }else if (stay_date_val[6] == 0){

                l_dm <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

          }else{

                l_dm <- l_dm2

          }

          may_bsx_v <- c(1:stay_date_val[6])

          may_bsx_v <- may_bsx_v[may_bsx_v > 0] 

          may_bsx_v <- may_bsx_v[(may_bsx_v %% 4) == 0]

          val_mult <- c((1 / 86400), (1 / 1440), (1/24), 1, 0, 365)

          day_val = 0

          for (dt in length(stay_date_val):1){

                day_val = day_val + stay_date_val[dt] * val_mult[dt] 

          }

          day_val = day_val + length(may_bsx_v[(may_bsx_v %% 100) != 0]) + length(is_divisible(inpt_v=may_bsx_v, divisible_v=c(100, 400))) 

          if (str_detect(string=stay_date_val[5], pattern="\\.")){

                  all_part <- as.numeric(unlist(strsplit(x=as.character(stay_date_val[5]), split="\\.")))

                  int_part <- all_part[1]

                  if (int_part != 0){

                        day_val = day_val + sum(l_dm[1:int_part])

                  }else{ int_part <- 1 }

                  day_val = day_val + l_dm1[int_part] * as.numeric(paste("0.", all_part[2], sep=""))

          }else if (stay_date_val[5] != 0){

                day_val = day_val + l_dm1[stay_date_val[5]] 

          }

          val_mult2 <- c(60, 60, 24, 1)

          idx_convert <- grep(pattern=convert_to, x=stay_date_v)

          if (idx_convert < 5){

                for (i in 4:idx_convert){

                    day_val = day_val * val_mult2[i]

                }

                return(day_val)

          }else{

            year = 0

            l_dm <- l_dm1

            month = 0

            bsx_cnt = 0

            while ((day_val / sum(l_dm)) >= 1 ){

                l_dmb <- l_dm

                day_val2 = day_val

                day_val = day_val - sum(l_dm)

                month = month + 12

                year = year + 1

                if (!(leap_yr(year=year))){

                        l_dm <- l_dm1

                }else{

                        bsx_cnt = bsx_cnt + 1

                        l_dm <- l_dm2

                } 

            }

            if (leap_yr(year=year)){

                day_val = day_val - 1

            }

            cnt = 1

            while ((day_val / l_dm[cnt]) >= 1){

                day_val = day_val - l_dm[cnt]

                month = month + 1

                cnt = cnt + 1 

            }

            month = month + (day_val / l_dm[cnt])

            if (convert_to == "m"){

                    return(month)

            }else{

                    year = year + ((month - 12 * year) / 12)

                    return(year)

            }

          }

        }

        date_symb <- c("s", "n", "h", "d", "m", "y")

        date_val <- c(0, 0, 0, 0, 0, 0)

        convert_to_v <- unlist(strsplit(x=convert_to, split=""))

        pre_v <- c()

        for (el in convert_to_v){

            pre_v <- c(pre_v, grep(pattern=el, x=date_symb))

        }

        pre_v2 <- sort(x=pre_v, decreasing=T)

        cvrt_v <- date_symb[pre_v2]

        calcd <- as.character(converter_date(inpt_date=as.character(inpt_date), convert_to=cvrt_v[1], frmt=frmt)) 

        pre_str <- as.numeric(unlist(strsplit(x=calcd, split="\\.")))

        inpt_date <- paste("0.", pre_str[2], sep="")

        date_val[pre_v2[1]] <- pre_str[1]

        if (length(convert_to_v) > 1){

            for (el in 1:(length(cvrt_v) - 1)){

                calcd <- as.character(converter_date(inpt_date=as.character(inpt_date), convert_to=cvrt_v[el+1], frmt=cvrt_v[el]))
               
                pre_str <- as.numeric(unlist(strsplit(x=calcd, split="\\.")))

                inpt_date <- paste("0.", pre_str[2], sep="")

                date_val[pre_v2[el+1]] <- pre_str[1]

            }

        }

    return(paste(date_val[pre_v], collapse=sep_))

}


#' date_addr

date_addr <- function(date1, date2, add=F, frmt1, frmt2=frmt1, sep_="-", convert_to){
 
        converter_date <- function(inpt_date, convert_to, frmt="snhdmy", sep_="-"){

          is_divisible <- function(inpt_v=c(), divisible_v=c()){

                cnt = 1

                while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                        inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) == 0]

                        cnt = cnt + 1

                }

                return(inpt_v)

          }

          leap_yr <- function(year){

                  if (year == 0){ return(F) }

                  if (year %% 4 == 0){
                    
                    if (year %% 100 == 0){
                      
                      if (year %% 400 == 0){
                        
                        bsx <- T
                        
                      }else{
                        
                        bsx <- F
                        
                      }
                      
                    }else{
                      
                      bsx <- T
                      
                    }
                    
                  }else{
                    
                    bsx <- F
                    
                  }

                  return(bsx)

          }

          inpt_date <- unlist(strsplit(x=inpt_date, split=sep_)) 

          stay_date_v <- c("s", "n", "h", "d", "m", "y")

          stay_date_val <- c(0, 0, 0, 0, 0, 0)

          frmt <- unlist(strsplit(x=frmt, split=""))

          for (el in 1:length(frmt)){

                  stay_date_val[grep(pattern=frmt[el], x=stay_date_v)] <- as.numeric(inpt_date[el]) 

          }

          if (stay_date_val[6] != 0 & stay_date_val[5] == 0){ stay_date_val[5] <- 1 }

          if (stay_date_val[5] != 0 & stay_date_val[4] == 0){ stay_date_val[4] <- 1 }
          
          l_dm1 <- c(31, 28, 31, 30, 31, 30, 31, 31,
                    30, 31, 30, 31)

          l_dm2 <- c(31, 29, 31, 30, 31, 30, 31, 31,
                    30, 31, 30, 31)

          if (!(leap_yr(year=stay_date_val[6])) & stay_date_val[6] != 0){

                l_dm <- l_dm1

          }else if (stay_date_val[6] == 0){

                l_dm <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

          }else{

                l_dm <- l_dm2

          }

          may_bsx_v <- c(1:stay_date_val[6])

          may_bsx_v <- may_bsx_v[may_bsx_v > 0] 

          may_bsx_v <- may_bsx_v[(may_bsx_v %% 4) == 0]

          val_mult <- c((1 / 86400), (1 / 1440), (1/24), 1, 0, 365)

          day_val = 0

          for (dt in length(stay_date_val):1){

                day_val = day_val + stay_date_val[dt] * val_mult[dt] 

          }

          day_val = day_val + length(may_bsx_v[(may_bsx_v %% 100) != 0]) + length(is_divisible(inpt_v=may_bsx_v, divisible_v=c(100, 400))) 

          if (str_detect(string=stay_date_val[5], pattern="\\.")){

                  all_part <- as.numeric(unlist(strsplit(x=as.character(stay_date_val[5]), split="\\.")))

                  int_part <- all_part[1]

                  if (int_part != 0){

                        day_val = day_val + sum(l_dm[1:int_part])

                  }else{ int_part <- 1 }

                  day_val = day_val + l_dm1[int_part] * as.numeric(paste("0.", all_part[2], sep=""))

          }else if (stay_date_val[5] != 0){

                day_val = day_val + l_dm1[stay_date_val[5]] 

          }

          val_mult2 <- c(60, 60, 24, 1)

          idx_convert <- grep(pattern=convert_to, x=stay_date_v)

          if (idx_convert < 5){

                for (i in 4:idx_convert){

                    day_val = day_val * val_mult2[i]

                }

                return(day_val)

          }else{

            year = 0

            l_dm <- l_dm1

            month = 0

            bsx_cnt = 0

            while ((day_val / sum(l_dm)) >= 1 ){

                l_dmb <- l_dm

                day_val2 = day_val

                day_val = day_val - sum(l_dm)

                month = month + 12

                year = year + 1

                if (!(leap_yr(year=year))){

                        l_dm <- l_dm1

                }else{

                        bsx_cnt = bsx_cnt + 1

                        l_dm <- l_dm2

                } 

            }

            if (leap_yr(year=year)){

                day_val = day_val - 1

            }

            cnt = 1

            while ((day_val / l_dm[cnt]) >= 1){

                day_val = day_val - l_dm[cnt]

                month = month + 1

                cnt = cnt + 1 

            }

            month = month + (day_val / l_dm[cnt])

            if (convert_to == "m"){

                    return(month)

            }else{

                    year = year + ((month - 12 * year) / 12)

                    return(year)

            }

          }

        }

        date_converter_reverse <- function(inpt_date, convert_to="dmy", frmt="y", sep_="-"){
 
                converter_date <- function(inpt_date, convert_to, frmt="snhdmy", sep_="-"){

                  is_divisible <- function(inpt_v=c(), divisible_v=c()){

                        cnt = 1

                        while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                                inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) == 0]

                                cnt = cnt + 1

                        }

                        return(inpt_v)

                  }

                  leap_yr <- function(year){

                          if (year == 0){ return(F) }

                          if (year %% 4 == 0){
                            
                            if (year %% 100 == 0){
                              
                              if (year %% 400 == 0){
                                
                                bsx <- T
                                
                              }else{
                                
                                bsx <- F
                                
                              }
                              
                            }else{
                              
                              bsx <- T
                              
                            }
                            
                          }else{
                            
                            bsx <- F
                            
                          }

                          return(bsx)

                  }

                  inpt_date <- unlist(strsplit(x=inpt_date, split=sep_)) 

                  stay_date_v <- c("s", "n", "h", "d", "m", "y")

                  stay_date_val <- c(0, 0, 0, 0, 0, 0)

                  frmt <- unlist(strsplit(x=frmt, split=""))

                  for (el in 1:length(frmt)){

                          stay_date_val[grep(pattern=frmt[el], x=stay_date_v)] <- as.numeric(inpt_date[el]) 

                  }

                  if (stay_date_val[6] != 0 & stay_date_val[5] == 0){ stay_date_val[5] <- 1 }

                  if (stay_date_val[5] != 0 & stay_date_val[4] == 0){ stay_date_val[4] <- 1 }

                  l_dm1 <- c(31, 28, 31, 30, 31, 30, 31, 31,
                            30, 31, 30, 31)

                  l_dm2 <- c(31, 29, 31, 30, 31, 30, 31, 31,
                            30, 31, 30, 31)

                  if (!(leap_yr(year=stay_date_val[6])) & stay_date_val[6] != 0){

                        l_dm <- l_dm1

                  }else if (stay_date_val[6] == 0){

                        l_dm <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

                  }else{

                        l_dm <- l_dm2

                  }

                  may_bsx_v <- c(1:stay_date_val[6])

                  may_bsx_v <- may_bsx_v[may_bsx_v > 0] 

                  may_bsx_v <- may_bsx_v[(may_bsx_v %% 4) == 0]

                  val_mult <- c((1 / 86400), (1 / 1440), (1/24), 1, 0, 365)

                  day_val = 0

                  for (dt in length(stay_date_val):1){

                        day_val = day_val + stay_date_val[dt] * val_mult[dt] 

                  }

                  day_val = day_val + length(may_bsx_v[(may_bsx_v %% 100) != 0]) + length(is_divisible(inpt_v=may_bsx_v, divisible_v=c(100, 400))) 

                  if (str_detect(string=stay_date_val[5], pattern="\\.")){

                          all_part <- as.numeric(unlist(strsplit(x=as.character(stay_date_val[5]), split="\\.")))

                          int_part <- all_part[1]

                          if (int_part != 0){

                                day_val = day_val + sum(l_dm[1:int_part])

                          }else{ int_part <- 1 }

                          day_val = day_val + l_dm1[int_part] * as.numeric(paste("0.", all_part[2], sep=""))

                  }else if (stay_date_val[5] != 0){

                        day_val = day_val + l_dm1[stay_date_val[5]] 

                  }

                  val_mult2 <- c(60, 60, 24, 1)

                  idx_convert <- grep(pattern=convert_to, x=stay_date_v)

                  if (idx_convert < 5){

                        for (i in 4:idx_convert){

                            day_val = day_val * val_mult2[i]

                        }

                        return(day_val)

                  }else{

                    year = 0

                    l_dm <- l_dm1

                    month = 0

                    bsx_cnt = 0

                    while ((day_val / sum(l_dm)) >= 1 ){

                        l_dmb <- l_dm

                        day_val2 = day_val

                        day_val = day_val - sum(l_dm)

                        month = month + 12

                        year = year + 1

                        if (!(leap_yr(year=year))){

                                l_dm <- l_dm1

                        }else{

                                bsx_cnt = bsx_cnt + 1

                                l_dm <- l_dm2

                        } 

                    }

                    if (leap_yr(year=year)){

                        day_val = day_val - 1

                    }

                    cnt = 1

                    while ((day_val / l_dm[cnt]) >= 1){

                        day_val = day_val - l_dm[cnt]

                        month = month + 1

                        cnt = cnt + 1 

                    }

                    month = month + (day_val / l_dm[cnt])

                    if (convert_to == "m"){

                            return(month)

                    }else{

                            year = year + ((month - 12 * year) / 12)

                            return(year)

                    }

                  }

                }

                date_symb <- c("s", "n", "h", "d", "m", "y")

                date_val <- c(0, 0, 0, 0, 0, 0)

                convert_to_v <- unlist(strsplit(x=convert_to, split=""))

                pre_v <- c()

                for (el in convert_to_v){

                    pre_v <- c(pre_v, grep(pattern=el, x=date_symb))

                }

                pre_v2 <- sort(x=pre_v, decreasing=T)

                cvrt_v <- date_symb[pre_v2]

                calcd <- as.character(converter_date(inpt_date=as.character(inpt_date), convert_to=cvrt_v[1], frmt=frmt)) 

                pre_str <- as.numeric(unlist(strsplit(x=calcd, split="\\.")))

                inpt_date <- paste("0.", pre_str[2], sep="")

                date_val[pre_v2[1]] <- pre_str[1]

                if (length(convert_to_v) > 1){

                    for (el in 1:(length(cvrt_v) - 1)){

                        calcd <- as.character(converter_date(inpt_date=as.character(inpt_date), convert_to=cvrt_v[el+1], frmt=cvrt_v[el]))
                       
                        pre_str <- as.numeric(unlist(strsplit(x=calcd, split="\\.")))

                        inpt_date <- paste("0.", pre_str[2], sep="")

                        date_val[pre_v2[el+1]] <- pre_str[1]

                    }

                }

            return(paste(date_val[pre_v], collapse=sep_))

        }

        ptrn_twkr <- function(inpt_l, depth="max", sep="-", 
                              default_val="0", add_sep=T, end_=T){
          
          ln <- length(inpt_l)
          
          if (depth == "min"){
            
            pre_val <- str_count(inpt_l[1], sep)
            
            for (i in 2:ln){
              
              if (str_count(inpt_l[i], sep) < pre_val){
                
                pre_val <- str_count(inpt_l[i], sep)
                
              }
              
            }
            
            depth <- pre_val
            
          }

          if (depth == "max"){
            
            pre_val <- str_count(inpt_l[1], sep)
            
            for (i in 2:ln){
              
              if (str_count(inpt_l[i], sep) > pre_val){
                
                pre_val <- str_count(inpt_l[i], sep)
                
              }
              
            }
            
            depth <- pre_val
            
          }

          if (end_){

                  for (I in 1:ln){
                   
                    hmn <- str_count(inpt_l[I], "-")
                    
                    if (hmn < depth){
                     
                      inpt_l[I] <- paste0(inpt_l[I], sep, default_val)

                      diff <- depth - hmn - 1

                      if (diff > 0){
                      
                                if (add_sep == T){
                                  
                                  for (i in 1:diff){
                                  
                                    inpt_l[I] <- paste0(inpt_l[I], sep, default_val)
                                  
                                  }
                                
                                }else{
                                  
                                  for (i in 1:diff){
                                    
                                    inpt_l[I] <- paste0(inpt_l[I], default_val)
                                    
                                  }
                                  
                                }

                     }
                    
                    }else if(depth < hmn){

                        if (add_sep == T){

                                inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse=sep)

                        }else{

                                inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse="")
                       
                        }

                    }

                  }
          
          }else{

                for (I in 1:ln){
                   
                    hmn <- str_count(inpt_l[I], "-")
                    
                    if (hmn < depth){
                     
                      inpt_l[I] <- paste0(default_val, sep, inpt_l[I])

                      diff <- depth - hmn - 1

                      if (diff > 0){
                      
                                if (add_sep == T){
                                  
                                  for (i in 1:diff){
                                  
                                    inpt_l[I] <- paste0(default_val, sep, inpt_l[I])
                                  
                                  }
                                
                                }else{
                                  
                                  for (i in 1:diff){
                                    
                                    inpt_l[I] <- paste0(default_val, inpt_l[I])
                                    
                                  }
                                  
                                }

                     }
                    
                    }else if(depth < hmn){

                        if (add_sep == T){

                                inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse=sep)

                        }else{

                                inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse="")
                       
                        }

                    }

                  }

          }

          return(inpt_l)
          
        }

        date_symb <- c("s", "n", "h", "d", "m", "y")

        pre_v <- c()

        for (el in unlist(strsplit(x=frmt1, split=""))){

                pre_v <- c(pre_v, grep(x=date_symb, pattern=el))

        }

        min1 <- min(pre_v)

        pre_v <- c()

        for (el in unlist(strsplit(x=frmt2, split=""))){

                pre_v <- c(pre_v, grep(x=date_symb, pattern=el))

        }

        min2 <- min(pre_v)

        if (min1 < min2){

            date2 <- ptrn_twkr(inpt_l=date2, depth=((length(unlist(str_locate_all(string=date2, pattern=sep_))) / 2) + (min2 - min1)), end_=F, sep_=sep_)  

        }

        if (min1 < min2){

            date1 <- ptrn_twkr(inpt_l=date1, depth=((length(unlist(str_locate_all(string=date1, pattern=sep_))) / 2) + (min1 - min2)), end_=F, sep_=sep_)  

        }

        print(date1)

        print(date2)

}

#print(date_addr())


