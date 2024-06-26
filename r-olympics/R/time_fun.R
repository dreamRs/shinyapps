time_fun <- function(x){

   time_col <- c()

   for (el in x){

           cur_el <- unlist(strsplit(x=el, split="")) 

           pre_grep <- grep(x=cur_el, pattern="-")

           if (!(identical(integer(0), pre_grep))){

                   if (length(pre_grep) > 1){

                        cur_el[pre_grep[1]:pre_grep[(length(pre_grep) - 1)]] <- "N"

                   }

           }

           time_col <- c(time_col, paste(cur_el[(pre_grep[length(pre_grep)]+1):length(cur_el)], collapse=""))

   }

   return(time_col)

}
