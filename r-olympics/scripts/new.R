pib <- readRDS(file="..datas/pib.rds")

pib_habitant <- readRDS(file="..datas/pib_habitant.rds")

print(nrow(pib))

print(nrow(pib_habitant))

# On va devoir mettre en commun les periodes des données pour les pays pour pibet pib_habitantavec une fonction que j'ai eppélé intersect_mod

#' intersect_mod
#'
#' Returns the mods that have elements in common
#'
#' @param datf is the input dataframe
#' @param inter_col is the column name or the column number of the values that may be commun betwee the different mods
#' @param mod_col is the column name or the column number of the mods in the dataframe
#' @param ordered_descendly, in case that the elements in commun are numeric, this option can be enabled by giving a value of TRUE or FALSE see examples
#' @param n_min is the minimum elements in common a mod should have to be taken in count
#'
#' @examples
#'
#'
#'
#'
#'
#'
#'
#' @export

intersect_mod <- function(datf, inter_col, mod_col, n_min, descendly_ordered=NA){

    if (typeof(inter_col) == "character"){

            inter_col <- match(inter_col, colnames(datf))

    }

    if (typeof(mod_col) == "character"){

            mod_col <- match(mod_col, colnames(datf))

    }

    mods <- unique(datf[, mod_col])  

    final_intersect <- as.numeric(datf[datf[, mod_col] == mods[1], inter_col])

    mods2 <- c(mods[1])

    print(mods2)

    if (length(mods) > 1){

            for (i in 2:length(mods)){

                    cur_val <- as.numeric(datf[datf[, mod_col] == mods[i], inter_col])

                    if (length(intersect(final_intersect, cur_val)) >= n_min){

                            final_intersect <- intersect(final_intersect, cur_val)

                            mods2 <- c(mods2, mods[i])

                    }

            }

    }

    cur_datf <- datf[datf[, mod_col] == mods2[1], ]

    if (!is.na(descendly_ordered)){

            final_intersect <- sort(x=final_intersect, decreasing=FALSE)

            rtn_datf <- cur_datf[sort(match(final_intersect, cur_datf[, inter_col]), decreasing=descendly_ordered), ]

            if (length(mods2) > 1){

                    for (i in 2:length(mods2)){

                        cur_datf <- datf[datf[, mod_col] == mods2[i], ]

                        rtn_datf <- rbind(rtn_datf, cur_datf[sort(match(final_intersect, cur_datf[, inter_col]), decreasing=descendly_ordered), ])
    

                    }

            }

    }else{

            rtn_datf <- cur_datf[match(final_intersect, cur_datf[, inter_col]), ]

            if (length(mods2) > 1){

                    for (i in 2:length(mods2)){

                        cur_datf <- datf[datf[, mod_col] == mods2[i], ]

                        rtn_datf <- rbind(rtn_datf, cur_datf[match(final_intersect, cur_datf[, inter_col]), ])
    

                    }

            }

    }

    return(rtn_datf)

}

# Je vais choisir une période commune entre les pays d'au moins 16 ans

pib$year <- as.numeric(pib$year)

f_pib <- intersect_mod(datf=pib, inter_col="year", mod_col="iso_a3", n_min=16, descendly_ordered=NA)

print(nrow(f_pib))

f_pib_habitant <- intersect_mod(datf=pib_habitant, mod_col="iso_a3", inter_col="year",
n_min=16, descendly_ordered=NA)

print(nrow(f_pib_habitant))

print((unique(f_pib$year)))

# On remarque la période commune entre les pays de chaque tableau est identique

print((unique(f_pib_habitant$year)))

countries1 <- unique(f_pib$iso_a3)

countries2 <- unique(f_pib_habitant$iso_a3)

print(paste(length(countries1), length(countries2)))

# On remarque qu'un pays est manquant 

print(countries2[-match(countries2, countries1)])

# C'est donc l'ARE (émirats arabes unis) qui est le seul pays que les 2 tables n'ont pas en commun

saveRDS(object=f_pib, file="..datas/f_pib.rds")

saveRDS(object=f_pib_habitant, file="..datas/f_pib_hab.rds")










