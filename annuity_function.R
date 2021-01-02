
rm(list = ls())

get_Omega <- function(Table, path) {
        
        Input <- paste0(path, "/", Table, ".csv")
        BiometricTable <- read.csv2(Input, skip = 0,
                                    header = TRUE,
                                    sep = ";",
                                    stringsAsFactors = FALSE,
                                    dec = ".")
        
        Omega <- max(BiometricTable[[1]])
        
        return(Omega)
        
}


whole_life_annuity <- function(Table, EntryAge, Omega, Amount, InterestRate,
                               expense_rate, Gender, m, t_0, t, current_year, 
                               GenerationTable, expenses_included, path) {
        
        Input <- paste0(path, "/", Table, ".csv")
        BiometricTable <- read.csv2(Input, skip = 0,
                                    header = TRUE,
                                    sep = ";",
                                    stringsAsFactors = FALSE,
                                    dec = ".")
        
        
        Menthonnex_faktoren <- FALSE
        
        if (length(grep("BV", Table)) > 0) {
                f_xy <- read.csv2(paste0(path, "/", "Menthonnex_faktoren.csv"), 
                                  skip = 0, 
                                  header = TRUE, 
                                  sep = ";", 
                                  stringsAsFactors = FALSE,
                                  dec = ".")
                Menthonnex_faktoren <- TRUE
        }
        
        if (ncol(BiometricTable) == 5) {
                names(BiometricTable) <- c("age", "q_x", "q_y", "lambda_x", "lambda_y")
                
        } else {
                names(BiometricTable) <- c("age", "q_x", "q_y")
        }
        
        if (missing(x = EntryAge) || EntryAge < min(BiometricTable[["age"]])) {EntryAge <- min(BiometricTable[["age"]])}
        
        if (missing(x = Omega) || Omega > max(BiometricTable[["age"]])) {Omega <- max(BiometricTable[["age"]])}
        
        if (Gender == "m" || Gender == "M" || Gender == 1) {
                j <- 2
        } else {
                j <- 3
        }
        
        v <- 1 / (1 + InterestRate)
        
        qx_t <- BiometricTable[BiometricTable[["age"]]>= EntryAge & BiometricTable[["age"]] < Omega, j]
        
        if (GenerationTable) {
                if (Menthonnex_faktoren) {
                        fx_index <- seq(from = 1, to = (Omega - EntryAge + 2) * (Omega - EntryAge - 1), by = (Omega - EntryAge + 1))
                        f_x_y <- f_xy[f_xy$year >= current_year & f_xy[["age"]] >= EntryAge, j + 1][fx_index]
                        qx <- qx_t * f_x_y
                } else {
                        lambda_t <- BiometricTable[BiometricTable[["age"]] >= EntryAge & BiometricTable[["age"]] < Omega, j + 2]
                        qx <- qx_t * exp(-lambda_t * (t - t_0 + seq(from = EntryAge, to = Omega - 1) - EntryAge))
                }
        } else {
                qx <- qx_t
        }
        
        if (!expenses_included) {expense_rate <- 0}
        px <- c(1, 1 - qx)
        
        tpx <- cumprod(x = px)
        aex <- cumsum(x = tpx * v^seq(from = 0, to = Omega - EntryAge)) - (m - 1) / (2 * m) ##### * (1 - tpx * v ^ seq(from = 0, to = Omega - EntryAge))
        
        
        Hv <- if (EntryAge > 40) {
                1
        } else {
                (EntryAge + 40) / 80
        }
        
        return(rev(x = aex)[1] * Amount * (1 + expense_rate) * Hv)
}


whole_life_annuity_cashflow <- function(Table, EntryAge, Amount, 
                                        InterestRate, expense_rate, Gender, m, 
                                        t_0, t, GenerationTable, expenses_included, path) {
        
        present_value <- mapply(FUN = whole_life_annuity,
                                Table = Table,
                                EntryAge = seq(from = EntryAge, to = get_Omega(Table, path)),
                                t_0 = t_0,
                                t = t + seq(from = 0, to = get_Omega(Table, path) - EntryAge),
                                InterestRate = InterestRate,
                                expense_rate = expense_rate,
                                Gender = Gender,
                                path = path,
                                GenerationTable = GenerationTable,
                                expenses_included = expenses_included,
                                m = m,
                                Amount = Amount,
                                USE.NAMES = FALSE)
        
        write.table(x = data.frame(EntryAge:get_Omega(Table, path), present_value),
                    file = file.path(directory, "present_value.csv"),
                    sep = ";", col.names = c("age", "present_value"))
        
}

# Table <- "GRMF_09"
# EntryAge <- 63
# Omega <- get_Omega(Table, directory)
# InterestRate <- 0.0175
# expense_rate <- 0.02
# Amount <- 1E0
# Gender <- "f"
# m <- 4
# t_0 <- 2010L
# t <- 2014L
# current_year <- 2018
# GenerationTable <- FALSE
# expenses_included <- FALSE
# path <- directory
# Table <- "ZLKR14GT_II"

directory <- file.path("C:", "Users", "Julia", "Documents", "R_files")

arguments <- list(Table = "GRMF_09", EntryAge = 63L, InterestRate = 0.0175,
                  expense_rate = 0.02, Amount = 1E0, Gender = "f",
                  m = 4, t_0 = 2014L, t = 2018L,
                  GenerationTable = F, expenses_included = F, path = directory)


do.call(what = whole_life_annuity_cashflow, args = arguments)





