# fuzzy_date_join_rcpp.R

# SOURCE THE Rcpp FILE ----
Rcpp::sourceCpp("fuzzy_date_join_rcpp.cpp")


# R WRAPPER FUNCTIONS ----

# _ INNER FUZZY JOIN ----

inner_fuzzy_date_join <- 
  function(x, y, 
           x_id_col, y_id_col,
           x_date_col, y_date_col,
           x_intvl_less = 0, x_intvl_more = 0,
           keep_y_id = TRUE) {
    Z_rows <- 
      inner_fuzzy_date_join_cpp(
        x = x, y = y,
        x_id_col = x_id_col, y_id_col = y_id_col,
        x_date_col = x_date_col, y_date_col = y_date_col,
        x_intvl_less = x_intvl_less, 
        x_intvl_more = x_intvl_more)
    if (keep_y_id) {
      Z <- cbind(x[Z_rows$i_rows + 1, ],
                 y[Z_rows$j_rows + 1, ])
    } else {
      Z <- cbind(x[Z_rows$i_rows + 1, ],
                 y[Z_rows$j_rows + 1, c(-1)])
    }
    
    Z[[x_date_col]] <- as.Date(Z[[x_date_col]], origin = "1970-01-01")
    Z[[y_date_col]] <- as.Date(Z[[y_date_col]], origin = "1970-01-01")
    return(Z)
  }


# _ LEFT FUZZY JOIN ----

left_fuzzy_date_join <-
  function(x, y,
           x_id_col, y_id_col,
           x_date_col, y_date_col,
           x_intvl_less = 0, x_intvl_more = 0,
           keep_y_id = TRUE) {
    Z_rows <-
      left_fuzzy_date_join_cpp(
        x = x, y = y,
        x_id_col = x_id_col, y_id_col = y_id_col,
        x_date_col = x_date_col, y_date_col = y_date_col,
        x_intvl_less = x_intvl_less, 
        x_intvl_more = x_intvl_more)
    if (keep_y_id) {
      Z <- cbind(x[Z_rows$i_rows + 1, ],
                 y[Z_rows$j_rows + 1, ])
    } else {
      Z <- cbind(x[Z_rows$i_rows + 1, ],
                 y[Z_rows$j_rows + 1, c(-1)])
    }
    Z[[x_date_col]] <- as.Date(Z[[x_date_col]], origin = "1970-01-01")
    Z[[y_date_col]] <- as.Date(Z[[y_date_col]], origin = "1970-01-01")
    return(Z)
  }


# _ RIGHT FUZZY JOIN ----
#   *Note that this uses left_fuzzy_date_join_cpp

right_fuzzy_date_join <-
  function(x, y,
           x_id_col, y_id_col,
           x_date_col, y_date_col,
           y_intvl_less = 0, y_intvl_more = 0, 
           keep_x_id = TRUE) {
    Z_rows <-
      left_fuzzy_date_join_cpp(
        x = y, y = x,
        x_id_col = y_id_col, y_id_col = x_id_col,
        x_date_col = y_date_col, y_date_col = x_date_col,
        x_intvl_less = y_intvl_less, 
        x_intvl_more = y_intvl_more)
    if (keep_x_id) {
      Z <- cbind(x[Z_rows$j_rows + 1, ],
                 y[Z_rows$i_rows + 1, ])
    } else {
      Z <- cbind(x[Z_rows$j_rows + 1, c(-1)],
                 y[Z_rows$i_rows + 1, ])
    }
    Z[[x_date_col]] <- as.Date(Z[[x_date_col]], origin = "1970-01-01")
    Z[[y_date_col]] <- as.Date(Z[[y_date_col]], origin = "1970-01-01")
    return(Z)
  }




# # # # # # # #
# # # # # # # #
# EXTRA SPACE #
# # # # # # # #
# # # # # # # #

