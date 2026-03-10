## Get include and conditions vectors from capillary versus condition tables

get_include_and_conc_vectors <- function(table1,table2,table3,table4,conditions,row_per_table) {

    tot_cond <- length(conditions)
    if (tot_cond <= 96-row_per_table*1) {table4 <- NULL}
    if (tot_cond <= 96-row_per_table*2) {table3 <- NULL}
    if (tot_cond <= 96-row_per_table*3) {table2 <- NULL}

    DF1 <- hot_to_r(table1)
    include_vector    <- DF1$Include
    conditions_vector <- DF1$Concentration

    if (!(is.null(table2))) {
        DF2 <- hot_to_r(table2)
        include_vector    <- c(include_vector,DF2$Include)
        conditions_vector <- c(conditions_vector,DF2$Concentration)
    }

    if (!(is.null(table3))) {
        DF3 <- hot_to_r(table3)
        include_vector    <- c(include_vector,DF3$Include)
        conditions_vector <- c(conditions_vector,DF3$Concentration)
    }

    if (!(is.null(table4))) {
        DF4 <- hot_to_r(table4)
        include_vector    <- c(include_vector,DF4$Include)
        conditions_vector <- c(conditions_vector,DF4$Concentration)
    }

    conditions_vector <- as.numeric(conditions_vector)

return(list("include_vector"=include_vector,"concentration_vector"=conditions_vector))

}


simulate_concentration_vector <- function(
    conditions,initial_ligand,nreps,dil_factor,reverse) {

    total_cond <- length(conditions)
    temp_vec <- rep(1:ceiling(total_cond/nreps), each=nreps)

    conc_vec <- sapply(temp_vec,function(x) initial_ligand / (dil_factor**(x-1)))

    if (reverse) conc_vec <- rev(conc_vec)

    return(conc_vec)

}

## Get the 4 renderRHandsontable Tables when the user selects the autofill option
get_renderRHandsontable_list_filled <- function(
    conc_vec,n_rows_conditions_table,label_vec,include_vec=NULL) {

    table2 <- NULL
    table3 <- NULL
    table4 <- NULL

    total_cond <- length(conc_vec)

    if (is.null(include_vec)) {
        include_vec <- rep(TRUE,total_cond)
    }

    d1max <- min(n_rows_conditions_table,total_cond)

    data1 <- data.frame(Index=1:d1max,
              Concentration=conc_vec[1:d1max],
              Include=include_vec[1:d1max],
              Label=label_vec[1:d1max])

    table1 <- renderRHandsontable({
    rhandsontable(data1,maxRows=n_rows_conditions_table,rowHeaders=F)   %>%
    hot_col(c(2),format = "0[.]00")  %>%
    hot_col(c(1),readOnly = TRUE)

    })

    if (total_cond > n_rows_conditions_table ) {

        d2max <- min(n_rows_conditions_table*2,total_cond)
        data2 <- data.frame(Index=(n_rows_conditions_table+1):d2max,
                    Concentration=conc_vec[(n_rows_conditions_table+1):d2max],
                    Include=include_vec[(n_rows_conditions_table+1):d2max],
                    Label=label_vec[(n_rows_conditions_table+1):d2max])

        table2 <- renderRHandsontable({
        rhandsontable(data2,maxRows=n_rows_conditions_table,rowHeaders=F)  %>%
        hot_col(c(2),format = "0[.]00") %>%
        hot_col(c(1),readOnly = TRUE)

    })
    }

    if (total_cond > n_rows_conditions_table*2 ) {

        d3max <- min(n_rows_conditions_table*3,total_cond)
        data3 <- data.frame(Index=(n_rows_conditions_table*2+1):d3max,
                    Concentration=conc_vec[(n_rows_conditions_table*2+1):d3max],
                    Include= include_vec[(n_rows_conditions_table*2+1):d3max],
                    Label=label_vec[(n_rows_conditions_table*2+1):d3max])

        table3 <- renderRHandsontable({
        rhandsontable(data3,maxRows=n_rows_conditions_table,rowHeaders=F)  %>%
        hot_col(c(2),format = "0[.]00") %>%
        hot_col(c(1),readOnly = TRUE)

    })
    }

    if (total_cond > n_rows_conditions_table*3 ) {

        d4max <- min(n_rows_conditions_table*4,total_cond)
        data4 <- data.frame(Index=(n_rows_conditions_table*3+1):d4max,
                    Concentration=conc_vec[(n_rows_conditions_table*3+1):d4max],
                    Include= include_vec[(n_rows_conditions_table*3+1):d4max],
                    Label=label_vec[(n_rows_conditions_table*3+1):d4max])

        table4 <- renderRHandsontable({
        rhandsontable(data4,maxRows=n_rows_conditions_table,rowHeaders=F)  %>%
        hot_col(c(2),format = "0[.]00") %>%
        hot_col(c(1),readOnly = TRUE)

    })
    }

    return(list("table1"=table1,"table2"=table2,"table3"=table3,"table4"=table4))
}

get_renderRHandsontable_list_autofill <- function(
    conditions,labels,n_rows_conditions_table,
    initial_ligand,nreps,dil_factor,reverse) {

    n_conditions <- length(conditions)

    conc_vec <- simulate_concentration_vector(conditions,initial_ligand,nreps,dil_factor,reverse)

    conc_vec <- conc_vec[1:n_conditions]

    return(get_renderRHandsontable_list_filled(conc_vec,n_rows_conditions_table,labels))

}