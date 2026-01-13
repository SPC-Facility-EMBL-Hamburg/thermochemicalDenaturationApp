output$download_params_table <- downloadHandler(

    filename = function() {
    paste0("fitted_params_Chemelt_",Sys.Date(),".csv")
    },

    content  = function(file) {

        fitted_params <- pySample$params_df

        write.csv(
            fitted_params,
            file,
            row.names = F,
            quote = F
        )

    }
)

output$download_signal <- downloadHandler(

    filename = function() {
    paste0("signal_Chemelt_",Sys.Date(),".csv")
    },

    content  = function(file) {

        write.csv(
            reactives$signal_df,
            file,
            row.names = F,
            quote = F
        )

    }
)

output$download_signal_fitted <- downloadHandler(

    filename = function() {
    paste0("fitted_signal_Chemelt_",Sys.Date(),".csv")
    },

    content  = function(file) {

        write.csv(
            reactives$signal_df_fitted,
            file,
            row.names = F,
            quote = F
        )

    }
)