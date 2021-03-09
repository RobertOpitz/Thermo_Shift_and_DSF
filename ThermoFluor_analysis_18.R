library(gdata)
library(xlsx)
#library(readxl)
library(gtools)
library(tcltk2)

#===============================================================================

compute_deriv_intensity <- function(temperature, intensity){
  
  # naiver Ansatz
  #n <- length(intensity) - 1
  #deriv_intensity <- rep(NA,n)
  #deriv_temperature <- rep(NA,n)
  
  #for ( i in seq( length=n ) ){
  #  deriv_intensity[i] <- (intensity[i+1] - intensity[i])/(temperature[i+1] - temperature[i])
  #  deriv_temperature[i] <- temperature[i] + 0.5 * (temperature[i+1] - temperature[i])
  #}
  
  # zentrale Differenz
  n <- length(intensity) - 2
  deriv_intensity <- rep(NA,n)
  deriv_temperature <- rep(NA,n)
  
  j <- 2
  for (i in seq(length = n)) {
    deriv_intensity[i] <- (intensity[j - 1] - intensity[j + 1])/(temperature[j - 1] - temperature[j + 1])
    deriv_temperature[i] <- temperature[j]
    j <- j + 1
  }
  
  return( list( deriv_intensity = deriv_intensity, 
                deriv_temperature = deriv_temperature ) )
}

#===============================================================================

get_data_CRIMS <- function(input_file) {
  
  # read excel file
  input <- read.xls( input_file )
  
  # number of cols
  n <- length( input[1,] )
  #n <- ncol(input)
  
  # number of rows
  m <- length( input[,1] )
  #m <- nrow(input)
  
  # number of data sets
  nds <- (n - 1)/2 # the first col is empty, therefore m is odd
  
  # data object that holds all data as alist
  output <- list()
  
  # create data object
  j <- 2 # skip first col
  for (i in seq( length = nds )) { #skip he first col, data start at second col 
    
    temperature <- as.numeric( as.vector( input[ 3:m, j ] ) ) 
    intensity <- as.numeric( as.vector( input[ 3:m, j + 1 ] ) )
    
    derivs <- compute_deriv_intensity( temperature, intensity )
    
    output[[i]] <- list( temperature = temperature,
                         intensity = intensity,
                         deriv_temperature = derivs$deriv_temperature,
                         deriv_intensity = derivs$deriv_intensity,
                         data_fit = list( ftc = list( result = NULL,         # fluorescence transition curve
                                                      model_type = NULL ),
                                          dsf = list( result = NULL,         # differential scanning fluorimetry, i.e. first derivative of ftc d(ftc)/dT
                                                      model_type = NULL ) )
                                    #list( result_int = NULL,
                                    #      result_dsf = NULL,
                                    #      model_type = NULL )
                      )
      
    j <- j + 2
    
  }
  
  return( output )
}

#===============================================================================

get_data <- function(input_file, screen_file = NULL) {
  
  # read excel file
  input <- read.xlsx( file = input_file, sheetIndex = 1 )
  #input <- read_xlsx(path = input_file, sheet = 1)
  #print(input)
  
  # data object that holds all data as alist
  output <- list()
  
  # number of rows
  #m <- length( input[,1] )
  #m <- nrow(input)
  
  # get temperature
  temperature <- as.numeric( as.vector( input[ ,2 ] ) )
  
  # number of data sets
  nds <- length( input[1,] ) - 2
  
  for (i in seq( length = nds)) {
    
    intensity <- as.numeric( as.numeric( input[ , i + 2 ] ) )
    
    derivs <- compute_deriv_intensity( temperature, intensity )
    
    output[[i]] <- list( temperature = temperature,
                         intensity = intensity,
                         deriv_temperature = derivs$deriv_temperature,
                         deriv_intensity = derivs$deriv_intensity,
                         buffer = "",
                         well = "",
                         data_fit = list( result_int = NULL,
                                          result_dsf = NULL,
                                          model_type_int = NULL,
                                          model_type_dsf = NULL ) 
                         )
    
  }
  
  for (i in seq_along(screen_file$buffer)) {
    output[[i]]$buffer <- as.character( screen_file$buffer[i] )
    output[[i]]$well   <- as.character( screen_file$well[i] )    
  }
  
  return( output )
}

#===============================================================================

plot_data <- function(input_data){
  
  #---BEGIN: INTERNAL FUNCTIONS-------------------------------------------------
  # plot_background_int <- function( temperature,
  #                                  data_fit,
  #                                  line_color )
  # {
  #   values <- coef( data_fit$result_int )
  #   
  # 
  #   if ( identical( data_fit$model_type_int, "linear_BG" ) ){
  #     
  #     # folded linear background
  #     Fn0 <- values[3]
  #     slope <- values[4]
  #     background <- slope * temperature + Fn0
  #     lines( x=temperature, 
  #            y=background, 
  #            lty=2,
  #            lwd=2,
  #            col=line_color )
  #     
  #     # unfolded linear background
  #     Fu0 <- values[5]
  #     slope <- values[6]
  #     background <- slope * temperature + Fu0
  # 
  #     lines( x=temperature, 
  #            y=background, 
  #            lty=2,
  #            lwd=2,
  #            col=line_color )
  #     
  #   }else if( identical( data_fit$model_type_int, "exp_BG" ) ){
  #     
  #     # folded linear background
  #     Fn0 <- values[4]
  #     slope <- values[5]
  #     background <- slope * temperature + Fn0
  #     lines( x=temperature, 
  #            y=background, 
  #            lty=2,
  #            lwd=2,
  #            col=line_color )
  #     
  #     # unfolded exponential background
  #     Fu0 <- values[6]
  #     bu <- values[3]
  #     background <- Fu0*exp( -( temperature - max(temperature) )/bu )
  #     
  #     lines( x=temperature, 
  #            y=background, 
  #            lty=2,
  #            lwd=2,
  #            col=line_color )
  #     
  #   }
  #   
  #   # plot line for Tm
  #   Tm <- exp(values[2])
  #   abline( v=Tm, lty=3 )
  # }
  # #---------------------------------------------------------------------------
  # plot_background_dsf <- function( temperature,
  #                                  data_fit,
  #                                  line_color )
  # {
  #  
  #   #-------------------------------------------------------------------------
  #   Kun <- function( temperature, Tm, DHm ){
  # 
  #     gas_constant <- 8.314462 # gas constant in J/(mol*K)
  #     TempK <- 273.15          # Kelvin at 0 °C
  #     
  #     temperature <- temperature + TempK
  #     Tm <- Tm + TempK
  # 
  #     return( exp( (DHm/gas_constant)*( (1.0/Tm) - (1.0/temperature) ) ) )
  #   }
  #   #-------------------------------------------------------------------------
  #   
  #   values <- coef( data_fit$result_dsf )
  #   print(values)
  #   
  #   if ( identical( data_fit$model_type_dsf, "linear_BG" ) ){
  #     
  #     DHm <- values[1]
  #     Tm <- values[2]
  #     da <- values[3]
  #     bu <- values[4]
  #     bn <- values[5]
  #   
  #     background <- ( bn + bu * Kun(temperature,Tm,DHm) ) / ( 1.0 + Kun(temperature,Tm,DHm) )
  #     #print(length(temperature))
  #     #print(length(background))
  #   
  #     lines( x=temperature,
  #            y=background,
  #            lty=2,
  #            lwd=2,
  #           col=line_color )
  #     
  #   }else if( identical( data_fit$model_type_dsf, "exp_BG" ) ){
  #     
  #     DHm <- values[1]
  #     Tm <- values[2]
  #     bu <- values[3]
  #     au <- values[4]
  #     bn <- values[5]
  #     an <- values[6]
  #     
  #     temp <- -(au/bu)*exp( -(temperature-max(temperature))/bu )
  #     
  #     background <- ( bn + temp * Kun(temperature,Tm,DHm) ) / ( 1.0 + Kun(temperature,Tm,DHm) )
  #     #print(length(temperature))
  #     #print(length(background))
  #     
  #     lines( x=temperature,
  #            y=background,
  #            lty=2,
  #            lwd=2,
  #            col=line_color )     
  #     
  #   }
  #   
  #   abline( v=Tm, lty=3 )
  #    
  # }
  #-----------------------------------------------------------------------------
  get_lyrics <- function(data_fit, data_type = "int", comment = NULL) {
    
    if (identical( data_type, "int" )) {
      
      lyrics <- "ThermoFluor"
      #results <- get_results_int( data_fit )

    }else if (identical( data_type, "dsf" )) {
      
      lyrics <- "DSF"
      #results <- get_results_dsf( data_fit )
      #}
      
    }else{
      stop( paste("ERROR in in function 'get_lyrics' in function 'plot_data': unknwon data_type -> ", data_type ))
    }

    # if ( !is.null( comment ) ) lyrics <- paste( lyrics ," - ", comment )
    # 
    # if ( !is.null(results) ){
    #   lyrics <- paste( lyrics, "\n",
    #                    "Tm = ", results$Tm$estimate, "(", results$Tm$se, ") °C; ",
    #                    "T1pro = ", results$T1pro$estimate, " °C\n",
    #                    "DS = ", results$DSm$estimate, "(", results$DSm$se, ") J/(mol*K); ",
    #                    "DH = ", results$DHm$estimate, "(", results$DHm$se, ") kJ/mol" )
    #   abline(v=results$T1pro$estimate, lty=3)
    # }
    
    return( lyrics )
    
  }
  #---END: INTERNAL FUNCTIONS---------------------------------------------------
  
  # find min and max values for xlim and ylim
  minTemp <- Inf
  maxTemp <- -Inf
  minInt <- Inf
  maxInt <- -Inf
  minDerivInt <- Inf
  maxDerivInt <- -Inf
  for (i in seq_along(input_data)) {
    rangeTemp <- range( input_data[[i]]$temperature )
    rangeInt <- range( input_data[[i]]$intensity )
    rangeDerivInt <- range( input_data[[i]]$deriv_intensity )
    
    if ( rangeTemp[1] < minTemp ) minTemp <- rangeTemp[1]
    if ( rangeTemp[2] > maxTemp ) maxTemp <- rangeTemp[2]
    if ( rangeInt[1] < minInt ) minInt <- rangeInt[1]
    if ( rangeInt[2] > maxInt ) maxInt <- rangeInt[2]
    if ( rangeDerivInt[1] < minDerivInt ) minDerivInt <- rangeDerivInt[1]
    if ( rangeDerivInt[2] > maxDerivInt ) maxDerivInt <- rangeDerivInt[2]
  }
  
  layout( c( 1,2 ) )
  
  # create colors
  nds <- length(input_data)
  if ( nds > 1)
    colors <- palette(rainbow(nds))
  else
    colors <- c("black")
  
  #--- Begin: temperature-intensity plot ---------------------------------------
  # create empty plot for temperature-intensity plot
  plot( x = NULL,
        y = NULL,
        main = get_lyrics( input_data[[i]]$data_fit$result_int, "int", 
                           input_data[[i]]$buffer ), #"ThermoFluor",
        xlab = "Temperature [°C]",
        ylab = "intensity [a.u.]",
        xlim = c( minTemp, maxTemp ),
        ylim = c( minInt, maxInt ) )
  
  # fill empty plot with data
  for (i in seq_along(input_data)) {
    
    # plot data
    points( input_data[[i]]$temperature,
            input_data[[i]]$intensity,
            col = colors[i] )
    
    # if there is fitted data plot that
    if ( !is.null( input_data[[i]]$data_fit$result_int ) & 
         !is.character( input_data[[i]]$data_fit$result_int ) ) { 
      
      # plot data fit
      lines( input_data[[i]]$temperature,
             predict( input_data[[i]]$data_fit$result_int ),
             lwd = 2,
             col = "red" )#colors[i] )
      
      # plot estimated background
      #plot_background_int( input_data[[i]]$temperature,
      #                     input_data[[i]]$data_fit,
      #                     "blue" )#colors[i] )
      
      print( summary( input_data[[i]]$data_fit$result_int ) )
      
    }
  }
  #--- End: temperature-intensity plot -----------------------------------------
  
  #--- Begin: temperature-deriv_intensity plot ---------------------------------
  # create empty main plot
  plot( x = NULL,
        y = NULL,
        main = get_lyrics( input_data[[i]]$data_fit$result_dsf, "dsf", 
                           input_data[[i]]$buffer ), #"first derivative dI/dT",
        xlab = "Temperature [°C]",
        ylab = "dI/dT [a.u./°C]",
        xlim = c( minTemp, maxTemp ),
        ylim = c( minDerivInt, maxDerivInt ) )
  
  abline(h = 0)
  
  for (i in seq_along(input_data)) {
    
    # plot deriv data
    points( input_data[[i]]$deriv_temperature,
            input_data[[i]]$deriv_intensity,
            col = colors[i] )
    
    # if there is fitted data plot that
    if ( !is.null( input_data[[i]]$data_fit$result_dsf ) & 
         !is.character( input_data[[i]]$data_fit$result_dsf ) ) {
      
      # plot data fit
      lines( input_data[[i]]$deriv_temperature,
             predict( input_data[[i]]$data_fit$result_dsf ),
             lwd = 2,
             col = "red" )#colors[i] )
      
      #plot_background_dsf( input_data[[i]]$deriv_temperature,
      #                     input_data[[i]]$data_fit,
      #                     "blue" )
      
      print( summary( input_data[[i]]$data_fit$result_dsf ) )
      
    }
    
  }
  #--- End: temperature-deriv_intensity plot -----------------------------------
  
}

#===============================================================================

print_all <- function(input_data) {
  
  filename <- tclvalue(tkgetSaveFile())
  if ( nchar(filename) ) {
    
    pdf( file = filename, paper = "a4" )
  
      for (i in seq_along(input_data)) {
        if ( identical( input_data[[i]]$buffer, "empty" ) ) next
        plot_data( sub_list( input_data, i, i ) )
      }
  
    dev.off()
  }
  
  return(invisible(NULL))
}

#===============================================================================

export_results <- function(input_data){

  filename <- tclvalue(tkgetSaveFile())
  if ( nchar(filename) ) {

    results_rfu <- NULL
    results_dsf <- NULL
    for (i in seq_along(input_data)) {
      
      if ( identical( input_data[[i]]$buffer, "empty" ) ) next
      
      if ( !is.null( input_data[[i]]$data_fit$result_int ) & 
           !is.character( input_data[[i]]$data_fit$result_int ) ) {
        
        # get data
        params_int <- get_results_int( input_data[[i]]$data_fit$result_int )
        
        # write in to table
        temp_int <- c( input_data[[i]]$well, 
                       input_data[[i]]$buffer, 
                       params_int$Tm$estimate, 
                       params_int$Tm$se,
                       params_int$T1pro$estimate,
                       params_int$DS$estimate, 
                       params_int$DS$se, 
                       params_int$DH$estimate, 
                       params_int$DH$se )
        
        results_rfu <- rbind( results_rfu, as.vector(temp_int) )
        
      }else{
        
        # write in to table
        temp_int <- c( input_data[[i]]$well, 
                       input_data[[i]]$buffer, 
                       NA, 
                       NA,
                       NA,
                       NA, 
                       NA, 
                       NA, 
                       NA )
        
        results_rfu <- rbind( results_rfu, as.vector(temp_int) )
        
      }
      
      if ( !is.null( input_data[[i]]$data_fit$result_dsf ) & 
           !is.character( input_data[[i]]$data_fit$result_dsf ) ) {
      
        # get data
        params_dsf <- get_results_dsf( input_data[[i]]$data_fit$result_dsf )
      
        # write in to table
        temp_dsf <- c( input_data[[i]]$well, 
                       input_data[[i]]$buffer, 
                       params_dsf$Tm$estimate, 
                       params_dsf$Tm$se,
                       params_dsf$T1pro$estimate,
                       params_dsf$DS$estimate, 
                       params_dsf$DS$se, 
                       params_dsf$DH$estimate, 
                       params_dsf$DH$se )
        
        results_dsf <- rbind( results_dsf, as.vector(temp_dsf) )
      
      }else{
        
        # write in to table
        temp_dsf <- c( input_data[[i]]$well, 
                       input_data[[i]]$buffer, 
                       NA, 
                       NA,
                       NA,
                       NA, 
                       NA, 
                       NA, 
                       NA )
        
        results_dsf <- rbind( results_dsf, as.vector(temp_dsf) )       
        
      }
    
    }

    # write object into file
    # write.table( results_rfu,
    #              file=paste(filename,"_RFU.dat"),
    #              row.names=FALSE,
    #              col.names = c( "No. data set", "RFU Tm [°C]", "RFU SE_Tm [°C]", "RFU DS [J/(mol*K)]", "RFU SE_DS [J/mol*K]", "RFU DH [kJ/mol]", "RFU SE_DH [kJ/mol]" )
    #            )
    results_rfu <- data.frame( results_rfu )
    colnames( results_rfu ) <- c( "well", "buffer", "RFU Tm [°C]", "RFU SE_Tm [°C]", "T1% [°C]", "RFU DS [J/(mol*K)]", "RFU SE_DS [J/mol*K]", "RFU DH [kJ/mol]", "RFU SE_DH [kJ/mol]" )
    
    write.xlsx( x = results_rfu,
                file = paste0( filename,  ".xlsx" ),
                sheetName = "RFU",
                append = FALSE,
                col.names = TRUE )
    
    # 
    # write.table( results_dsf,
    #              file=paste(filename,"_DSF.dat"),
    #              row.names=FALSE,
    #              col.names = c( "No. data set", "DSF Tm [°C]", "DSF SE_Tm [°C]", "DSF DS [J/(mol*K)]", "DSF SE_DS [J/mol*K]", "DSF DH [kJ/mol]", "DSF SE_DH [kJ/mol]" ) 
    #            )
    results_dsf <- data.frame( results_dsf )
    colnames( results_dsf ) <- c( "well", "buffer", "DSF Tm [°C]", 
                                  "DSF SE_Tm [°C]", "T1% [°C]", 
                                  "DSF DS [J/(mol*K)]", "DSF SE_DS [J/mol*K]", 
                                  "DSF DH [kJ/mol]", "DSF SE_DH [kJ/mol]" ) 
    
    write.xlsx( x = results_dsf,
                file = paste0( filename, ".xlsx" ),
                sheetName = "DSF",
                append = TRUE,
                col.names = TRUE )
    
  }
  
}

#===============================================================================

sub_list <- function(input, from = 1, to = 1) {

  if ( !is.list( input ) ) stop("ERROR in function 'sub_list': You have to provide a list")
  if ( from <= 0 & to <= 0 ) stop("ERROR in function 'sub_list': from and/or to have to be larger than zero")
  
  output <- list()
  #output <- vector("list", length = (from - to + 1))
  k <- 0
  for (i in seq(from,to)) {
    k <- k + 1
    output[[k]] <- input[[i]]
  }
  
  return( output )
}

#===============================================================================

do_dsf_regression <- function(data_set){
  
  for (i in seq_along(data_set)) {
    data_set[[i]] <- ThermoFluor_fit( data_set[[i]] )
    data_set[[i]] <- fit_DSF_signal( data_set[[i]] )
  }
  
  return( data_set )
}

#===============================================================================

get_Tm_start <- function(input_data){

  # get the data only for positive values of dI/dT
  temp <- pmax( input_data$deriv_intensity, 0.0 ) > 0.0
  
  if ( !any( temp ) ) {
    return( NULL )
  }
  
  # reduce data set to that values
  temperature <- input_data$temperature[temp]
  intensity <- input_data$intensity[temp]
  
  # find min and max Intsensity in reduced data set
  minInt <- min( intensity )
  maxInt <- max( intensity )
  halfInt <- 0.5 * (maxInt - minInt) + minInt
  
  Tm_start <- temperature[ which.max( intensity >= halfInt ) ]
  
  return( Tm_start )
}

#===============================================================================

ThermoFluor_fit <- function(input_data) {
  
  #-----------------------------------------------------------------------------
  
  Kun <- function(temperature, Tm, DSm) {

    gas_constant <- 8.314462 # gas constant in J/(mol*K)
    TempK <- 273.15          # Kelvin at 0 °C

    temperature <- temperature + TempK
    Tm <- Tm + TempK
    temp <- Tm/temperature

    return( exp( (DSm/gas_constant)*(1.0 - temp) ) )
  }
  
  #-----------------------------------------------------------------------------
  
  thermofluor_signal <- function(temperature, lnDSm, lnTm,
                                 bu = NULL, Tmax = NULL) {

    Kun_result <- Kun( temperature, exp(lnTm), exp(lnDSm) )
    Kun_result2 <- Kun_result + 1.0

    if ( !is.null(bu) & !is.null(Tmax) ) {

      # linear background for folded state
      # exp. background for unfolded state
      result <- cbind( 1.0 / Kun_result2,
                       temperature / Kun_result2,
                       (exp( -(temperature - Tmax)/bu ) * Kun_result) / Kun_result2
                     )

    }else{

      # linear backgorund for folded state
      # linear background for unfolded state
      result <- cbind( 1.0 / Kun_result2,
                       temperature / Kun_result2,
                       Kun_result / Kun_result2,
                       temperature * Kun_result / Kun_result2
                    )

    }

    return( result )
  }
  
  #-----------------------------------------------------------------------------
  
  thermofluor_signal_nls <- function(temperature, lnTm, lnDSm, n = NULL) {

    if ( length(lnTm) == 2 ) {
      Kun1 <- Kun( temperature, exp(lnTm[1]), exp(lnDSm[1]) )
      Kun2 <- Kun( temperature, exp(lnTm[2]), exp(lnDSm[2]) )
    }else{
      Kun1 <- Kun( temperature, exp(lnTm), exp(lnDSm) )
      Kun2 <- 0.0
    }

    n_length <- length(n)
    if ( n_length == 0 ) {
      n1 <- 0.0
      n2 <- 0.0
    }else if ( n_length == 1 ) {
      n1 <- n[1]
      n2 <- n[1]
    }else if ( n_length == 2 ) {
      n1 <- n[1]
      n2 <- n[2]
    }

    result <- cbind( (1.0/(1.0 + Kun1) + (n1/(1.0 + Kun2))),
                     (temperature * (1.0/(1.0 + Kun1) + n1/(1.0 + Kun2))),
                     (Kun1/(1.0 + Kun1) + n2 * Kun2/(1.0 + Kun2)),
                     (temperature * (Kun1/(1.0 + Kun1) + n2 * Kun2/(1.0 + Kun2)))
    )

    return( result )
  }
  
  #-----------------------------------------------------------------------------
  
  # prepare starting values: get starting value for Tm
  values <- get_Tm_start(input_data) #other_starting_value( input_data )
  if ( is.null( values ) ) {
    return( input_data )
  }else{
    Tm_start <- values
  }

  print("========================== DATA FIT ===================================")
  # do data fit
  
  temperature <- input_data$temperature
  intensity <- input_data$intensity
  
  lnDSm_start <- log(500)
  lnTm_start <- log(Tm_start)

  result_1trans <- try( nls( intensity ~ thermofluor_signal(temperature, 
                                                            lnDSm, lnTm),
                             algorithm = "plinear",
                             trace = TRUE,
                             start = list( lnDSm = lnDSm_start,
                                           lnTm  = lnTm_start )
                    ) 
                 )
    
  print( summary( result_1trans ) )
  
  if ( !is.character( result_1trans ) ) {
    values <- coef( result_1trans )
    print( exp(values[1:2]) )
  
    DS_start <- exp(values[1])
    DS_start <- c(2*DS_start,2*DS_start)
  
    Tm_start <- exp(values[2])
    Tm_start <- c( Tm_start - 5, Tm_start + 5 )
  
    result_2trans <- try( nls( intensity ~ thermofluor_signal_nls( temperature, 
                                                                   lnTm, lnDSm, 
                                                                   n),
                               algorithm = "plinear",
                               trace = FALSE,
                               start = list( lnDSm = log(DS_start),
                                             lnTm  = log(Tm_start),
                                             n   = c(1) ),
                               control = list( maxiter = 500 )
                            )
                        )
    print( summary(result_2trans) )  
    values <- try( exp( coef( result_2trans )[1:4] ) )
    print(values)
    
    if ( !is.character( result_2trans ) ) {
      input_data$data_fit$result_int <- result_2trans
      input_data$data_fit$model_type_int <- "linear_BG"
    }else{
      input_data$data_fit$result_int <- result_1trans
      input_data$data_fit$model_type_int <- "linear_BG"    
    }
    
  }
    
  # if ( !is.character( result1 ) ){
  # 
  #   values <- coef( result1 )
  #   Tmax <- max( temperature )
  # 
  #   result2 <- try( nls( intensity ~ thermofluor_signal( temperature, lnDSm, lnTm, bu, Tmax ),
  #                        algorithm = "plinear",
  #                        trace = TRUE,
  #                        start = list( lnDSm = values[1],
  #                                      lnTm  = values[2],
  #                                      bu = abs(values[5]/values[6]) - Tmax )
  #                       ) 
  #                 )
  # 
  #   print( summary( result2 ) )
  # 
  #   if ( !is.character( result2 ) ){
  #   
  #     print( paste( summary(result1)$sigma, summary(result2)$sigma ) )
  # 
  #     if ( summary(result1)$sigma > summary(result2)$sigma ){
  #   
  #       input_data$data_fit$result_int <- result2
  #       input_data$data_fit$model_type_int <- "exp_BG"
  #   
  #     }else{
  #   
  #       input_data$data_fit$result_int <- result1
  #       input_data$data_fit$model_type_int <- "linear_BG"
  #   
  #     }
  #     
  #   }else{
  # 
  #     input_data$data_fit$result_int <- result1
  #     input_data$data_fit$model_type_int <- "linear_BG"
  #           
  #   }
  
  #}
  
  return( input_data )
}

#===============================================================================

fit_DSF_signal <- function(input_data) {

  # rewrite as get function
  # gas_constant <- get_constants("gas_constant")
  # TempK <- get_constants("TempK")
  gas_constant <- 8.314462 # gas constant in J/(mol*K)
  TempK <- 273.15          # Kelvin at 0 °C
  
  #-----------------------------------------------------------------------------
  
  KunDH <- function(temperature, Tm, DHm) {
     
    temperature <- temperature + TempK
    Tm <- Tm + TempK

    return( exp( (DHm/gas_constant)*((1.0/Tm) - (1.0/temperature) ) ) )
  }
  
  #-----------------------------------------------------------------------------
  
  KunDS <- function(temperature, Tm, DSm) {
    
    #gas_constant <- 8.314462 # gas constant in J/(mol*K)
    #TempK <- 273.15          # Kelvin at 0 °C
    
    #Tm <- exp(lnTm)
    #DSm <- exp(lnDSm)
    
    temperature <- temperature + TempK
    Tm <- Tm + TempK
    temp <- Tm/temperature
    
    return( exp( (DSm/gas_constant) * (1.0 - temp) ) )
  }

  #-----------------------------------------------------------------------------
  
  compute_dsf_signal <- function(temperature, DHm, Tm, bu = NULL, Tmax = NULL) {
    
    K1 <- KunDH( temperature, Tm, DHm )
    K2 <- 1.0 + K1

    temperature <- temperature + TempK
    
    if (!is.null(bu) & !is.null(Tmax)) {
      
      C2 <- (DHm/(gas_constant*temperature^2)) / K2
      bu_inv <- 1.0/bu
      
      result <- cbind( exp( -(temperature - Tmax) * bu_inv) * (K1/K2) * (C2 - bu_inv),
                       (1.0/K2) * (1.0 - temperature * K1 * C2),
                       -C2 * K1/K2
                     )
    
    }else{
      
      C1 <- DHm/(gas_constant*temperature)
      
      result <- cbind( (C1/temperature) * K1 / K2^2,
                       (K1 * (C1 + K2)) / K2^2,
                       (K2 - K1 * C1) / K2^2 )
      
    }
    
    return( result )
  }
  
  #-----------------------------------------------------------------------------
  
  dsf_signal_nls <- function(temperature, lnTm, lnDSm, n = NULL) {
    
    #gas_constant <- 8.314462 # gas constant in J/(mol*K)
    #TempK <- 273.15          # Kelvin at 0 °C
    
    
    if (length(lnTm) == 2) {
      
      K1 <- KunDS( temperature, exp(lnTm[1]), exp(lnDSm[1]) )
      K2 <- KunDS( temperature, exp(lnTm[2]), exp(lnDSm[2]) )
      
      temperature <- temperature + TempK
      C1 <- (exp( lnDSm[1]) * exp(lnTm[1])) / (gas_constant * (temperature)^2)
      C2 <- (exp( lnDSm[2]) * exp(lnTm[2])) / (gas_constant * (temperature)^2)
      
    }else{
      
      K1 <- KunDS( temperature, exp(lnTm), exp(lnDSm) )
      K2 <- 0.0
      
      temperature <- temperature + TempK
      C1 <- (exp(lnDSm) * exp(lnTm)) / (gas_constant * (temperature)^2)
      C2 <- 0.0
    }
    
    n_length <- length(n)
    if (n_length == 0) {
      n1 <- 0.0
      n2 <- 0.0
    }else if (n_length == 1) {
      n1 <- n[1]
      n2 <- n[1]
    }else if (n_length == 2) {
      n1 <- n[1]
      n2 <- n[2]
    }
    
    result <- cbind( (C1 * K1/(1.0 + K1)^2 + n1 * C2 * K2/(1.0 + K2)^2),
                     ((K1/(1.0 + K1)) * ((C1 * temperature)/(1.0 + K1) + 1.0 ) + (n2*K2/(1.0 + K2))*(C2*temperature/(1.0 + K2) + 1.0) ),
                     ((1.0/(1.0 + K1)) * (1.0 - C1*K1*temperature/(1.0 + K1) ) + (n2/(1.0 + K2)) * (1.0 - C2 * K2 * temperature/(1 + K2)) )
    )
    
    return( result )
  }
    
  #-----------------------------------------------------------------------------

  # prepare starting values: get starting values for T_nu and DS
  value <- get_Tm_start( input_data )
  if ( is.null( value ) ) {
    return( input_data )
  }else{
    Tm_start <- value
  }

  temperature <- input_data$deriv_temperature
  dsf_intensity <- input_data$deriv_intensity

  print("DSF 1 trans")
  result_1trans <- try( nls(dsf_intensity ~ compute_dsf_signal(temperature, 
                                                               DHm, Tm),
                            algorithm = "plinear",
                            start = list(DHm = 500000.0,
                                         Tm  = Tm_start)
                    )
              ) 
  
  print( summary( result_1trans ) )

  if ( !is.character( result_1trans ) ) {
    print("DSF 2 trans")
    values <- coef( result_1trans )
    print( values[1:2] )
    
    DH_est <- values[1]
    Tm_start <- values[2]
    
    DS_start <- DH_est/(Tm_start + TempK)
    print(paste("DS -> ", DS_start))
    DS_start <- c(2.0 * DS_start, 2.0 * DS_start)
    Tm_start <- c(Tm_start - 5.0, Tm_start + 5.0)
    
    result_2trans <- try( nls(dsf_intensity ~ dsf_signal_nls( temperature, 
                                                              lnTm, lnDSm, n),
                              algorithm = "plinear",
                              trace = FALSE,
                              start = list( lnDSm = log(DS_start),
                                            lnTm  = log(Tm_start),
                                            n   = c(1) ),
                              control = list( maxiter = 500 )
                            )
                        )
    print( summary(result_2trans) )  
    values <- try( exp( coef( result_2trans )[1:4] ) )
    print(values)
    
    if ( !is.character( result_2trans ) ) {
      input_data$data_fit$result_dsf <- result_2trans
      input_data$data_fit$model_type_dsf <- "linear_BG"
    }else{
      input_data$data_fit$result_dsf <- result_1trans
      input_data$data_fit$model_type_dsf <- "linear_BG"    
    }
    
  }
  
  
  #print( cov( result1 ) )
  # if ( !is.character(result1) ){
  #   #sigma1 <- NULL
  # #}else{
  #   sigma1 <- summary( result1 )$sigma
  # #}
  # 
  # Tmax <- max( temperature )
  # values1 <- coef(result1)
  # 
  # if ( identical( input_data$data_fit$model_type_int, "exp_BG" ) )
  #   bu_start <- coef( input_data$data_fit$result_int )[3]
  # else
  #   bu_start <- 1
  #   
  # 
  # result2 <- try( nls( dsf_intensity ~ compute_dsf_signal( temperature, DHm, Tm, bu, Tmax ),
  #                      algorithm = "plinear",
  #                      start = list( DHm = values1[1],
  #                                    Tm  = values1[2],
  #                                    bu  = bu_start )
  #                   )
  #             ) 
  # 
  # print( summary( result2 ) )
  # if ( is.character(result2) ){
  #   sigma2 <- NULL
  # }else{
  #   sigma2 <- summary( result2 )$sigma
  # }
  # 
  # if ( !is.null(sigma1) ){
  # 
  #   if ( !is.null(sigma2) ){
  #     if ( (summary(result1)$sigma > summary(result2)$sigma) ){
  # 
  #     input_data$data_fit$result_dsf <- result2
  #     input_data$data_fit$model_type_dsf <- "exp_BG"
  #       
  #     }else{
  # 
  #       input_data$data_fit$result_dsf <- result1
  #       input_data$data_fit$model_type_dsf <- "linear_BG"
  # 
  #     }
  #   }else{
  #     
  #     input_data$data_fit$result_dsf <- result1
  #     input_data$data_fit$model_type_dsf <- "linear_BG"
  #     
  #   }
  #   
  # }
  # 
  # }
  
  return( input_data )
}

#===============================================================================

signifRound <- function(value, sd) {
  
  #if ( is.na(sd) ) return( mySignif(value,3) )
  
  #---Begin: Functions----------------------------------------------------------
  compute_power_of_ten <- function(input_data) {
    
    if (invalid(input_data)) return(NA)
    
    #print(paste("input_data ->", input_data))
    if (input_data >= 1.0) {
      
      temp <- input_data
      i <- 0
      repeat {
        temp <- temp %/% 10
        if (temp <= 0) break
        i <- i + 1
      }
      
    }else{
      
      temp <- input_data
      i <- 0
      repeat {
        temp <- temp * 10
        #print(paste("temp ->", temp))		
        if ( (temp %/% 10) > 0) break
        i <- i - 1
      }
      
    }
    
    return(i)
  }
  #---End: Functions------------------------------------------------------------
  
  potenz_von_sd <- compute_power_of_ten(sd)
  
  ergebnis <- NULL
  for (i in seq_along(value)) {
    
    if ( invalid( value[i] ) | invalid( potenz_von_sd ) ){
      ergebnis <- c( ergebnis, NA )
    }else{
      sign_of_value <- sign(value[i])
      
      temp <- abs(value[i])
      
      potenz_von_value <- compute_power_of_ten(temp)
      
      potenz_diff <- potenz_von_value - potenz_von_sd + 1
      
      ergebnis <- c( ergebnis, sign_of_value * signif(temp, digits = potenz_diff) )
    }
    
  }
  
  return(ergebnis)
}

#===============================================================================

get_results_int <- function(data_fit, compute_ci = FALSE) {
  
  if ( is.character(data_fit) | is.null(data_fit) ) {
    result <- NULL
  }else {
  
    # use get function for constants
    TempK <- 273.15
    gas_constant <- 8.314462 # gas constant in J/(mol*K)
    fn <- 0.99
  
    params <- summary( data_fit )$coefficients
  
    m <- length( params[,1] )
    ci <- matrix( rep(NA,m), nrow = m, ncol = 2 )
    if ( compute_ci == TRUE )
      ci <- tryCatch( confint( data_fit ), 
                      error = function(e) matrix( rep(NA,m), 
                                                  nrow = m, ncol = 2 ) )
  
    DSm <- exp(params[1,1])
    DSm_se <- DSm * params[1,2]
  
    Tm <- exp(params[2,1])
    Tm_se <- Tm * params[2,2]
  
    DSmTnu_se <- exp(vcov( data_fit )[1,2])
  
    DHm <- ((Tm + TempK) * DSm)/1000.0
    DHm_se <- sqrt( (DSm * Tm_se)^2 + ((Tm + TempK) * DSm_se)^2 + 2.0 * DSm * (Tm + TempK) * DSmTnu_se )/1000.0
    
    T1pro <- ((Tm + TempK) / (1.0 - (gas_constant/DSm) * log((1.0 - fn)/fn))) - TempK
  
    result <- list( Tm    = list( estimate = signifRound( Tm, Tm_se ),
                                  se       = signif( Tm_se, 1 ),
                                  ci       = signifRound( ci[2,], Tm_se ) ),
                    T1pro = list( estimate = signif( T1pro, 3 ) ),
                    DSm   = list( estimate = signifRound( DSm, DSm_se ),
                                  se       = signif( DSm_se, 1 ) ),
                    DHm   = list( estimate = signifRound( DHm, DHm_se ),
                                  se       = signif( DHm_se, 1 ) )
                  ) 
  
  }
  
  return( result )
}

#===============================================================================

get_results_dsf <- function(data_fit, compute_ci = FALSE){
  
  if ( is.character(data_fit) | is.null(data_fit) ) {
    
    result <- NULL
    
  }else{
    
    # use get function for constants
    TempK <- 273.15
    gas_constant <- 8.314462 # gas constant in J/(mol*K)
    fn <- 0.99
  
    params <- summary( data_fit )$coefficients
  
    m <- length( params[,1] )
    ci <- matrix( rep(NA,m), nrow = m, ncol = 2 )
    if ( compute_ci == TRUE )
      ci <- tryCatch( confint( data_fit ), 
                      error = function(e) matrix( rep(NA,m), 
                                                  nrow = m, ncol = 2 ) )
  
    DHm <- params[1,1]
    DHm_se <- params[1,2]
  
    Tm <- params[2,1]
    Tm_se <- params[2,2]
  
    DHmTm_se <- vcov( data_fit )[1,2]
  
    DSm <- DHm/(Tm + TempK)
    DSm_se <- sqrt( (DHm_se / (Tm + TempK))^2 + (Tm_se * DHm / (Tm + TempK)^2 )^2 - 2.0 * DHmTm_se / (Tm + TempK)^3)
  
    T1pro <- ((Tm + TempK) / (1.0 - ((gas_constant * (Tm + TempK))/DHm) * log((1.0 - fn)/fn))) - TempK
  
    DHm <- DHm/1000.0
    DHm_se <- DHm_se/1000.0
    
    result <-  list( Tm   = list( estimate = signifRound( Tm, Tm_se ),
                                  se       = signif( Tm_se, 1 ),
                                  ci       = signifRound( ci[2,], Tm_se )
                                ),
                    T1pro = list( estimate = signif( T1pro, 3 ) ),
                    DHm   = list( estimate = signifRound( DHm, DHm_se ),
                                  se       = signif( DHm_se, 1 )
                              ),
                    DSm   = list( estimate = signifRound( DSm, DSm_se ),
                                  se       = signif( DSm_se, 1 ) )
                  ) 
  
  }
  
  return(result)
}

#===============================================================================

complete_thermofluor_analysis <- function(screen_file_name = NULL){
  
  if ( is.null( screen_file_name ) ) {
    screen_file <- NULL 
  }else{
    screen_file <- read.table( file = screen_file_name, header = TRUE )
  }
  
  data_set <- get_data( file.choose(), screen_file )
  
  #print(data_set[[1]])
  #stop("BY USER")
  
  data_set <- do_dsf_regression( data_set )
  
  print_all( data_set )
  
  export_results( data_set )
  
}

#===============================================================================