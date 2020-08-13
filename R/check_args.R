


check_arg_type <- function(arg_value, arg_name, expected_type){

  if(expected_type == 'numeric') expected_type <- c('double', 'integer')

  arg_type <- typeof(arg_value)

  type_match <- arg_type %in% expected_type

  if (!type_match) {

    expected_types <- glue::glue_collapse(x = expected_type,
                                          sep = ', ',
                                          last = ' or ')

    error_msg <- glue::glue("{arg_name} should have type <{expected_types}>",
                            "\nbut instead has type <{arg_type}>")

    stop(as.character(error_msg), call. = FALSE)

  }

}

check_arg_length <- function(arg_value, arg_name, expected_length){

  if(is.null(expected_length)) return(invisible())

  arg_length <- length(arg_value)

  length_match <- arg_length %in% expected_length

  if (!length_match) {

    expected_lengths <- glue::glue_collapse(x = expected_length,
                                            sep = ', ',
                                            last = ' or ')

    error_msg <- glue::glue("{arg_name} should have length <{expected_lengths}>",
                            "\nbut instead has length <{arg_length}>")

    stop(as.character(error_msg), call. = FALSE)

  }

}

check_arg_bounds <- function(arg_value, arg_name, bound_lwr, bound_upr){

  if(!is.null(bound_lwr)) check_bound_lwr(arg_value, arg_name, bound_lwr)
  if(!is.null(bound_upr)) check_bound_upr(arg_value, arg_name, bound_upr)

}

check_bound_lwr <- function(arg_value, arg_name, bound_lwr) {

  if(any(arg_value < bound_lwr)){
    error_msg <- glue::glue("{arg_name} = {arg_value} should be >= {bound_lwr}")
    stop(as.character(error_msg), call. = FALSE)
  }

}

check_bound_upr <- function(arg_value, arg_name, bound_upr) {

  if(any(arg_value > bound_upr)){
    error_msg <- glue::glue("{arg_name} = {arg_value} should be <= {bound_upr}")
    stop(as.character(error_msg), call. = FALSE)
  }

}

check_call <- function(call, expected){

  arg_names <- setdiff( names(call), '' )

  #browser()
  n_frames <- length(sys.frames())

  for (arg_name in arg_names ){

    object_found <- FALSE

    n <- 1

    while(n <= n_frames & !object_found){

      arg_value <- try(
        eval(call[[arg_name]], envir = parent.frame(n = n)),
        silent = TRUE
      )

      if(!inherits(arg_value, 'try-error')) object_found <- TRUE

      n <- n + 1

    }

    if(inherits(arg_value, 'try-error'))
      stop("object '", deparse(call[[arg_name]]),"' not found",
           call. = FALSE)

    if(is.null(arg_value)) return(invisible())

    expected_type <- expected[[arg_name]]$type
    expected_length <- expected[[arg_name]]$length
    bound_lwr = expected[[arg_name]]$lwr
    bound_upr = expected[[arg_name]]$upr

    check_arg_type(arg_name = arg_name,
                   arg_value = arg_value,
                   expected_type = expected_type)

    check_arg_length(arg_name = arg_name,
                     arg_value = arg_value,
                     expected_length = expected_length)

    check_arg_bounds(arg_name = arg_name,
                     arg_value = arg_value,
                     bound_lwr = bound_lwr,
                     bound_upr = bound_upr)

  }

}
