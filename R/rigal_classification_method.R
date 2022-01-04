class.trajectory <- function (Y = NULL, X = NULL, dataset = NULL, interval_size = 0.5) {

  if (is.null(Y) == TRUE & is.null(Y) == TRUE & is.null(dataset) == TRUE) {
    stop("either 'dataset' or at least 'Y' and 'X' must be specified")
  }
  if (is.null(Y) == TRUE & is.null(Y) == TRUE) {
    Y <- dataset[,1]
    X <- dataset[,2]
  } else {
    if (class(Y) == "character" & class(X) == "character") {
      if (is.null(dataset) == TRUE) {
	stop("if 'Y' and 'X' are character, 'dataset' must exist")
      }else{
	Y <- dataset[, Y]
	X <- dataset[, X]
      }
    }else{
      if (!(class(Y) %in% c("numeric","integer")) == TRUE & !(class(X) %in% c("numeric","integer")) == TRUE) {stop("'Y' and 'X' must be either characters or vector but 'class' must be similar")}
    }
  }

  data <- data.frame(cbind(Y, X))
  data <- data[order(data$X),]                                                                      # ordering the X values

  if (length(X)<4){
    stop("time series length must be at least 4")
  }

  Y <- data$Y
  X <- data$X

  linear.model <- lm(Y~X)

  orthogonal_polynomial <- lm(Y~poly(X,2, raw=F))
  # After getting Y = gamma*chi + delta*X' + epsilon with orthogonal polynomial
  # we have to perform a variable change to obtain relevant values in the X interval 
  # for first_order_coefficient, second_order_coefficient and intercept,
  # knowing that X'= alpha*X + beta 
  # and chi = eta*X'^2 + theta

  gammab  <-  orthogonal_polynomial$coefficients[3]
  delta  <-  orthogonal_polynomial$coefficients[2]
  epsilon  <-  orthogonal_polynomial$coefficients[1]

  alpha  <-  lm(orthogonal_polynomial$model[, 2][, 1]~X)$coef[2]
  beta  <-  lm(orthogonal_polynomial$model[, 2][, 1]~X)$coef[1]

  eta  <-  1/lm((orthogonal_polynomial$model[, 2][, 1])^2~orthogonal_polynomial$model[, 2][, 2])$coef[2]
  theta  <-  (-lm((orthogonal_polynomial$model[, 2][, 1])^2~orthogonal_polynomial$model[, 2][, 2])$coef[1])*eta

  Y2<-Y*(max(X)-min(X))/(max(Y)-min(Y))
  # p2 and p3 are relevant when Y and X amplitudes are equivalent,
  # in particular when studying scaled-to-1 indices, Y and X amplitudes
  # may be very different, so we scaled the amplitudes to calculate p2 and p3 
  polynomial_orthonormal_basis<-lm(Y2~poly(X,2, raw=T))$coefficients

  if(summary(orthogonal_polynomial)$coefficients[3, 4] <= 0.05){                                     # non linear case
    classification <- data.frame(
      first_order_coefficient = (delta+2*beta*gammab*eta)*alpha,
      first_order_pvalue = summary(orthogonal_polynomial)$coefficients[2, 4],
      second_order_coefficient = (alpha^2)*gammab*eta,
      second_order_pvalue = summary(orthogonal_polynomial)$coefficients[3, 4],
      strd_error=summary(orthogonal_polynomial)$coefficients[2, 2],
      intercept = epsilon+beta*delta+(beta^2)*gammab*eta+gammab*theta,
      x_m = (X[length(X)]-X[1])/2+X[1],
      p1 = -(delta+2*beta*gammab*eta)/(2*alpha*gammab*eta),                    # points of interest
      p2 = (-polynomial_orthonormal_basis[2]+1)/(2*polynomial_orthonormal_basis[3]),
      p3 = (-polynomial_orthonormal_basis[2]-1)/(2*polynomial_orthonormal_basis[3]))
  }else{                                                                                            # linear case
    classification <- data.frame(
      first_order_coefficient = delta*alpha,
      first_order_pvalue = summary(orthogonal_polynomial)$coefficients[2, 4],
      second_order_coefficient = 0,
      second_order_pvalue = summary(orthogonal_polynomial)$coefficients[3, 4],
      strd_error=summary(orthogonal_polynomial)$coefficients[2, 2],
      intercept = epsilon+delta*beta,
      x_m = (X[length(X)]-X[1])/2+X[1],
      p1 = NA,
      p2 = NA,
      p3 = NA)
  }

  classification$r.sq <- summary(orthogonal_polynomial)$adj.r.squared                                # retrieve the adjusted coefficient of determination

  # compute the derivaive at xm-delta and at xm + delta with delta being half of the input interval size
  derivative  <-  2*(classification$x_m-(X[length(X)]-X[1])*(interval_size/2))*classification$second_order_coefficient+classification$first_order_coefficient
  derivative2  <-  2*(classification$x_m+(X[length(X)]-X[1])*(interval_size/2))*classification$second_order_coefficient+classification$first_order_coefficient


  if(sign(derivative) != sign(derivative2)){            
    # non consistent direction around x_m
    classification$derivative  <-  NA
    classification$intercept_derivative  <-  NA
  }else{
    # consistent direction around x_m
    classification$derivative  <-  mean(c(derivative, derivative2))
    classification$intercept_derivative  <-  (classification$second_order_coefficient*classification$x_m^2+classification$first_order_coefficient*classification$x_m+classification$intercept)-classification$x_m*classification$derivative
  }

  # compute the derivative of the curvature function
  classification$derivated_curvature  <-  -12*(classification$second_order_coefficient^2)*(2*classification$second_order_coefficient*classification$x_m+classification$first_order_coefficient)*(classification$second_order_coefficient/abs(classification$second_order_coefficient))/
    ((1+(2*classification$second_order_coefficient*classification$x_m+classification$first_order_coefficient)^2)^(2.5))

  if(classification$second_order_pvalue>0.05){classification$derivated_curvature <- NA}

  classification$direction <- NA                                                                    # classify the direction
  classification$direction[which(classification$derivative > 0)] <- "increase"
  classification$direction[which(classification$derivative < 0)] <- "decrease"
  classification$direction[which(is.na(classification$derivative))] <- "stable"
  classification$direction[which(as.numeric(classification$first_order_pvalue)>0.05 & as.numeric(classification$second_order_pvalue)>0.05)] <- "stable"

  classification$acceleration <- NA                                                                 # classify the acceleration
  classification$acceleration[which(classification$derivated_curvature < 0)] <- "accelerated"
  classification$acceleration[which(classification$derivated_curvature > 0)] <- "decelerated"
  classification$acceleration[which(classification$direction == "stable" &
    classification$second_order_coefficient < 0)] <- "concave"
  classification$acceleration[which(classification$direction == "stable" &
    classification$second_order_coefficient > 0)] <- "convex"
  classification$acceleration[which(is.na(classification$derivated_curvature))] <- "constant"

  classification$shape_class <- paste(classification$direction,                                       # give the final classification combining direction and acceleration
    classification$acceleration,
    sep="_")

  linear.model.summary <- summary(linear.model)                                                       # provide the linear approach results for comparison

  #Just to look: strd_error=summary(orthogonal_polynomial)$coefficients[2, 2],
  classification$linear_slope <- linear.model.summary$coefficients[2, 1]
  classification$linear_slope_strd_error <- linear.model.summary$coefficients[2, 2]
  classification$linear_slope_pvalue <- linear.model.summary$coefficients[2, 4]
  classification$linear_intercept <- linear.model.summary$coefficients[1, 1]

  classification$first_X_value <- X[1]
  classification$last_X_value <- X[length(X)]

  row.names(classification) <- "Y"

  return(classification)

}

#' Get rigal trajectory for multiple sites
#'
#'@examples
#'
#'tar_load(op_protocol)
#'tar_load(abun_rich_op)
#'
#'trends_data <- abun_rich_op %>%
#'  left_join(op_protocol, by = "op_id") %>%
#'  filter(siteid %in% mask_siteid)
#'
#'get_rigal_trajectory_classification(
#'  trends_data[trends_data$siteid == unique(trends_data$siteid)[2], ],
#'  y_var = "total_abundance",
#'  x_var = "year", site_id = "siteid")
get_rigal_trajectory_classification <- function(
  dataset = NULL,
  y_var = NULL,
  x_var = NULL,
  site_id = NULL) {

  site_id_sym <- rlang::sym(site_id)

  col_mask <- colnames(dataset) %in% c(site_id, y_var, x_var)
  data_to_analyse <- dataset[, col_mask] %>%
    na.omit() %>%
    group_by(!!site_id_sym) %>%
    nest()

  output <- data.frame(data_to_analyse[[site_id]])
  colnames(output) <- site_id

  classification <- furrr::future_map(data_to_analyse$data,
    ~try(class.trajectory(Y = .x[[y_var]], X = .x[[x_var]]))
  )

  error <- purrr::map_chr(classification, class) == "try-error"
  if (any(error)) {
    warning(paste0(length(which(error)), " stations/variable failed"))
  }

  to_add_to_output <- classification[!error] %>%
    bind_rows %>%
    mutate(!!site_id_sym := output[[site_id]][!error]) %>%
    as_tibble

  output <- output %>%
    left_join(to_add_to_output, by = site_id) %>%
    as_tibble

  return(output)
}


#' Compute rigal classification on multiple variable
#'
#' @param dataset data.frame
#' @param y_vars character vector indicating the response variables
#' @param time_var character string variable describing time
#' @param site_id_var character string 
#' @return data.frame
#' @examples
#' Todo
compute_rigal_classif <- function(
  dataset = NULL,
  y_vars = NULL,
  time_var = NULL,
  site_id_var = NULL
  ) {

  rigal_classif <- tibble(variable = y_vars)
  rigal_classif %<>%
    mutate(classif = purrr::map(variable,
      ~get_rigal_trajectory_classification(dataset = dataset,
        y_var = .x,
        x_var = time_var,
        site_id = site_id_var)
      )
  )

  return(rigal_classif)
}
