#' @title calculate time in range for a pair of points
#' @description interpolate linarly between the two points with x-coord. #' x'
#'   and y-coord. 'y'. If 'x' represents time, TiR.help calculates the amount of
#'   time the curve spends in the interval given by 'y.int' during the time
#'   'x.int' .  If incl.low= TRUE (default FALSE) then the int is ''[ ]''
#'   (rather than ''( ]''
#' @param x numerical; x-coordinates of the two points of interest
#' @param y numerical; y-coordinates of the two points of interest
#' @param y.int numerical; the intervall (as a vector of length 2) of interest
#' @param x.int numerical; analogous to 'y.int'.  Default= -Inf,Inf
#' @param incl.low logical; if TRUE then int=c(a,b) is interpreted as [a,b] if
#'   FALSE then as (a,b]. (FALSE if missing)
#' @param y.int.order character; "allow"="a", "warn"="w" or "stop"="s". This
#'   tells the function how to deal with an unordered 'y.int'.
#' @param x.int.order character; analogous to 'y.int.order
#' @return If it does not stop due to incorrect arguments it always return a
#'   numeric value.
#' @export

tir.for.pair <- function(x, y,
                         y.int, x.int, incl.low,
                         y.int.order, x.int.order,
                         par.test = TRUE){
   # FIRST PART makes sure arguments are ok -------------------->
   if(par.test) {
      if(missing(incl.low)) incl.low <- F
      if(!is.logical(incl.low)) stop("[t_f_p]: 'incl.low' must be logical")
      if( missing(x.int) )  x.int <- c(-Inf,Inf)
      if( missing(y.int) ) {
         y.int <- c(2,3)
         warning("[t_f_p]: 'y.int' is missing and is set to c(2,3)")
      }
      if( missing(y.int.order) ) y.int.order <- "w"
      YINTORD <- c("allow","a","warn","w","stop","s")
      if( !is.element(y.int.order,YINTORD) ) {
         stop("[t_f_p]: 'y.int.order' must be 'allow', 'warn' or 'stop'")
      }
      if( missing(x.int.order) ) x.int.order <- "a"
      XINTORD <- c("allow","a","warn","w","stop","s")
      if( !is.element(x.int.order,XINTORD) ) {
         stop("[t_f_p]: 'x.int.order' must be 'allow', 'warn' or 'stop'")
      }
      if( any( is.na( c(x, y, x.int, y.int) ) ) ) {
         stop("[t_f_p]: No missing values allowed in 'x','y','x.int' or 'y.int'")
      }
      if( length(x)!=2 | length(y)!=2 | length(y.int)!=2 | length(x.int)!=2 ) {
         stop("[t_f_p]: All numerical arguments must be of length 2")
      }
      if( y.int[1]==y.int[2] ) {
         warning("[t_f_p]: Degenerate 'y.int'")
         return(0)
      }
      if( x.int[1]==x.int[2] ){
         warning("[t_f_p]: Degenerate 'x.int'")
         return(0)
      }
      if( x[1]==x[2] ){
         warning("[t_f_p]: Degenerate 'x'")
         return(0)
      }
      if( is.element(y.int.order,YINTORD[3:6]) ) {
         if( y.int[1]>y.int[2] ) {
            if( is.element(y.int.order,YINTORD[5:6]) ) {
               stop("[t_f_p]: 'y.int' is unordered")
            } else {
               warning("[t_f_p]: 'y.int' is unordered")
            }
         }
      }
      y.int <- sort(y.int)
      if( is.element(x.int.order,XINTORD[3:6]) )  {
         if( x.int[1]>x.int[2] ) {
            if( is.element(x.int.order,XINTORD[5:6]) ) {
               stop("[t_f_p]: 'x.int' is unordered")
            } else {
               warning("[t_f_p]: 'x.int' is unordered")
            }
         }
      }
      x.int <- sort(x.int)
      if(x[1]>x[2])  stop("[t_f_p]: 'x' must be ordered")
      if( abs(x[1])==Inf | abs(x[2])==Inf | abs(y[1])==Inf | abs(y[2])==Inf) {
         stop("[t_f_p]: 'x' and 'y' must be finite.")
      }
   }
   # END OF FIRST PART <------------------------------------------

   if( x[1]>= x.int[2] | x[2] <= x.int[1] ) return(0)
   if( sort(y)[1]> y.int[2] | sort(y)[2] < y.int[1] ) return(0)

   slope     <- ( y[1]-y[2] ) / ( x[1]-x[2] )
   intercept <- ( x[1]*y[2] - x[2]*y[1] ) / ( x[1]-x[2] )

   if( x[1]>= x.int[1] ) {
      X1 <- x[1]
   } else {
      X1 <- x.int[1]
   }
   if( x[2]<= x.int[2] ) {
      X2 <- x[2]
   } else {
      X2 <- x.int[2]
   }
   if( y[1]==y[2] ) {
      if( y[1]<=y.int[2] & y[1]>y.int[1] ) {
         return( X2-X1 )
      }
      if( y[1]==y.int[1] & incl.low )  {
         return( X2-X1)
      } else {
         return(0)
      }
   }

   if( y[1] < y.int[1] ) {
      X1  <- ( y.int[1] - intercept ) / slope
   } else {
      if( y[1] > y.int[2] ) {
         X1 <- ( y.int[2] - intercept ) / slope
      } else {
         X1 <- x[1]
      }
   }

   if( y[2] > y.int[2] ) {
      X2  <- ( y.int[2] - intercept ) / slope
   } else {
      if( y[2] < y.int[1] )  {
         X2 <- ( y.int[1] - intercept ) / slope
      } else {
         X2 <- x[2]
      }
   }

   if( X1>= x.int[2] | X2 <= x.int[1] ) return(0)
   if( X1 < x.int[1] ) X1 <- x.int[1]
   if( X2 > x.int[2] ) X2 <- x.int[2]

   return( X2-X1 )
}
