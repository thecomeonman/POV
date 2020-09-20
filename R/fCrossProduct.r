#' Cross product between two vectors
#' @param v1, a matrix [x,y,z]
#' @param v2, a matrix [x,y,z]
#' @export
fCrossProduct = function(v1, v2) {

    matrix(
        c(
            ( v1[2] * v2[3] ) - ( v1[3] * v2[2] ),
            ( v1[3] * v2[1] ) - ( v1[1] * v2[3] ),
            ( v1[1] * v2[2] ) - ( v1[2] * v2[1] )
        ),
        ncol = 3
    )

}