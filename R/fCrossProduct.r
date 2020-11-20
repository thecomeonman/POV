#' Cross product between two vectors
#' @param v1, a matrix [x,y,z]
#' @param v2, a matrix [x,y,z]
#' @export
fCrossProduct = function(v1, v2) {
    
    if ( class(v1) != 'matrix') {
        warning('Assuming vector of 3 reals has been passed as v1')
        v1 = matrix(v1, ncol = 3)
    }

    if ( class(v2) != 'matrix') {
        warning('Assuming vector of 3 reals has been passed as v2')
        v2 = matrix(v2, ncol = 3)
    }

    mCP = cbind(
        ( v1[,2] * v2[,3] ) - ( v1[,3] * v2[,2] ),
        ( v1[,3] * v2[,1] ) - ( v1[,1] * v2[,3] ),
        ( v1[,1] * v2[,2] ) - ( v1[,2] * v2[,1] )
    )

    mCP

}