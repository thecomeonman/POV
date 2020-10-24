#' @export
fGetNormalVector = function(
    mVector1
) {
    ( mVector1 / ( sum(mVector1^2) ^ 0.5 ) )
}