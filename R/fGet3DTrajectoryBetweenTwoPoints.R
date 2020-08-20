#' Calculate a trajectory between two points
#' @export
fGet3DTrajectoryBetweenTwoPoints = function(
   x = 0,
   y = 0,
   z = 0,
   endx = 30,
   endy = 45,
   endz = 20,
   ax = 0,
   ay = 0,
   az = -9.8,
   t = 3,
   timesteps = 100
) {

   fGetInitialVelocity = function(
      s,
      t,
      a
   ) {
      ( s - ( 0.5 * a * t * t ) )  / t
   }

   fGetDisplacement = function(
      u,
      t,
      a
   ) {
      ( u * t ) + ( 0.5 * a * t * t )
   }

   ux = fGetInitialVelocity(
      endx - x,
      t,
      ax
   )

   uy = fGetInitialVelocity(
      endy - y,
      t,
      ay
   )

   uz = fGetInitialVelocity(
      endz - z,
      t,
      az
   )

   cbind(
      fGetDisplacement(ux, t * seq(0, timesteps) / timesteps, ax),
      fGetDisplacement(uy, t * seq(0, timesteps) / timesteps, ay),
      fGetDisplacement(uz, t * seq(0, timesteps) / timesteps, az)
   )

}
