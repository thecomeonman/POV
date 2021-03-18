#' Experimental - generating random trajectories with bounes
#' @import data.table
#' @export
fModelBounce = function(
   bounce_heights = c(10,5,2),
   bGoingUpAtStart = F,
   bGoingDownAtEnd = T,
   x = 10,
   y = 20,
   z = 3,
   endx = 40,
   endy = 30,
   endz = 2,
   ax = 0,
   ay = 0,
   az = -9.8
) {

   vz = c(0,sapply(bounce_heights[1:length(bounce_heights)],function(x)c(x,0)))

   vbLocalMinimas = which(vz > c(Inf, head(vz,-1)) & vz > c(tail(vz,-1), Inf))
   nextlocalminima = 2
   t = c()
   repeat {

      u = ( 2 * az * ( vz[nextlocalminima-1] - vz[nextlocalminima] ) ) ^ 0.5
      t = c(t,-2 * u / az)
      nextlocalminima = nextlocalminima + 2


      if ( nextlocalminima > max(vbLocalMinimas) ) {
         break
      }

   }

   vnTimeAtEnd = vbLocalMinimas + 1

   for ( i in seq(length(t)) ) {

      if ( i == 1 ) {

         u = (2 * -az * abs(vz[2]-z)) ^ 0.5
         t[1] = (t[1]/2) + ifelse(bGoingUpAtStart, -u/az, u/az)
         vz[1] = z
         vz[2] = ifelse(bGoingUpAtStart, vz[2], z)

      } else if ( i == length(vnTimeAtEnd) ) {

         u = (2 * -az * abs(vz[length(vz)-1]-endz)) ^ 0.5
         t[i] = (t[i]/2) + ifelse(bGoingDownAtEnd, -u/az, u/az)
         vz[length(vz)] = endz
         vz[length(vz)-1] = ifelse(bGoingDownAtEnd, vz[length(vz)-1], endz)

      }

   }

   dttrajectory = rbindlist(lapply(
      seq(length(vnTimeAtEnd)),
      function(i) {

         if ( i == 1 ) {

            x_increment = x
            y_increment = y

            if ( length(vnTimeAtEnd) == i ) {

               endx_increment = endx
               endy_increment = endy

            } else {

               endx_increment = (endx - x) * sum(t[1:i]) / sum(t)
               endx_increment = endx_increment + x
               endy_increment = (endy - y) * sum(t[1:i]) / sum(t)
               endy_increment = endy_increment + y

            }

         } else if ( i == length(vnTimeAtEnd) ) {

            x_increment = (endx - x) * sum(t[1:(i-1)]) / sum(t)
            x_increment = x_increment + x
            y_increment = (endy - y) * sum(t[1:(i-1)]) / sum(t)
            y_increment = y_increment + y

            endx_increment = endx
            endy_increment = endy

         } else {

            x_increment = (endx - x) * sum(t[1:(i-1)]) / sum(t)
            x_increment = x_increment + x
            y_increment = (endy - y) * sum(t[1:(i-1)]) / sum(t)
            y_increment = y_increment + y

            endx_increment = (endx - x) * sum(t[1:i]) / sum(t)
            endx_increment = endx_increment + x
            endy_increment = (endy - y) * sum(t[1:i]) / sum(t)
            endy_increment = endy_increment + y

         }

         if ( t[i] == 0 ) {

            t[i] = 1
            az_temp = 0

         } else {
            az_temp = az
         }


         qwe = fGet3DTrajectoryBetweenTwoPoints(
            x = x_increment, y = y_increment, z = vz[vnTimeAtEnd[i]-2],
            endx = endx_increment, endy = endy_increment, endz = vz[vnTimeAtEnd[i]],
            ax = 0, ay = 0, az = az_temp,
            t = t[i],
            timesteps = 100 * round(t[i],3)
         )

         data.table(qwe)

      }
   ))

   setnames(dttrajectory, c('x','y','z'))

   return ( dttrajectory )

}

