c conc_monthly_trends.f

c This script calculates the linear monthly trends through each grid
c   point.

      implicit none

c Parameters
      integer nx, ny, nt, i, j, n, nvars
      real x,y
      parameter (nx=304, ny=448, nt=516)
      parameter (nvars=1)

c Varibales
      real monthly_conc(nx, ny, nt), var(nx,ny,nt) 
      real slope(nx, ny), intercept(nx, ny), r2(nx, ny)
      real se_slope(nx, ny)
c     real sum_x, sum_x2, sum_y, sum_xy, mean_y
c     real residuals_sq_sum, variance_residuals, denominator
c     integer counter

c Misc
      real undef
      parameter (undef=-9999.0)

c Initialize variables
      slope = 0.0
      intercept = 0.0
      r2 = 0.0
      se_slope = 0.0

c Input and Output path
c      character*128 path_in, path_out
c      data path_in /
c     &  '/data3/annabel/seals/1_nsidc_0051/5_gdat_monthly/'/
c      data path_out /
c     &  '/data3/annabel/seals/1_nsidc_0051/5_gdat_monthly/'/

c Input and Output path
      character path_in*(*)
      parameter (path_in =
     & '/data3/annabel/seals/1_nsidc_0051/5_gdat_monthly/')

      character path_out*(*)
      parameter (path_out =
     & '/data3/annabel/seals/1_nsidc_0051/5_gdat_monthly/')

c Input files
      open (21,file=path_in//'glen_monthly.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)

c Output files
      open (61,file=path_out//'monthly_trends.gdat',
     &  form='unformatted',access='direct', recl=4*nx*ny,
     &  status='replace')

c Read in all the data.
      do n=1, nt
  
        print *, 'Reading month =', nt

        read (21,rec=n) ((monthly_conc(i,j,nt),i=1,nx),j=1,ny)
      enddo

c Call subroutine to calculate linear regression, r2, SE
     
      print *,'Calculating monthly trends...'
      call calculate_monthly_ave_trends(nx, ny, nt, monthly_conc,
     &  undef, slope, intercept, r2, se_slope)


c Write trends, intercepts, r-squared values, and SE
      
      print *, 'Writing data...'
      write(61, rec=1) ((slope(i, j), i=1, nx), j=1, ny)
      write(61, rec=2) ((intercept(i, j), i=1, nx), j=1, ny)
      write(61, rec=3) ((r2(i, j), i=1, nx), j=1, ny)
      write(61, rec=4) ((se_slope(i, j), i=1, nx), j=1, ny)

      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine calculate_monthly_ave_trends  (nx, ny, nt, var,
     &  undef,slope,intercept, r2, se_slope)

      implicit none

      real var(nx,ny,nt)
c     real average(nx,ny,nt)
      real slope(nx, ny), intercept(nx, ny), r2(nx, ny)
      real se_slope(nx,ny)
      real residuals_sq_sum, variance_residuals, denominator

c Local variables
      real sum_x, sum_x2, sum_xy, sum_y, mean_y, residuals_sq_sum
      real x, y
      integer nx, ny, nt, i, j, n, counter

c Loop through grid cells
     
      do j=1,ny
        do i=1,nx

c Initialize summation variables
          sum_x = 0.0
          sum_x2 = 0.0
          sum_xy = 0.0
          sum_y = 0.0
          mean_y = 0.0
          residuals_sq_sum = 0.0
          counter = 0.0
          

c Loop over time to calculate summations for linear regression
          do n=1, nt
            x = real(n)
            y = var(nx,ny,nt)
            if (y .ne. undef) then
              sum_x = sum_x + x
              sum_x2 = sum_x2 + x * x
              sum_y = sum_y + y
              sum_xy = sum_xy + x  * y
              counter = counter+1
            endif
          enddo

c Calcualte the trend, y intercept, r2
          if (counter > 1) then
            mean_y = sum_y / real(counter)
            slope(i, j) = (counter * sum_xy - sum_x * sum_y) /
     &               (counter * sum_x2 - sum_x * sum_x)
            intercept(i, j) = mean_y - slope(i, j) * (sum_x /
     &                real(counter))

            r2(i, j) = (sum_xy - sum_x * mean_y) /
     &                sqrt((sum_x2 - sum_x * sum_x / real(counter)) *
     &                    (sum_y - counter * mean_y * mean_y))

c Square to get final r2
            r2(i, j) = r2(i, j) * r2(i, j)

c Calculate residuals and SE
         do n=1,nt
           x = real(n)
           y = var(nx,ny,nt)
           if (y .ne. undef) then
             residuals_sq_sum = residuals_sq_sum +
     &          (y-(slope(i, j) * n + intercept(i, j)))**2
           endif
         enddo

c The variance of residuals is calcualted as the sum of the squared
c     residuals divided by DOF (which is 2 for a linear regression).
         variance_residuals = residuals_sq_sum / real(counter-2)

c Calcualte the denominator for the SE calculation of the slope, which
c     is the spread of the independent variable, x (here:time in mo).
         denominator = counter * sum_x2 - sum_x * sum_x
         se_slope(i, j) = sqrt(variance_residuals / denominator)
         
         else
           slope(i, j) = undef
           intercept(i, j) = undef
           r2(i, j) = undef
           se_slope(i, j) = undef
         endif
        enddo
       enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    
