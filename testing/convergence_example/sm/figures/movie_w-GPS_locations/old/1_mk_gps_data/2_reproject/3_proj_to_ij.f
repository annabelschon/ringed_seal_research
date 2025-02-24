c 3_proj_to_ij.f

c Convert the projection coordinates to decimal (i,j) coordinates.
c   This just makes the plotting cleaner and less complicated,
c   because the conversion is done here instead of within the
c   plotting script.

      implicit none

      integer max_lines,k

      parameter (max_lines=1000000)

      double precision x,y,xmn,ymn,deltax,deltay,dec_i,dec_j

c Provide the SnowModel domain coordinate information from the
c   snowmodel.par file.  The "D0" on the end of these numbers
c   make them double precision for the calculations.
      deltax = 5000.0D0
      deltay = 5000.0D0

      xmn = -1498000.0D0
      ymn = 502000.0D0

c Input and output files.
      open (21,file='proj_coords.dat')
      open (31,file='ij_coords.dat')

c Read the projection coordinates.
      do k=1,max_lines
        read (21,*,end=99) x,y

c Convert the x and y locations to decimal (ii,jj) locations.
        dec_i = 1.0 + (x - xmn) / deltax
        dec_j = 1.0 + (y - ymn) / deltay

        write (31,91) dec_i,dec_j
      enddo

   99 continue

   91 format (2f14.6)

      end

