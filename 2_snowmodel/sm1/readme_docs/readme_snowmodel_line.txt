If you are running SnowModel without the blowing snow model,
you can run the code WITH ALL OF THE GRID CELLS OF INTEREST
in a line.  As an example, maybe you want to run all of the
glacier grid cells in the Northern Hemisphere for glaciers
over 1-km by 1-km in size, or all of the ice-free grid cells
in Greenland.  Both of these domains have MANY fewer grid
cells than would be covered by a rectangular domain.  If the
grid cells of interest do not communicate mass (blowing snow)
with each other, you don't have to run the nx by ny grid,
you can run an nx by ny=1 grid, where nx is the number of
grid cells you are interested in.

To do this requires a few things:

In the snowmodel.par file,

set: snowmodel_line_flag = 1.0

set: barnes_lg_domain = 1.0

set: n_stns_used = 1 to 4

Note that the code will find and use the nearest stations to
each grid cell, so you need to make sure you have at least
'n_stns_used' close by.  For example, if you want to use 1
'met station' for each grid cell of interest, and you have
set n_stns_used = 4, it will use the 4 nearest met points,
even if 3 of them are 100's of km away.

set: run_snowtran = 0.0

In the /extra_met/ directory you need to create a
snowmodel_line_pts.dat file that has 5 columns:

  icount,i,j,xg_line(i,j),yg_line(i,j)

where icount is the number of grid cells you are processing (=
nx), and i and j are the (i,j) positions of the grid cell in
the 2D array (icount, i, and j are not used in the SnowModel
code; they are usually used in post processing steps where you
want to put the SnowModel outputs back in the 2D array space).
xg_line and yg_line are the x and y coordinates of each grid
cell on the 2D grid space.

Below is an example code that would generate this
snowmodel_line_pts.dat file.

Your topo_vege.gdat file must also be in a line
that represents your simulation line.  For example,
/topo_vege/topo_vege_line.gdat, where nx=max_icount, ny=1.

If the grid cells are very far away from each other,
you will probably also need to make line versions of
/extra_met/grid_lat.gdat and /extra_met/grid_lon.gdat.


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c snowmodel_line_pts.f

c Extract the glacier points from the 2D domain.

      parameter (nx=13713,ny=13713)

      real vege(nx,ny)

      open (21,file='../topo_vege_2D/glaciers_topo_vege_1km.gdat',
     &  form='unformatted',access='direct',recl=4*nx*ny)
      read (21,rec=2) ((vege(i,j),i=1,nx),j=1,ny)

      open (31,file='snowmodel_line_pts.dat')

      xmn = -6856261.
      ymn = -6856739.
      deltax = 1000.0
      deltay = 1000.0

      icount = 0
      do j=1,ny
        do i=1,nx
          if (vege(i,j).eq.20.0) then
            icount = icount + 1

c xcoords of grid nodes at index i,j
c ycoords of grid nodes at index i,j
            xg = xmn + deltax * (real(i) - 1.0)
            yg = ymn + deltay * (real(j) - 1.0)

            write(31,90) icount,i,j,xg,yg

          endif
        enddo
      enddo

  90  format(i8,2i10,2f10.0)

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

