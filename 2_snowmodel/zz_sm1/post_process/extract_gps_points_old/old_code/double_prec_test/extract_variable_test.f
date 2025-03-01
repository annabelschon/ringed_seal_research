c extract_variable_test.f

      implicit none

      real snod
      integer irec,limit_size

      open (21,file=
     &  '/data3/svalbard/sm_coat_1-1e_tmp/snod_63years.gdat',
     &  form='unformatted',access='direct',recl=4*1)

c Single precision max integer size = 2,147,483,647.
c   Note that this is big enough to deal with a nx=1000, ny=1000,
c   daily outputs, for 5.9 years.  Anything bigger requires
c   double precision for irec when accessing single values out
c   of the (x,y,t) single-variable data cube.
      limit_size = 2147483647

c Double precision max integer size = 9,223,372,036,854,775,807.
      limit_size = 9223372036854775807

c Below.
c     irec = limit_size - 10000
c     irec = limit_size - 10010
c     irec = 100000

c Above.
      irec = limit_size + 200000
c     irec = limit_size + 200010

      read (21,rec=irec) snod

      print *, 100.0 * snod

      end

