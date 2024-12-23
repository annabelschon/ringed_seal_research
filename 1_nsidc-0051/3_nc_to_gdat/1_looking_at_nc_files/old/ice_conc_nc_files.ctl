DSET /data3/annabel/nsidc_0051/1_orig/NSIDC0051_SEAICE_PS_N25km_%y4%m2%d2_v2.0.nc
DTYPE netcdf
OPTIONS template yrev
TITLE NSIDC sea ice concentration data
UNDEF -9999.0
#XDEF   304 LINEAR -3837500.0  25000.0
#YDEF   448 LINEAR -5337500.0  25000.0
XDEF   304 LINEAR 1.0  1.0
YDEF   448 LINEAR 1.0  1.0
ZDEF     1 LEVELS 1
# start date:      1980         1         1       
# end date:        1980        12        31       
TDEF      366 LINEAR 01JAN1980 1dy
VARS  1
N07_ICECON=>n07_icecon   0  t,y,x  ice concentration (0-1)
ENDVARS
