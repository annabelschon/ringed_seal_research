DSET /data3/annabel/seals/1_nsidc_0051/1_orig/NSIDC0051_SEAICE_PS_N25km_%y4%m2%d2_v2.0.nc
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
# end date:        2024        03        31       
TDEF    16162 LINEAR 01JAN1980 1dy
VARS  5
N07_ICECON=>n07_icecon   0  t,y,x  ice concentration (0-1)
F08_ICECON=>f08_icecon   0  t,y,x  ice concentration (0-1)
F11_ICECON=>f11_icecon   0  t,y,x  ice concentration (0-1)
F13_ICECON=>f13_icecon   0  t,y,x  ice concentration (0-1)
F17_ICECON=>f17_icecon   0  t,y,x  ice concentration (0-1)
ENDVARS
