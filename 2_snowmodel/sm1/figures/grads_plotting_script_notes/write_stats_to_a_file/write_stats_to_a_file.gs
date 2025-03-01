 'reinit'

 'open ../1_extract_merra2/merra2_1apr2014_budget.ctl'
 'open ../2_extract_era5/era5_1apr2014_budget.ctl'

 'set gxout stat'

 'set x 70 280'
 'set y 90 300'

 'set dfile 1'
 'd rpre_mn' ; line=sublin(result,11) ; ave1mn=subwrd(line,2)
 'd spre_mn' ; line=sublin(result,11) ; ave2mn=subwrd(line,2)
 'd ssub_mn' ; line=sublin(result,11) ; ave3mn=subwrd(line,2)
 'd bsub_mn' ; line=sublin(result,11) ; ave4mn=subwrd(line,2)
 'd roff_mn' ; line=sublin(result,11) ; ave5mn=subwrd(line,2)
 'd dyna_mn' ; line=sublin(result,11) ; ave6mn=subwrd(line,2)
 'd swed_mn' ; line=sublin(result,11) ; ave7mn=subwrd(line,2)
 'd sden_mn' ; line=sublin(result,11) ; ave8mn=subwrd(line,2)
 'd snod_mn' ; line=sublin(result,11) ; ave9mn=subwrd(line,2)

 'd rpre_mm' ; line=sublin(result,11) ; ave1mm=subwrd(line,2)
 'd spre_mm' ; line=sublin(result,11) ; ave2mm=subwrd(line,2)
 'd ssub_mm' ; line=sublin(result,11) ; ave3mm=subwrd(line,2)
 'd bsub_mm' ; line=sublin(result,11) ; ave4mm=subwrd(line,2)
 'd roff_mm' ; line=sublin(result,11) ; ave5mm=subwrd(line,2)
 'd dyna_mm' ; line=sublin(result,11) ; ave6mm=subwrd(line,2)
 'd swed_mm' ; line=sublin(result,11) ; ave7mm=subwrd(line,2)
 'd sden_mm' ; line=sublin(result,11) ; ave8mm=subwrd(line,2)
 'd snod_mm' ; line=sublin(result,11) ; ave9mm=subwrd(line,2)

  fmt = '%6.1f'
  ave1mn = math_format(fmt,ave1mn)
  ave2mn = math_format(fmt,ave2mn)
  ave3mn = math_format(fmt,ave3mn)
  ave4mn = math_format(fmt,ave4mn)
  ave5mn = math_format(fmt,ave5mn)
  ave6mn = math_format(fmt,ave6mn)
  ave7mn = math_format(fmt,ave7mn)
  ave8mn = math_format(fmt,ave8mn)
  ave9mn = math_format(fmt,ave9mn)

  ave1mm = math_format(fmt,ave1mm)
  ave2mm = math_format(fmt,ave2mm)
  ave3mm = math_format(fmt,ave3mm)
  ave4mm = math_format(fmt,ave4mm)
  ave5mm = math_format(fmt,ave5mm)
  ave6mm = math_format(fmt,ave6mm)
  ave7mm = math_format(fmt,ave7mm)
  ave8mm = math_format(fmt,ave8mm)
  ave9mm = math_format(fmt,ave9mm)


 'set dfile 2'
 'd rpre_en' ; line=sublin(result,11) ; ave1en=subwrd(line,2)
 'd spre_en' ; line=sublin(result,11) ; ave2en=subwrd(line,2)
 'd ssub_en' ; line=sublin(result,11) ; ave3en=subwrd(line,2)
 'd bsub_en' ; line=sublin(result,11) ; ave4en=subwrd(line,2)
 'd roff_en' ; line=sublin(result,11) ; ave5en=subwrd(line,2)
 'd dyna_en' ; line=sublin(result,11) ; ave6en=subwrd(line,2)
 'd swed_en' ; line=sublin(result,11) ; ave7en=subwrd(line,2)
 'd sden_en' ; line=sublin(result,11) ; ave8en=subwrd(line,2)
 'd snod_en' ; line=sublin(result,11) ; ave9en=subwrd(line,2)

 'd rpre_ee' ; line=sublin(result,11) ; ave1ee=subwrd(line,2)
 'd spre_ee' ; line=sublin(result,11) ; ave2ee=subwrd(line,2)
 'd ssub_ee' ; line=sublin(result,11) ; ave3ee=subwrd(line,2)
 'd bsub_ee' ; line=sublin(result,11) ; ave4ee=subwrd(line,2)
 'd roff_ee' ; line=sublin(result,11) ; ave5ee=subwrd(line,2)
 'd dyna_ee' ; line=sublin(result,11) ; ave6ee=subwrd(line,2)
 'd swed_ee' ; line=sublin(result,11) ; ave7ee=subwrd(line,2)
 'd sden_ee' ; line=sublin(result,11) ; ave8ee=subwrd(line,2)
 'd snod_ee' ; line=sublin(result,11) ; ave9ee=subwrd(line,2)

  fmt = '%6.1f'
  ave1en = math_format(fmt,ave1en)
  ave2en = math_format(fmt,ave2en)
  ave3en = math_format(fmt,ave3en)
  ave4en = math_format(fmt,ave4en)
  ave5en = math_format(fmt,ave5en)
  ave6en = math_format(fmt,ave6en)
  ave7en = math_format(fmt,ave7en)
  ave8en = math_format(fmt,ave8en)
  ave9en = math_format(fmt,ave9en)

  ave1ee = math_format(fmt,ave1ee)
  ave2ee = math_format(fmt,ave2ee)
  ave3ee = math_format(fmt,ave3ee)
  ave4ee = math_format(fmt,ave4ee)
  ave5ee = math_format(fmt,ave5ee)
  ave6ee = math_format(fmt,ave6ee)
  ave7ee = math_format(fmt,ave7ee)
  ave8ee = math_format(fmt,ave8ee)
  ave9ee = math_format(fmt,ave9ee)


  rc = write(SM_LG_aves.dat,'rainfall                  ,(cm SWE), 'ave1mn', 'ave1mm', 'ave1en', 'ave1ee)
  rc = write(SM_LG_aves.dat,'snowfall                  ,(cm SWE), 'ave2mn', 'ave2mm', 'ave2en', 'ave2ee)
  rc = write(SM_LG_aves.dat,'static-surface sublimation,(cm SWE), 'ave3mn', 'ave3mm', 'ave3en', 'ave3ee)
  rc = write(SM_LG_aves.dat,'blowing-snow sublimation  ,(cm SWE), 'ave4mn', 'ave4mm', 'ave4en', 'ave4ee)
  rc = write(SM_LG_aves.dat,'snowmelt runoff           ,(cm SWE), 'ave5mn', 'ave5mm', 'ave5en', 'ave5ee)
  rc = write(SM_LG_aves.dat,'ice dynamics              ,(cm SWE),      -, 'ave6mm',      -, 'ave6ee)
  rc = write(SM_LG_aves.dat,'SWE depth                 ,(cm SWE), 'ave7mn', 'ave7mm', 'ave7en', 'ave7ee)
  rc = write(SM_LG_aves.dat,'snow density              ,(kg m-3), 'ave8mn', 'ave8mm', 'ave8en', 'ave8ee)
  rc = write(SM_LG_aves.dat,'snow depth                ,    (cm), 'ave9mn', 'ave9mm', 'ave9en', 'ave9ee)


