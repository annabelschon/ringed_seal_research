 'reinit'
 'open bsfx.ctl'
*'open /home/aschoen/seals/2_snowmodel/sm1/topo_vege/pan_arctic/topo_vege.ctl'
 'open /home/aschoen/seals/1_nsidc-0051/5_daily_to_yearly/1_mk_yearly_data/ice_conc_mask.ctl' 
*'open ../../2_seaice/5_mk_grads/conc_1987-2018_timesmth.ctl'
 'open /home/aschoen/seals/1_nsidc-0051/4_extract_temporal_subdomain/3_fill_polar_hole/ice_conc_extracted_domain.ctl'

 'set mpdraw off'
 'set gxout grfill'

*'set t 203'
 'set t 100'
* 'set t 50'
 
* 'd bsfx'
* 'cbarn'

* pull dummy
* 'set gxout fgrid'
 'set fgvals 1 61 2 11 3 11'
 'c'
 'd mask.2(t=1)'
 
 'd bsfx.1'
 'cbarn'
* pull dummy
*'c'
*'d conc.3'
*'cbarn'

 'gxprint bsfx.png png'
