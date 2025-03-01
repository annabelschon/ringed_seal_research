 'reinit'

 'open /home/aschoen/seals/2_snowmodel/sm1/topo_vege/pan_arctic/topo_vege.ctl'
 'open mask.ctl'

 'set mpdraw off'

 'set gxout fgrid'
 'set fgvals 24 15'
 'd vege.1'

 pull dummy

 'set gxout grfill'
 'd mask.2(t=1)'
 
 'gxprint LME1.png png'

*'set gxout fgrid'
*'set fgvals 18 2'
*'d vege.1'

