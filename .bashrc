# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

# Begin Glen's additions.

#alias dir='ls -alFo'
alias dir='ls -lFov --color'
alias dirh='ls -lFovh --color'
alias del='rm -i'
#alias df='df -h -x fuse.sshfs -x tmpfs -x devtmpfs -T'
alias df='df -h -x devtmpfs -x tmpfs --output=fstype,size,used,avail,pcent,target'
alias vi='vim'
alias more='less -i'
alias last='last -40'
alias cdd='cd /data1/working/'
alias grass='grass64 -text'
alias time='time -p'
#alias time='/usr/bin/time -f "\n%E real [(hh:)mm:ss.ss]\n%U user [(hh:)mm:ss.ss]\n%S sys  [(hh:)mm:ss.ss]\n"'
#alias time2='/usr/bin/time -f "\n%e real [ss.ss]\n"'
alias lpr='lpr -P printer10'
alias gfortran='gfortran -Wall -mcmodel=medium'
alias gf='gfortran -Wall'
alias pgf77='pgf77 -mcmodel=medium'

# Use this like, du * or du -h *, to get file and directory sizes.
alias du='du -s --apparent-size'

alias ssh='ssh -X'

# use this to view .tif files: openev test.tif
alias openev='openev -h'

# use this to display .png, .eps, and .pdf files
alias display='display -resize 900x700\>'

# enscript -2r toss.f
alias enscript='enscript -M Letter -DDuplex:false -P printer10'
alias enscript1s='enscript -M Letter -DDuplex:false -P printer10'
alias enscript2s='enscript -M Letter -DDuplex:true -P printer10'

export PATH="$PATH:$HOME/util"

#export PATH="$PATH:$HOME/fwtools/FWTools/bin_safe"

export PATH="$PATH:/local/opt/R/bin"

# PGI ENVIRONMENT
export PATH="$PATH:/opt/pgi/linux86-64/15.4/bin"
export PGI=/opt/pgi/linux86-64/10.9
export MANPATH=$MANPATH:$PGI/man

# GRADS ENVIRONMENT
export GADDIR=/home/gliston/grads/data
export GASCRP=/home/gliston/grads/lib
export GAUDPT=/local/opt/grads/lib

#Old laptop
#alias gradsp='grads -p -g 635x825+0+0 -c "set imprun imprun.gs"'
#alias gradsl='grads -l -g 1065x825+0+0 -c "set imprun imprun.gs"'
#alias gradspp='grads -p -g 600x775+0+0'
#alias gradsll='grads -l -g 775x600+0+0'

#New laptop, with 300% display.
#alias gradsp='grads -p -g 386x500+0+0 -c "set imprun imprun.gs"'
#alias gradsl='grads -l -g 647x500+0+0 -c "set imprun imprun.gs"'
#alias gradspp='grads -p -g 309x400+0+0'
#alias gradsll='grads -l -g 518x400+0+0'

#New laptop, with 225% display (ratio = 1.29, 0.77).
alias gradsp='grads -p -g 541x700+0+0 -c "set imprun imprun.gs"'
alias gradsl='grads -l -g 840x649+0+0 -c "set imprun imprun.gs"'
#alias gradsl='grads -l -g 907x700+0+0 -c "set imprun imprun.gs"'
alias gradspp='grads -p -g 386x500+0+0'
alias gradsll='grads -l -g 647x500+0+0'

alias gradsp2='grads -p -g 541x700+542+0 -c "set imprun imprun.gs"'
alias gradsp3='grads -p -g 541x700+1083+0 -c "set imprun imprun.gs"'
alias gradsl2='grads -l -g 840x649+846+0 -c "set imprun imprun.gs"'


#export PATH="$HOME/grads/bin:$PATH"
export PATH="/local/opt/grads/bin:$PATH"
export PATH="$PATH:/home/gliston/grads/wgrib/"
export PATH="$PATH:/home/gliston/grads/wgrib2/grib2/wgrib2/"

#HDFView
export PATH="$PATH:/home/gliston/HDFView/HDFView-2.11/HDFView-2.11.0-Linux/HDF_Group/HDFView/2.11.0/bin/"

# wget: the version required for NASA MERRA-2 downloads.
export PATH="/opt/wget-1.18/bin:$PATH"

# netcdf.
export PATH="/opt/netcdf-4.4.3/bin:$PATH"

# GDAL 3.x
export PATH="/opt/proj6/bin:/opt/gdal3/bin:$PATH"

# Panoply to look at .nc files.
export PATH="/home/gliston/panoply:$PATH"
alias panoply='panoply.sh'

#export PS1='$PWD/ -> '
export PS1='$PWD/\n Annabel -> '

alias ps1='ps --sort=start_time -e -o user,pid,stime,time,pmem,pcpu,comm | grep gliston'
alias ps2='ps1 | grep -v -e "sendmail" -e "konsole" -e "bash" -e "akonadi" -e "kde"'
alias ps3='ps2 | grep -e "vim" -e "a.out" -e "snowmodel" -e "grads" -e "less" -e "job"'

set -o vi

#echo
#echo "     hello Glen !!!!!!!!!!!!!!!"
#echo

# PGI fortran compiler and debugger license.
LM_LICENSE_FILE=/opt/pgi/license.dat
export LM_LICENSE_FILE

LD_LIBRARY_PATH="/usr/local/lib"
export LD_LIBRARY_PATH

# netcdf.
export LD_LIBRARY_PATH="/opt/netcdf-4.4.3/lib:$LD_LIBRARY_PATH"
LIBRARY_PATH="/opt/netcdf-4.4.3/lib"
export LIBRARY_PATH

export PYTHONPATH="/home/gliston/ecmwf_python/"

# put '.' at the head of my path.
export PATH=".:$PATH"

#LS_COLORS=$LS_COLORS:'*.f=1;35:'
#export LS_COLORS

# End Glen's additions.

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
if [[ -f ~/.dir_colors ]]; then
	eval `dircolors -b ~/.dir_colors`
else
	eval `dircolors -b /etc/DIR_COLORS`
fi

# Change the window title of X terminals 
case ${TERM} in
	xterm*|rxvt*|Eterm|aterm|kterm|gnome)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
		;;
	screen)
		PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
		;;
esac

