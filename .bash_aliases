#set -euo pipefail

path_virt_env(){
	local LOC_ENV=
	if [ ${#CONDA_DEFAULT_ENV} -gt 0 ];  then
		LOC_ENV=$CONDA_DEFAULT_ENV
	elif [ ${#VIRTUAL_ENV} -gt 0 ]; then
		LOC_ENV=$VIRTUAL_ENV
	fi
	echo $LOC_ENV 2> /dev/null
}

function read_yn {
	MESSAGE="Choose: "
	TIMEOUT=5
	DEFAULT_CHOICE="y"
  case $# in
    0) printf "Need at least one argument\n args: [named reference to return result, custom message, custom timeout, default choice]";exit 1 ;;
    1)  ;;
    2) MESSAGE=$2 ;;
    3) MESSAGE=$2;TIMEOUT=$3 ;;
    4) MESSAGE=$2;TIMEOUT=$3;DEFAULT_CHOICE=$4 ;;
    *) printf "Invalid number of arguments\n args: [named reference to return result, custom message, custom timeout, default choice]";exit 1 ;;
  esac
  declare waitreadyn=""
  declare -n RESULT=$1
  local LOC_RES=""
  for (( i=$TIMEOUT; i>=0; i--)); do
      printf "\r${MESSAGE} ('Y/n' in ${i}s) "
      read -s -n 1 -t 1 waitreadyn
      if [ $? -eq 0 ]
      then
        break
      fi
  done
  case $waitreadyn in
    y|Y) LOC_RES="y" ;;
    n|N) LOC_RES="n"  ;;
    *) LOC_RES="$DEFAULT_CHOICE" ; printf "\nInvalid answer. Using default choice \"$DEFAULT_CHOICE\" instead\n" ;;
  esac
  RESULT=$LOC_RES
}

get_manpager(){
	if [ `command -v most` ]; then
		echo most 2> /dev/null
		export MANPAGER=most
	else
		echo more 2> /dev/null
		export MANPAGER=more
	fi
}

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls -F --color'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep -i --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
llfunc(){
	case $# in
		1) ls -ahlF --group-directories-first | less -p "$1" -R ;;
		2) ls -ahlF --group-directories-first "$1" | less -p "$2" -R ;;
		*) ls -ahlF --group-directories-first | less -R ;;
	esac
}
alias ll='llfunc'

alias lu='du -sch .[!.]* * 2>/dev/null | sort -h'
alias lf='find -maxdepth 1 -type f -exec du -sch {} + 2>/dev/null | sort -h'
alias lt='ls -lt | sed -e "s/^.\{10\}[ ]*[0-9]*[ ]*\w*[ ]*\w*[ ]*[0-9]*//g"'
alias la='ls -A'
alias l='ls -CF'
alias clc='clear; cdls .'

alias mv='mv -v'
alias rm='rm -v'
alias cp='cp -iv'
alias ln='ln -v'

# why would you ever not use -w on vwstat?
alias vmstat='vmstat -w '

alias less='less -R'

if [ ${WORKON_HOME} ]; then
	alias mkv='mkvirtualenv'
	alias rmv='rmvirtualenv'
	alias lsv='lsvirtualenv'
	alias showv='showvirtualenv'
	alias cpv='cpvirtualenv'
	alias allv='allvirtualenv'
	alias wkon='source $(path_virt_env)/bin/activate'
	alias cdv='cdvirtualenv'
	alias cdvs='cdsitepackages'
	alias lsvs='lssitepackages'
	alias add2v='add2virtualenv'
	alias togvg='toggleglobalsitepackages'
	alias mkp='mkproject'
	alias bindv='setvirtualenvproject'
	alias cdp='cdproject'
	alias wipev='wipeenv'
	alias vcmds='virtualenvwrapper'
	alias stopv='deactivate'
fi

mkcondaenv(){
	NAME=$1
	conda create -n ${NAME} python=3.7
}
alias mkc='mkcondaenv'

rmcondaenv(){
	NAME=$1
	conda remove -n ${NAME} --all
}
alias rmc='rmcondaenv'

alias wkonc='conda activate'
alias stopc='conda deactivate'
alias lsc='conda env list'
alias lscp='conda list'

# perform 'ls' after 'cd' if successful.
cdls() {
  if [ $# -eq 0 ]; then
    FARG=${HOME}
  else
    FARG="$*"
  fi
  builtin cd "$FARG"
  SUCCESS=$?
  if [ "$SUCCESS" -eq 0 ]; then
    #clear
    ls -ACF --group-directories-first
		#if [[ -x stat && -x du ]]; then
    #if [[ ${PWD} != ~ &&  $(stat -c '%U' "$PWD") == "${USER}" ]]; then
    #  	#du -sh 2>/dev/null
		#  DUS=`find -maxdepth 1 -name '*' -exec du -sh {} + 2>/dev/null`
		#	NUMF=`find -maxdepth 1 -type f | wc -l 2> /dev/null`
		#	NUMD=`find -maxdepth 1 -type d | wc -l 2> /dev/null`
		#	echo ${DUS} in ${NUMF} files and $((NUMD-1)) folders
    #fi
		#fi
  fi
}
alias cd='cdls'

cdun(){
	if [ $# -eq 0 ]; then
		cdls ..
	else
		if [[ $1 == ?(-)+([0-9]) ]]; then
			local x=''
			for i in $(seq ${1:-1}); do
				x="$x../"
			done
			cdls $x
		fi
	fi
}
alias ..='cdun'

alias x='exit'

the_pag(){
  ps aux | grep -i $1 | grep -v grep
}
alias pag='the_pag '

alias c+x='chmod +x '

alias c-x='chmod -x '

alias mown='sudo chown -R $USER:users '

vivg(){
	cd
	vivado -jvm -patch-module=java.desktop=/data/work/stephen/AR72614_vivado_2019_2_preliminary_rev2/vivado/lib/classes/AR72614/AR72614.jar
}

alias vivadog='vivg'
alias filterv='filter.pl'
alias chip='$HOME/Programs/chipvault_src_files/cv.pl'

alias stephen='cd /data/work/stephen'
alias stephen2='cd /data/work/stephen2'
alias stephen3='cd /data/work/stephen3'
alias stephen4='cd /data/work/stephen4'
alias stephen5='cd /data/work/stephen5'
alias fpga='cd /data/work/stephen/fpga-firmware'
alias fpga2='cd /data/work/stephen2/fpga-firmware'
alias fpga3='cd /data/work/stephen3/fpga-firmware'
alias fpga4='cd /data/work/stephen4/fpga-firmware'
alias fpga5='cd /data/work/stephen5/fpga-firmware'
alias testing='cd /data/work/stephen/exanic-software/testing/automated/'

viv_def(){
	/usr/local/bin/stephen/run-vivado-2018.3.sh $@
}
#alias vivado='viv_def'
alias src-viv='source /data/Xilinx/Vivado/2018.3/.settings64-Vivado.sh || source /data/xilinx/Vivado/2018.3/.settings64-Vivado.sh && which vivado'
alias src-viv192='source /data/Xilinx/Vivado/2019.2/.settings64-Vivado.sh || source /data/xilinx/Vivado/2019.2/.settings64-Vivado.sh && which vivado'
alias src-viv202='source /data/Xilinx/Vivado/2020.2/.settings64-Vivado.sh || source /data/xilinx/Vivado/2020.2/.settings64-Vivado.sh && which vivado'

verilog_template(){
	local TEMPLATE=
	if [ -f ${HOME}/.vim/templates/verilog_template.v ]; then
		TEMPLATE="${HOME}/.vim/templates/verilog_template.v"
	fi
 	echo $TEMPLATE 2> /dev/null
}

vim_template(){
	for VAR in $@; do
		EXT=`echo ${VAR} | cut -d. -f 2`
		BASE=`echo ${VAR} | cut -d. -f 1`
		if [ ! -f `pwd`/${VAR} ]; then
			case $EXT in
				"v") echo "creating new verilog file from template" && cp $(verilog_template ${BASE}) `pwd`/${VAR} && sed -i -e "s|__NAME__|${BASE}|" `pwd`/${VAR} ;;
			esac
		fi
	done
	vim $@
}
#alias vim='vim_template'

tidy_up_ip_folders(){
  if [ -d ./ip/ ]; then
    pushd ip
    for f in `ls --color=none -d *`; do
      pushd $f
      `rm -rf !(*.xci|*.tcl)` &> /dev/null
      popd
    done
    popd
  fi
}

tidy_up_vivados_mess(){
	rm vivado*
	rm -rf .Xil
	rm -rf hd_visual
	rm -rf ip_user_files
	rm -rf managed_ip_project
	rm .XIC.lock
	rm -rf .Xilinx
	rm -f webtalk*
	rm -f usage_statistics_webtalk*
	rm -f tight_setup_hold_pins.txt
	rm -f iter_*
  tidy_up_ip_folders
}
alias tv='tidy_up_vivados_mess'

alias khup='kill -s SIGTERM `lsof -t nohup.out`'

alias em='emacsclient -cn -a "vim" "$@"'

alias dmacs='emacs --daemon'
alias kmacs='killall emacs'

alias gpgp='gpg -c --no-symkey-cache --cipher-algo AES256'

hex2dec(){
  hex=$1
  echo "ibase=16; ${hex^^}" | bc
}
alias h2d='hex2dec'

# want to add deletion of non-xci-files in ip folder to tv alias
# find ./ip/ -mindepth 2 \! -name *.xci -delete