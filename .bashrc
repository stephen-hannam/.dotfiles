#~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

if [[ $- == *i* ]]; then
  if [[ -x `which zsh 2>/dev/null` ]]; then
    zsh;
    exit 0;
  fi
fi

if [[ -x `which most &>/dev/null` ]]; then
	echo "changing manpager to most"
	export MANPAGER=most
else
	export MANPAGER=less
fi

if [ ${DISPLAY+x} ]; then
	xset -b
fi

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# load customization for key bindings
if [ -f ~/.Xmodmap ]; then
  xmodmap ~/.Xmodmap &>/dev/null
fi

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

join_by(){ local IFS="$1"; shift; echo "$*"; }

parse_git_branch() {
 git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

parse_virt_env(){
	local LOC_ENV="_"
	if [ ${#CONDA_DEFAULT_ENV} -gt 0 ];  then
		LOC_ENV=$CONDA_DEFAULT_ENV
	elif [ ${#VIRTUAL_ENV} -gt 0 ]; then
		LOC_ENV=(`basename $VIRTUAL_ENV`)
	fi
	echo $LOC_ENV 2> /dev/null
}

parse_path(){
  W=`pwd`
  #W=${W/$HOME/\~}
  #CCNT=`pwd | wc -m`
  #if [ $W == "~" ]; then
  #  CCNT=$(($CCNT-9))
  #fi
  #if [ $CCNT -gt 80 ]; then
  #  IFS='/' read -ra DIRS <<< "$W"
  #  RMLEN=$((${#DIRS[@]}-2))
  #  W=$(join_by / "${DIRS[0]}" "..${RMLEN}.." "${DIRS[-1]}")
  #fi
  #if [ -z "$W" ]; then
  #  W='/'
  #fi
  echo $W 2> /dev/null
}

#PS1='\n\[\e[32;1m\]\342\224\214\342\224\200(\[\e[34;1m\]$(parse_path)\[\e[32;1m\])-(\[\e[37;1m\]\h\[\e[32;1m\])\[\e[32;1m\] $(parse_git_branch)\n\[\e[32;1m\]\342\224\224\342\224\200(\[\e[36;1m\]$(parse_virt_env)\[\e[32;1m\])-(\[\e[35;1m\]\u\[\e[32;1m\])\$ \[\e[0m\]'
PS1='\n\[\e[32;1m\]\342\224\214\342\224\200\[\e[34;1m\]$(parse_path)\[\e[32;1m\] $(parse_git_branch)\n\[\e[32;1m\]\342\224\224\342\224\200\[\e[35;1m\]\u\[\e[32;1m\]\[\e[34;1m\]@\[\e[37;1m\]\h\[\e[32;1m\]$ \[\e[0m\]'
export VIRTUAL_ENV_DISABLE_PROMPT=1

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ -x ~/.local/bin/virtualenvwrapper.sh ]; then
	if [ -d $HOME/.virtualenvs/py3 ]; then
	  if [ ${WORKON_HOME} ]; then
	    workon py3
	  else
		  source $HOME/.virtualenvs/py3/bin/activate
			export WORKON_HOME=$HOME/.virtualenvs
			export VIRTUALENVWRAPPER_PYTHON=$WORKON_HOME/py3/bin/python
			source ~/.local/bin/virtualenvwrapper.sh 1> /dev/null
			#export VIRTUALENVWRAPPER_VIRTUALENV=$WORKON_HOME/py3/bin/virtualenv
	  fi
	else
	  if [ -d $HOME/.virtualenvs/py2 ]; then
	    if [ ${WORKON_HOME} ]; then
	      workon py2
	    else
	      source $HOME/.virtualenvs/py2/bin/activate
				export WORKON_HOME=$HOME/.virtualenvs
				export VIRTUALENVWRAPPER_PYTHON=$WORKON_HOME/py2/bin/python
				source ~/.local/bin/virtualenvwrapper.sh 1> /dev/null
				#export VIRTUALENVWRAPPER_VIRTUALENV=$WORKON_HOME/py2/bin/virtualenv
	    fi
	  fi
	fi
fi

# end
#if [ ! -S $HOME/.ssh/ssh_auth_sock ]; then
#  eval `ssh-agent`
#  ln -sf "$SSH_AUTH_SOCK" $HOME/.ssh/ssh_auth_sock
#fi
#export SSH_AUTH_SOCK=$HOME/.ssh/ssh_auth_sock
#ssh-add -l > /dev/null || ssh-add

export XILINXD_LICENSE_FILE=2100@exabuild001.cisco.com

export PIPENV_IGNORE_VIRTUALENVS=1

export PATH=$HOME/.local/bin:$HOME/.opam/default/bin:$PATH

export ADS_HOSTS="2822 5399 8174 7891 5706 893 6886 9066 7148 5856"
export ADS_HOSTS_192="2822 5399 8174 7891 5706"
export ADS_HOSTS_202="893 6886 9066 7148 5856"

function sshagent_findsockets {
    find /tmp -uid $(id -u) -type s -name agent.\* 2>/dev/null
}

function sshagent_testsocket {
    if [ ! -x "$(which ssh-add)" ] ; then
        echo "ssh-add is not available; agent testing aborted"
        return 1
    fi

    if [ X"$1" != X ] ; then
        export SSH_AUTH_SOCK=$1
    fi

    if [ X"$SSH_AUTH_SOCK" = X ] ; then
        return 2
    fi

    if [ -S $SSH_AUTH_SOCK ] ; then
        ssh-add -l > /dev/null
        if [ $? = 2 ] ; then
            echo "Socket $SSH_AUTH_SOCK is dead!  Deleting!"
            rm -f $SSH_AUTH_SOCK
            return 4
        else
            echo "Found ssh-agent $SSH_AUTH_SOCK"
            return 0
        fi
    else
        echo "$SSH_AUTH_SOCK is not a socket!"
        return 3
    fi
}

function sshagent_init {
    # ssh agent sockets can be attached to a ssh daemon process or an
    # ssh-agent process.

    AGENTFOUND=0

    # Attempt to find and use the ssh-agent in the current environment
    if sshagent_testsocket ; then AGENTFOUND=1 ; fi

    # If there is no agent in the environment, search /tmp for
    # possible agents to reuse before starting a fresh ssh-agent
    # process.
    if [ $AGENTFOUND = 0 ] ; then
        for agentsocket in $(sshagent_findsockets) ; do
            if [ $AGENTFOUND != 0 ] ; then break ; fi
            if sshagent_testsocket $agentsocket ; then AGENTFOUND=1 ; fi
        done
    fi

    # If at this point we still haven't located an agent, it's time to
    # start a new one
    if [ $AGENTFOUND = 0 ] ; then
        eval `ssh-agent`
    fi

    # Clean up
    unset AGENTFOUND
    unset agentsocket

    # Finally, show what keys are currently in the agent
    ssh-add -l || ssh-add $HOME/.ssh/id_rsa
}
alias sagent="sshagent_init"

sshagent_init

[ -f $HOME/.local/share/rcs/.bash_extras ] && source $HOME/.local/share/rcs/.bash_extras || echo "No local aliases to source for $HOST"

cd ~
