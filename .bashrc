PS1='[\u \W$(__git_ps1 " (%s)")]\$ '

alias cgrep='grep --color=always'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias less='less -R'

if [[ -x `which colordiff` ]]; then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

alias ls='ls -G'
alias ll='ls -alhG'
alias la='ls -A'

alias emacs='emacs -nw'

alias rm='set -f; rmrm'
function rmrm() {
    local ch
    echo $@ | grep -Gq "^/$\| /$\|^/ \| / "
    if [ $? -eq 0 ]; then
	set +f
	echo "DONOT execute 'rm /'"
	return 1
    fi

    echo $@ | grep -q "*"
    if [ $? -eq 0 ]; then
	set +f
	ls $@
	if [ $? -ne 0 ]; then
	    return 0
	fi
	echo -n "are you sure? [Y/n] "
	while : ; do
	    read ch
	    if [ "${ch}" = "Y" ]; then
		command rm $@
		return 0
	    elif [ "${ch}" = "n" ] || [ "${ch}" = "N" ]; then
		return 1
	    else
		echo -n "[Y/n]"
	    fi
	done
    else
	command rm $@
	return 0
    fi
}

alias date='LANG="C" date'
