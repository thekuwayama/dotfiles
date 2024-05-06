source ~/.bashrc

# brew
eval "$(/opt/homebrew/bin/brew shellenv)"
export HOMEBREW_NO_INSTALL_CLEANUP=TRUE

# rbenv
eval "$(rbenv init -)"

# nodenv
eval "$(nodenv init -)"

# direnv
eval "$(direnv hook bash)"

# rust
export PATH=$HOME/.cargo/bin:$PATH
source $HOME/.cargo/env

# golang
export GOROOT="$(brew --prefix golang)/libexec"
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# history
export HISTTIMEFORMAT='%F %T '
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
shopt -u histappend
export BASH_SILENCE_DEPRECATION_WARNING=1

# mcfly
eval "$(mcfly init bash)"
export MCFLY_FUZZY=true
export MCFLY_RESULTS=50

# git peckout
export PATH=$HOME/git-peckout:$PATH

# todo
eval "$(todo completion)"

# git
source "$(brew --prefix)/etc/bash_completion.d/git-prompt.sh"
source "$(brew --prefix)/etc/bash_completion.d/git-completion.bash"
export GIT_PAGER=delta

# mysql 5.7
export PATH="$(brew --prefix)/opt/mysql@5.7/bin:$PATH"

# utils
export PATH=$HOME/hack:$PATH

# openssl
export PATH="$(brew --prefix openssl)/bin:$PATH"
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:"$(brew --prefix openssl)/lib/pkgconfig"

# java
export JAVA_HOME=$(/usr/libexec/java_home -v 17)

# aqua
export PATH="$(aqua root-dir)/bin:$PATH"
