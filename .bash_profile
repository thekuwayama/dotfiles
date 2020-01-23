source ~/.bashrc

# openssl
export PATH="/usr/local/opt/openssl/bin:$PATH"

# rbenv
eval "$(rbenv init -)"

# rust
export PATH="$HOME/.cargo/bin:$PATH"

# golang
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# history
export HISTTIMEFORMAT='%F %T '
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
shopt -u histappend
