source ~/.bashrc
eval "$(rbenv init -)"
export PATH="/usr/local/opt/openssl/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
