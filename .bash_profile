source ~/.bashrc

# openssl
export PATH="/usr/local/opt/openssl/bin:$PATH"

# rbenv
export PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init -)"

# rust
source $HOME/.cargo/env
export RUSTC_WRAPPER=$(which sccache)

# golang
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# nodenv
export PATH=$PATH:$HOME/.nodenv/bin
eval "$(nodenv init -)"

# scala
export PATH="/usr/local/opt/scala@2.11/bin:$PATH"

# history
export HISTTIMEFORMAT='%F %T '
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
shopt -u histappend
export BASH_SILENCE_DEPRECATION_WARNING=1

# spark
export JAVA_HOME=`/usr/libexec/java_home -v "1.8"`
export PATH=${JAVA_HOME}/bin:${PATH}
export HADOOP_HOME=/opt/hadoop
export HADOOP_COMMON_LIB_NATIVE_DIR=$HADOOP_HOME/lib/native
export HADOOP_OPTS="-Djava.library.path=$HADOOP_HOME/lib"
export SPARK_HOME=/opt/spark
export SPARK_DIST_CLASSPATH=$($HADOOP_HOME/bin/hadoop classpath)

# gcp
source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc
source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc

# git
source /usr/local/etc/bash_completion.d/git-prompt.sh
source /usr/local/etc/bash_completion.d/git-completion.bash

# mcfly
eval "$(mcfly init bash)"
export MCFLY_FUZZY=true
export MCFLY_RESULTS=50

# argo
export ARGO_SERVER=localhost:2746

# krew
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

# git peckout
export PATH=$HOME/git-peckout:$PATH
