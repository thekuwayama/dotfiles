source ~/.bashrc

# openssl
export PATH="/usr/local/opt/openssl/bin:$PATH"

# rbenv
eval "$(rbenv init -)"

# rust
source $HOME/.cargo/env

# golang
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# history
export HISTTIMEFORMAT='%F %T '
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
shopt -u histappend
export BASH_SILENCE_DEPRECATION_WARNING=1

# spark
export JAVA_HOME=`/usr/libexec/java_home -v "1.8"`
export PATH=${JAVA_HOME}/bin:${PATH}
export HADOOP_HOME=/opt/hadoop
export SPARK_HOME=/opt/spark
export SPARK_DIST_CLASSPATH=$($HADOOP_HOME//bin/hadoop classpath)

# gcp
export GOOGLE_APPLICATION_CREDENTIALS="$HOME/.google_application_credentials"
