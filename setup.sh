#!/bin/sh

dir=`pwd`

echo 'Link dotfiles ...'
for x in .tmux.conf .emacs.d .bashrc .bash_profile .gemrc .gitconfig
do
  if [ -h "$HOME/${x}" ]; then
      printf "%-30s: %s\n" "$HOME/${x}" "Symlink exists"
  elif [ -e "$HOME/${x}" ]; then
      printf "%-30s: %s\n" "$HOME/${x}" "File exists"
  else
      cmd="ln -s ${dir}/${x} $HOME"
      echo ${cmd}
      ${cmd}
  fi
done

if ! command -v brew >/dev/null 2>&1; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
brew bundle check
brew bundle --verbose

echo 'Link diff-highlight ...'
sudo ln -s /usr/local/share/git-core/contrib/diff-highlight/diff-highlight /usr/local/bin/diff-highlight
