#!/bin/bash

function print_info() {
    # white
    printf "\033[0;37m --- $*\033[0m\n"
}

function print_warn() {
    # yellow
    printf "\033[0;33m [!] $*\033[0m\n"
}

function print_error() {
    # red
    printf "\033[0;31m [x] $*\033[0m\n"
}

function print_success() {
    # cyan
    printf "\033[0;36m [o] $*\033[0m\n"
}

function check_os() {
    if [ "$(uname)" != "Darwin" ]; then
        print_error "Sorry, this script supports only Darwin..."
        exit 1
    fi
}

function check_pwd() {
    if [ "$(pwd)" != "$HOME" ]; then
        print_error "Sorry, cd $HOME..."
        exit 1
    fi
}

function change_login_shell() {
    if [[ "$SHELL" != "/bin/bash" ]]; then
        chsh -s /bin/bash
    fi
}

DOTFILES_PATH="$(pwd)/dotfiles"

function download_dotfiles() {
    print_info "Download dotfiles"

    if [ -d $DOTFILES_PATH ]; then
        print_warn "dotfiles: already exists"
    else
        print_success "Downloading dotfiles..."
        if command -v git >/dev/null 2>&1; then
            git clone git@github.com:thekuwayama/dotfiles.git
        else
            curl -sL https://github.com/thekuwayama/dotfiles/archive/main.tar.gz | tar xz
            mv dotfiles-main dotfiles
        fi
        print_success "Success to download dotfiles"
    fi
}

function link_dotfiles() {
    print_info "Link dotfiles"

    for x in .bashrc .bash_profile .emacs.d .gemrc .gitconfig .irbrc .tmux.conf git-peckout
    do
        if [ -h "$HOME/${x}" ]; then
            print_warn "$HOME/${x}: already linked"
        elif [ -e "$HOME/${x}" ]; then
            print_warn "$HOME/${x}: file exists"
        else
            ln -s ${DOTFILES_PATH}/${x} $HOME
            print_success "Success to link ${DOTFILES_PATH}/${x}"
        fi
    done
}

function brew_bundle() {
    print_info "Bundle Install brew"

    if ! command -v brew >/dev/null 2>&1; then
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    else
        print_warn "brew: already exists"
    fi

    brew bundle check --file "${DOTFILES_PATH}/Brewfile"
    brew bundle --verbose --file "${DOTFILES_PATH}/Brewfile"
}

function link_diffhighlight() {
    print_info "Link diff-highlight"

    if ! command -v diff-highlight >/dev/null 2>&1; then
        if [ ! -e /usr/local/share/git-core/contrib/diff-highlight/diff-highlight ]; then
            # /usr/local/share/git-core is installed by `brew install git`
            print_warn "Sorry, require diff-highlight..."
            exit
        fi
        sudo ln -s /usr/local/share/git-core/contrib/diff-highlight/diff-highlight /usr/local/bin/diff-highlight
        print_success "Success to link diff-highlight"
    else
        print_warn "diff-highlight: already linked"
    fi
}

function todo_completion() {
    if [ ! -f "$HOME/.todo.bash" ]; then
        wget https://raw.githubusercontent.com/thekuwayama/todo/main/todo.bash -O $HOME/.todo.bash
    fi
}

function main() {
    check_os
    check_pwd

    download_dotfiles
    link_dotfiles
    brew_bundle
    link_diffhighlight
    todo_completion

    change_login_shell
}

main
