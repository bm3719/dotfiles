## -*- Mode: Shell-script -*-
## Bruce C. Miller
## FreeBSD and GNU/Linux version
## NOTE: To use as root, which is probably not a good idea to begin with:
##       - Remove . from PATH.
##       - Change compinstall filename path.
##       - Change WWW_HOME and ANT_HOME.
## TIPS: The <() construct lets you avoid having to use temporary files as
##       arguments to commands, e.g. `diff -y <(wc -l 1/*) <(wc -l 2/*)'

# Huge history file; great for C-r searching long ago used commands.
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000

# Dumb terminal config for Eshell/TRAMP.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

setopt appendhistory    # Save history from all zsh sessions.
setopt beep             # Gets overridden by screen's visual bell.
setopt extendedglob     # zsh pattern matching.
setopt nomatch
setopt nocheckjobs      # Don't complain about background jobs on exit.
setopt nohup            # Don't kill background jobs on exit.
setopt hist_expire_dups_first
# setopt printexitvalue   # Print exit value from jobs.
# pushd: Creates directory stacks that you can popd back to with cd -n, where n
# is an integer.
setopt auto_pushd

# Set a universal term type.
export TERM=xterm-256color
#export TERM=screen-256color

# Detect OS type.  Currently only using for the difference in `ls' flags.
platform='unknown'
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    platform='linux'
elif [[ "$unamestr" == 'FreeBSD' ]]; then
    platform='freebsd'
elif [[ "$unamestr" == 'OpenBSD' ]]; then
    platform='openbsd'
fi

# A righteous umask.
umask 027               # u=rw,g=r,o=

# Disable flow control handling (can mess up screen sessions if C-s or C-a s is
# hit).
stty -ixon -ixoff
# Stop background job output to stdout until foregrounded.  A message will be
# printed before the next prompt display.  Usually don't want this.
#stty tostop

## Keybindings

bindkey -e              # Emacs keybindings.
# Fix C-arrow keys.
bindkey "^[[1;5D" emacs-backward-word
bindkey "^[[1;5C" emacs-forward-word
# Fix home/end.
bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line
# [PageUp] - Up a line of history
if [[ -n "${terminfo[kpp]}" ]]; then
    bindkey -M emacs "${terminfo[kpp]}" up-line-or-history
    bindkey -M viins "${terminfo[kpp]}" up-line-or-history
    bindkey -M vicmd "${terminfo[kpp]}" up-line-or-history
fi
# [PageDown] - Down a line of history
if [[ -n "${terminfo[knp]}" ]]; then
    bindkey -M emacs "${terminfo[knp]}" down-line-or-history
    bindkey -M viins "${terminfo[knp]}" down-line-or-history
    bindkey -M vicmd "${terminfo[knp]}" down-line-or-history
fi
# Start typing + [Up-Arrow] - fuzzy find history forward
if [[ -n "${terminfo[kcuu1]}" ]]; then
    autoload -U up-line-or-beginning-search
    zle -N up-line-or-beginning-search

    bindkey -M emacs "${terminfo[kcuu1]}" up-line-or-beginning-search
    bindkey -M viins "${terminfo[kcuu1]}" up-line-or-beginning-search
    bindkey -M vicmd "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# Start typing + [Down-Arrow] - fuzzy find history backward
if [[ -n "${terminfo[kcud1]}" ]]; then
    autoload -U down-line-or-beginning-search
    zle -N down-line-or-beginning-search

    bindkey -M emacs "${terminfo[kcud1]}" down-line-or-beginning-search
    bindkey -M viins "${terminfo[kcud1]}" down-line-or-beginning-search
    bindkey -M vicmd "${terminfo[kcud1]}" down-line-or-beginning-search
fi
# [Home] - Go to beginning of line
if [[ -n "${terminfo[khome]}" ]]; then
    bindkey -M emacs "${terminfo[khome]}" beginning-of-line
    bindkey -M viins "${terminfo[khome]}" beginning-of-line
    bindkey -M vicmd "${terminfo[khome]}" beginning-of-line
fi
# [End] - Go to end of line
if [[ -n "${terminfo[kend]}" ]]; then
    bindkey -M emacs "${terminfo[kend]}"  end-of-line
    bindkey -M viins "${terminfo[kend]}"  end-of-line
    bindkey -M vicmd "${terminfo[kend]}"  end-of-line
fi
# [Delete] - delete forward
if [[ -n "${terminfo[kdch1]}" ]]; then
    bindkey -M emacs "${terminfo[kdch1]}" delete-char
    bindkey -M viins "${terminfo[kdch1]}" delete-char
    bindkey -M vicmd "${terminfo[kdch1]}" delete-char
else
    bindkey -M emacs "^[[3~" delete-char
    bindkey -M viins "^[[3~" delete-char
    bindkey -M vicmd "^[[3~" delete-char

    bindkey -M emacs "^[3;5~" delete-char
    bindkey -M viins "^[3;5~" delete-char
    bindkey -M vicmd "^[3;5~" delete-char
fi

## zstyle modifications.

# Hostname completions based on the contents of `~/.ssh/known_hosts' file.
# Requires `~/.ssh/config' change of: HashKnownHosts no
# Disabling this by default on multi-user machines, since anyone that gains
# read access can help themselves to a non-hashed list, and known_hosts is by
# default chmod 0644.
#hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\*}%%,*})
#zstyle ':completion:*:hosts' hosts $hosts

## Completion

autoload -Uz compinit
compinit
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'       # Case insensitive tab completion
zstyle ':completion:*' rehash true                              # automatically find new executables in path
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' menu select
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*:descriptions' format '%U%F{cyan}%d%f%u'
# Speed up completions.
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zcache
# Automatically load bash completion functions.
autoload -U +X bashcompinit && bashcompinit

## $PATH

# Adds a path to $PATH only if it exists and not already present.
add_to_path() {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        export PATH="$1:$PATH"
    fi
}
add_to_path "/bin"
add_to_path "/usr/bin"
add_to_path "/usr/sbin"
add_to_path "/usr/local/bin"
add_to_path "/usr/local/sbin"
add_to_path "/sbin"
add_to_path "$HOME/.local/bin"
add_to_path "$HOME/.yarn/bin"
add_to_path "$HOME/node_modules/.bin"
add_to_path "/var/lib/snapd/snap/bin"
add_to_path "$HOME/.ghcup/bin"
add_to_path "$HOME/.config/cabal/bin"
add_to_path "$HOME/.cargo/bin"
add_to_path "$HOME/bin"

## Plugins section: Enable fish style features

# Use syntax highlighting
if [ -f "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
# Use autosuggestion
if [ -f "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-autosuggestions.zsh" ]; then
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi
# Use history substring search
if [ -f "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-history-substring-search.zsh" ]; then
    source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
fi

# fzf
if [ -f "/usr/share/fzf/key-bindings.zsh" ]; then
    source /usr/share/fzf/key-bindings.zsh
fi
if [ -f "/usr/share/fzf/completion.zsh" ]; then
    source /usr/share/fzf/completion.zsh
fi
# Allow searching dot-files/dirs, ignoring any .git directories.
export FZF_DEFAULT_COMMAND="find . -name '.git' -prune -o -type f -print"
# Theme
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
  --color=fg:#d0d0d0,fg+:#d0d0d0,bg:#121212,bg+:#262626
  --color=hl:#5f87af,hl+:#5fd7ff,info:#afaf87,marker:#87ff00
  --color=prompt:#2d0f90,spinner:#af5fff,pointer:#af5fff,header:#87afaf
  --color=border:#262626,label:#aeaeae,query:#d9d9d9'

# AWS-related
# FETCH ROLES / POPULATE SAML-LOGIN
alias fetch-roles='saml-login aws-assume-role --show >> ~/.saml-login'

# SAML LOGINS
alias awsprodlogin='saml-login -c bpk-write@bpk-prod aws-assume-role'
alias awsnonprodlogin='saml-login -c bpk-write@bpk-nonprod aws-assume-role'
alias awsmgmtlogin='saml-login -c bpk-read-only@bpk-mgmt aws-assume-role'
alias awsrootlogin='saml-login -c bpk-read-only@bpk-root aws-assume-role'
alias awsmktplacelogin='saml-login -c bpk-read-only@bpk-mktplace aws-assume-role'

# AWS ALIASES
alias awsprod='aws --profile bpk-write@bpk-prod'
alias awsnonprod='aws --profile bpk-write@bpk-nonprod'
alias awsmgmt='aws --profile bpk-read-only@bpk-mgmt'
alias awsroot='aws --profile bpk-read-only@bpk-root'
alias awsmktplace='aws --profile bpk-read-only@bpk-mktplace'

export AWS_PROFILE="bpk-read-only@bpk-root"

# Autoload zsh modules when they are referenced.
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile

## Prompt

autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
# for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
#   eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
#   eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
#   (( count = $count + 1 ))
# done
# PR_NO_COLOR="%{$terminfo[sgr0]%}"
# PS1=`echo "[$PR_BLUE%n@%m:%c$PR_NO_COLOR]%# " | tr -d "$<2>"`

## Environmental variables

export SBCL_HOME=/usr/local/lib/sbcl
export FTPANONPASS=nobody@nodomain.nox
# Explicitly set TZ, which saves many system calls.
export TZ=:/etc/localtime
# `less' gets aliased later, so unaliasing here if it's aliased prevents errors
# when sourcing this file.
if [ -n "`alias -m \"*less*\"`" ]
then
    unalias less >& /dev/null
fi
# Set PAGER to `less' only if it exists.
if [ -x $(which less) ]
then
    export PAGER=less
else
    alias less=more
fi
export EDITOR='emacsclient -n'
export SVN_EDITOR='emacsclient'
export GIT_EDITOR='emacsclient'
export GREP_COLORS='mt=01;32'              # Set grep color to green (default red).
# export GREP_COLOR='01;32'                # Legacy version of the above.
# Set Lynx start page to bookmarks file.
export WWW_HOME="file://$HOME/lynx_bookmarks.html"
if [ -x $(which brave) ]
then
    export BROWSER='brave'
else
    export BROWSER='firefox'
fi
# Regionalization used by w3m.
export LC_ALL=en_US.UTF-8
# General locale.
export LANG=en_US.UTF-8
export CHARSET=UTF-8
export LC_CTYPE=en_US.UTF-8
# Java ecology stuff
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
#export JAVA_HOME=/usr/local/openjdk12
#export CLASSPATH=$CLASSPATH:.:/usr/local/share/java/classes/jline.jar
export ANT_HOME=/usr/local/share/java/apache-ant
# Suppress JVM warning on certain JDK 1.8 versions, at least when using lein.
if [[ $(java -version 2>&1 | grep 'version' | cut -d'"' -f2 | cut -d'.' -f1-2) == "1.8" ]]
then
    export LEIN_JVM_OPTS="-XX:TieredStopAtLevel=1"
fi
# Python-related stuff.
if [ -d /usr/local/share/doc/python2.7 ]
then
    export PYTHONDOCS=/usr/local/share/doc/python2.7
elif [ -d /usr/share/doc/python2.6 ]
then
    export PYTHONDOCS=/usr/share/doc/python2.6
else
    export PYTHONDOCS=/usr/share/doc/python/html
fi
# XDG related
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_STATE_HOME=$HOME/.local/state
export XDG_CACHE_HOME=$HOME/.cache
# ASDF
if [ -f "$HOME/.asdf/asdf.sh" ]; then
    source $HOME/.asdf/asdf.sh
fi
# mplayer
export MPLAYER_HOME="$XDG_CONFIG_HOME"/mplayer
# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Colorized manpages.
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

## Aliases

alias -g ...="../.."
alias -g ....="../../.."
alias -g .....="../../../.."
alias -g NUL="> /dev/null 2>&1"
alias f="finger"
if [[ $platform == 'linux' ]]
then
    alias ls='ls --color=auto --time-style=+%Y-%m-%d'
elif [[ $platform == 'freebsd' ]]
then
    alias ls='ls -G'
fi
alias ll="ls -al"
alias diff="colordiff"
alias grep="grep --colour=auto"
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
#alias s="screen -U"
#alias sd="screen -d"
#alias sr="screen -rx"
# alias tn="tmux new -s main -d && \
#           tmux new-window -t main && \
#           tmux send-keys -t main "em" Enter && \
#           tmux a -t main"
alias tn="tmux new -s main -d && \
          tmux new-window -t main && \
          tmux a -t main"
alias td="tmux detach -s main"
alias ta="tmux a -t main"
alias tl="tmux ls"
alias untar="tar xzfv "
alias tarnow="tar -acf "
alias unbz2="tar xvfj "
alias sui="sudo -i"
alias lup="sudo /usr/libexec/locate.updatedb"
alias rz="lrz -e"
alias sz="lsz -e"
alias ec="emacsclient -n"    # Use this with server-start.
alias ef="emacsclient -c"    # Creates a new frame with existing daemon.
alias gc="gnuclient-emacs"   # Use this with gnuserv-emacs.
alias em="emacs -nw"
alias xpdf="xpdf -z page"
if [ -x $(which less) ]
then
    alias less="less -RXF"
fi
alias grubup="sudo update-grub"
# alias fixpacman="sudo rm /var/lib/pacman/db.lck"
alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'
alias bc="bc -ql"
alias ttt="telnet yay.tim.org 5440"
alias tb="nc termbin.com 9999"
# Aliases for X11 forwarding.
alias rurxvt="urxvt -sl 10000 -ls -geometry 120x48"
alias hw='hwinfo --short'
# Disk Hogs Summary - disk usage by directory beneath this, sorted by
# size. (Directories that contain less than 100k are silently removed.)
alias dfs="du -kd 1000 \"\$@\" | awk '(\$1 >= 100)' | sort -rn"
# # Sort installed packages according to size in MB (expac must be installed.)
# alias big="expac -H M '%m\t%n' | sort -h | nl"
# # List -git packages.
# alias gitpkg='pacman -Q | grep -i "\-git"'

# kitty kittens
alias icat="kitty +kitten icat"

# # Get fastest mirrors
# alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
# alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
# alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
# alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

if [[ -x "$(command -v vim)" ]]; then
    alias vi=vim
fi

## Functions

# A find function, saving some typing for the most common find call.
ff() {
    find . -name \*$1\* -print;
}

# Make a directory and `cd` into it.
mkcd() {
    mkdir -p "$1" && cd "$1"
}

# Move up some number of directories.
up() {
    cd $(printf "%0.s../" {1..$1})
}

# Uncompress various file formats without having to remember the syntax for
# each.
extract() {
    if [ -f "$1" ]; then
        case "$1" in
            *.tar.bz2) tar xvjf "$1" ;;
            *.tar.gz) tar xvzf "$1" ;;
            *.tar.xz) tar xvJf "$1" ;;
            *.bz2) bunzip2 "$1" ;;
            *.rar) unrar x "$1" ;;
            *.gz) gunzip "$1" ;;
            *.tar) tar xvf "$1" ;;
            *.tbz2) tar xvjf "$1" ;;
            *.tgz) tar xvzf "$1" ;;
            *.zip) unzip "$1" ;;
            *.Z) uncompress "$1" ;;
            *.7z) 7z x "$1" ;;
            *.jar) jar -xf "$1" ;;
            *) echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

## oh-my-zsh: Borrowed from this project just what I think is useful.  There's
## a lot of stuff in oh-my-zsh that I don't agree with, and it's inconvenient
## to maintain a separate fork of a repo for my shell.  This keeps it all on
## one file.

# oh-my-zsh plugins: Just using git and svn.  Consider lein and github later.
# git.plugin.zsh
alias g='git'
compdef g=git
alias gst='git status'
compdef _git gst=git-status
alias gu='git pull --rebase'
compdef _git gl=git-pull
alias gup='git fetch && git rebase'
compdef _git gup=git-fetch
alias gp='git push -u origin master'
compdef _git gp=git-push
gdv() { git diff -w "$@" | view - }
compdef _git gdv=git-diff
alias gco='git checkout'
compdef _git gco=git-checkout
alias gcm='git checkout master'
alias gba='git branch -a'
compdef _git gba=git-branch
alias gcount='git shortlog -sn'
compdef gcount=git
alias glg='git log --stat --max-count=5'
compdef _git glg=git-log
alias glgg='git log --graph --max-count=5'
compdef _git glgg=git-log
alias ga='git add'
compdef _git ga=git-add
alias gm='git merge'
compdef _git gm=git-merge
alias grh='git reset HEAD'
alias grhh='git reset HEAD --hard'
# A super-useful alias to list all git directories under the current directory.
alias glist='find $PWD -type d -name .git | xargs -n 1 dirname'
# Git and svn mix.
alias git-svn-dcommit-push='git svn dcommit && git push github master:svntrunk'
compdef git-svn-dcommit-push=git
alias gsr='git svn rebase'
alias gsd='git svn dcommit'
# Returns the current branch name.
# Usage example: git pull origin $(current_branch)
function current_branch() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo ${ref#refs/heads/}
}
# These aliases take advantage of the previous function.
alias ggpull='git pull origin $(current_branch)'
compdef ggpull=git
alias ggpush='git push origin $(current_branch)'
compdef ggpush=git
alias ggpnp='git pull origin $(current_branch) && git push origin $(current_branch)'
compdef ggpnp=git

# svn.plugin.zsh
function svn_prompt_info {
    if [ $(in_svn) ]; then
        echo "$ZSH_PROMPT_BASE_COLOR$ZSH_THEME_SVN_PROMPT_PREFIX\
$ZSH_THEME_REPO_NAME_COLOR$(svn_get_repo_name)$ZSH_PROMPT_BASE_COLOR$ZSH_THEME_SVN_PROMPT_SUFFIX$ZSH_PROMPT_BASE_COLOR$(svn_dirty)$ZSH_PROMPT_BASE_COLOR"
    fi
}
function in_svn() {
    if [[ -d .svn ]]; then
        echo 1
    fi
}
function in_exclude() {
    # TODO: Exclude /usr/ports.
}
function svn_get_repo_name {
    if [ $(in_svn) ]; then
        svn info | sed -n 's/Repository\ Root:\ .*\///p' | read SVN_ROOT
        svn info | sed -n "s/URL:\ .*$SVN_ROOT\///p" | sed "s/\/.*$//"
    fi
}
function svn_get_rev_nr {
    if [ $(in_svn) ]; then
        svn info 2> /dev/null | sed -n s/Revision:\ //p
    fi
}
function svn_dirty_choose {
    if [ $(in_svn) ]; then
        s=$(svn status|grep -E '^\s*[ACDIM!?L]' 2>/dev/null)
        if [ $s ]; then
            echo $1
        else
            echo $2
        fi
    fi
}
function svn_dirty {
    svn_dirty_choose $ZSH_THEME_SVN_PROMPT_DIRTY $ZSH_THEME_SVN_PROMPT_CLEAN
}

# oh-my-zsh/lib/theme-and-appearance.zsh: Only relevant stuff used here.
# Apply theming defaults.
PS1="%n@%m:%~%# "
# Setup the prompt with pretty colors.
setopt prompt_subst

# oh-my-zsh/lib/git.zsh
# Get the name of the branch we are on.
function git_prompt_info() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}
# Checks if working tree is dirty.
parse_git_dirty() {
    local SUBMODULE_SYNTAX=''
    if [[ $POST_1_7_2_GIT -gt 0 ]]; then
        SUBMODULE_SYNTAX="--ignore-submodules=dirty"
    fi
    if [[ -n $(git status -s ${SUBMODULE_SYNTAX}  2> /dev/null) ]]; then
        echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
    else
        echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
    fi
}
# Checks if there are commits ahead from remote.
function git_prompt_ahead() {
    if $(echo "$(git log origin/$(current_branch)..HEAD 2> /dev/null)" | grep '^commit' &> /dev/null); then
        echo "$ZSH_THEME_GIT_PROMPT_AHEAD"
    fi
}
# Formats prompt string for current git commit short SHA.
function git_prompt_short_sha() {
    SHA=$(git rev-parse --short HEAD 2> /dev/null) && echo "$ZSH_THEME_GIT_PROMPT_SHA_BEFORE$SHA$ZSH_THEME_GIT_PROMPT_SHA_AFTER"
}

# Formats prompt string for current git commit long SHA.
function git_prompt_long_sha() {
    SHA=$(git rev-parse HEAD 2> /dev/null) && echo "$ZSH_THEME_GIT_PROMPT_SHA_BEFORE$SHA$ZSH_THEME_GIT_PROMPT_SHA_AFTER"
}
# Get the status of the working tree.
git_prompt_status() {
    INDEX=$(git status --porcelain 2> /dev/null)
    STATUS=""
    if $(echo "$INDEX" | grep '^?? ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_UNTRACKED$STATUS"
    fi
    if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
    elif $(echo "$INDEX" | grep '^M  ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_ADDED$STATUS"
    fi
    if $(echo "$INDEX" | grep '^ M ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
    elif $(echo "$INDEX" | grep '^AM ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
    elif $(echo "$INDEX" | grep '^ T ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_MODIFIED$STATUS"
    fi
    if $(echo "$INDEX" | grep '^R  ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_RENAMED$STATUS"
    fi
    if $(echo "$INDEX" | grep '^ D ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
    elif $(echo "$INDEX" | grep '^AD ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_DELETED$STATUS"
    fi
    if $(echo "$INDEX" | grep '^UU ' &> /dev/null); then
        STATUS="$ZSH_THEME_GIT_PROMPT_UNMERGED$STATUS"
    fi
    echo $STATUS
}
# Compare the provided version of git to the version installed and on path.
# Prints 1 if input version <= installed version.
# Prints -1 otherwise.
function git_compare_version() {
    local INPUT_GIT_VERSION=$1;
    local INSTALLED_GIT_VERSION
    INPUT_GIT_VERSION=(${(s/./)INPUT_GIT_VERSION});
    INSTALLED_GIT_VERSION=($(git --version));
    INSTALLED_GIT_VERSION=(${(s/./)INSTALLED_GIT_VERSION[3]});
    for i in {1..3}; do
        if [[ $INSTALLED_GIT_VERSION[$i] -lt $INPUT_GIT_VERSION[$i] ]]; then
            echo -1
            return 0
        fi
    done
    echo 1
}
# This is unlikely to change so make it all staticly assigned.
POST_1_7_2_GIT=$(git_compare_version "1.7.2")
# Clean up the namespace slightly by removing the checker function.
unset -f git_compare_version

# oh-my-zsh theme: A modified dpoggi.zsh-theme.
if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="blue"; fi
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
PROMPT='%{$fg[$NCOLOR]%}%n%{$reset_color%}@%{$fg[blue]%}%m\
%{$reset_color%}:%{$fg[magenta]%}%~\
$(git_prompt_info)\
$(svn_prompt_info)\
%{$fg[blue]%}%(!.#.»)%{$reset_color%} '
PROMPT2='%{$fg[red]%}\ %{$reset_color%}'
RPS1='${return_code}'
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}○%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}⚡%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[yellow]%})%{$reset_color%}"
ZSH_THEME_SVN_PROMPT_PREFIX="%{$fg[yellow]%}("
ZSH_THEME_SVN_PROMPT_SUFFIX="%{$fg[yellow]%})%{$reset_color%}"
ZSH_THEME_SVN_PROMPT_DIRTY="%{$fg[red]%}⚡%{$reset_color%}"
ZSH_THEME_SVN_PROMPT_CLEAN="%{$fg[green]%}○%{$reset_color%}"

## Final config
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Run neofetch if session is interactive.
if [[ $- == *i* && $(which neofetch &>/dev/null) ]]; then
    #if test -o interactive && which neofetch &>/dev/null; then
    neofetch
fi
