black=$(tput setaf 0)
red=$(tput setaf 1)
green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
purple=$(tput setaf 5)
cyan=$(tput setaf 6)
bold=$(tput bold)
standout=$(tput smso)
underline=$(tput smul)
dim=$(tput dim)
reset=$(tput sgr0)

if [ -s "$HOME/.dotfiles/git/git-prompt.sh" ]
then
  source "$HOME/.dotfiles/git/git-prompt.sh"
  GIT_PS1_SHOWDIRTYSTATE=true
  GIT_PS1_SHOWSTASHSTATE=true
  GIT_PS1_SHOWUNTRACKEDFILES=true
  GIT_PS1_SHOWUPSTREAM=verbose
  GIT_PS1_SHOWCOLORHINTS=true
  PROMPT_COMMAND='__git_ps1 "\[$black\]\u@\h \[$reset\]\[$yellow$bold\]\w\[$reset\]" " \$ " " [%s]"'
fi
