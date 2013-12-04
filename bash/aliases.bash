alias sv='source activate'
alias ls='\ls -F'
if [ $OSTYPE == "linux" ]; then
    alias ls='\ls --color'
else
    alias ls='\ls -F'
fi
alias la='ls -a'
alias ll='ls -al'
alias pu='pushd'
alias po='popd'
alias cl='rm *.~*~'
alias rg='find . | xargs grep -i $1'
alias auth='source ~/.dotfiles/bash/ssh-agent.bash.manual'
alias org='emacs --name Organizer ~/.org/organizer.org &'
