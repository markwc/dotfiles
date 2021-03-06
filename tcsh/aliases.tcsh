### Personal stuff

if ( $OSTYPE == "linux" ) then
    alias ls='\ls --color'
else
    alias ls='\ls -F'
endif

alias sv   'source activate'
alias la   'ls -a'
alias ll   'ls -al'
alias pu   'pushd'
alias po   'popd'
alias cl   'rm *.~*~'
alias gvim 'gvim -reverse'
alias org  'emacs --name Organizer ~/.org/organizer.org &'

alias clean 'find . | grep "~" | xargs rm -rf'

alias sa-mdl 'cd /project/mdl/mdl-vob; source activate'
alias org='emacs --name Organizer ~/.org/organizer.org &'

### Missouri stuff

alias pars 'sv mdl-prodl-101510'

### ClearCase stuff

alias ctlsb   'ct lsbl -stream $MYPROJ-integration@/project/tseries/genx-pvob | grep -v " stream: " | grep -v " component: "'
alias ctlsc   'ct lspriv | grep -v checkedout'
alias ctlse   'ct ls -r | grep eclipse'
alias ctlsv   'ct ls -r -view_only'
alias ctclean 'ct lspriv | grep -v checkedout | xargs rm -rf'

### xterms
# C:\cygwin\bin\run.exe -p /usr/X11R6/bin xterm  -fg green -bg black -display 127.0.0.1:0.0 -geom 128x64 -sl 200 -sb -T nemesis -e /usr/bin/ssh nemesis

alias xterm- 'xterm -ls -geom 128x32 -bg black -T $HOST'
alias xtermg 'xterm- -fg green &'
alias xtermr 'xterm- -fg red &'
alias xtermb 'xterm- -fg blue &'
alias xtermw 'xterm- -fg white &'
alias xtermy 'xterm- -fg yellow &'
alias xtermo 'xterm- -fg orange &'

alias kodiak 'ssh -Y kodiak'

### Project stuff

alias fs2l     'setenv MYPROJ fs2-lite; setenv MYVOB /project/common/csw-swf-vob'
alias fs2lsi   'setenv MYVIEW $MYPROJ-integration ; sv $MYVIEW'
alias fs2lsv   'setenv MYVIEW $MYPROJ-prodl-\!* ; sv $MYVIEW'
alias fs2lsa   'cd $MYVOB/$MYPROJ; source activate'

alias prebuild 'pushd $FS_COMPONENT/src; make prebuild; popd'
alias pb       'cd $PROJECT_BASE'

### All done

