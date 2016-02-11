cd ~/.emacs.d/
touch site-specific.el
mkdir lisp

cd ~/.emacs.d/lisp
git clone https://github.com/markhepburn/tags-view.git
git clone https://github.com/company-mode/company-mode.git

sudo apt-get install org-mode
sudo apt-get install pyflakes
sudo cp ~/.dotfiles/emacs/pychecker /usr/local/bin


