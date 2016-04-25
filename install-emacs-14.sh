#/bin/bash

cd ~/.emacs.d/
touch site-specific.el
mkdir lisp

cd ~/.emacs.d/lisp
git clone https://github.com/markhepburn/tags-view.git
git clone https://github.com/company-mode/company-mode.git
wget https://elpa.gnu.org/packages/cl-lib-0.4.el
ln -fs cl-lib-0.4.el cl-lib.el
wget http://repo.or.cz/emacs.git/blob_plain/HEAD:/lisp/emacs-lisp/pcase.el

sudo apt-get -y install global
sudo apt-get -y install org-mode
sudo apt-get -y install pyflakes
sudo cp ~/.dotfiles/emacs/pychecker /usr/local/bin

sudo apt-get -y install doxygen
sudo apt-get -y install clang-format-3.4
sudo ln -fs /usr/bin/clang-format--3.4 /usr/bin/clang-format
sudo ln -fs /usr/bin/clang-format-diff--3.4 /usr/bin/clang-format-diff
