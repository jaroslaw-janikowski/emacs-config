# aktualizacja
apt update -y
apt upgrade -y
apt autoremove -y

# desktop
apt install -y xorg openbox pcmanfm pavucontrol p7zip-full stterm clangd gnupg2 unifont gdb clzip sqlite3 sqlite3-doc brightnessctl dmenu
su - work -- <<EOF
mkdir -p ~/.config/openbox
echo 'exec openbox-session' >> ~/.xinitrc &&
echo '[ "$(tty)" = "/dev/tty1" ] && exec startx' >> ~/.profile
echo 'brightnessctl set 20% &' >> ~/.config/openbox/autostart
echo 'pcmanfm --desktop &' >> ~/.config/openbox/autostart
EOF

# przeglądarki internetowe do webdevu
su - work -- <<EOF
cd /tmp
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
EOF
apt install -y /tmp/google-chrome-stable_current_amd64.deb
apt install -y firefox-esr

# spelling tools
apt install -y aspell aspell-pl aspell-en

# git setup
apt install -y git git-gui

# ufw setup
apt install -y ufw
systemctl enable ufw
su - root -- <<EOF
ufw deny telnet
ufw deny vnc
EOF

# emacs
apt build-dep -y emacs
apt install -y libtree-sitter-dev libsqlite3-dev silversearcher-ag
su - work -- <<EOF
rm -rf ~/.emacs.d
rm ~/.emacs
git clone --depth 1 https://github.com/jaroslaw-janikowski/emacs-config ~/.emacs.d
sed -i "s/;; (require 'work)/(require 'work)/"
git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git ~/workspace/emacs
cd ~/workspace/emacs && make bootstrap
EOF
cd /home/work/emacs && make install
su - work -- <<EOF
emacs -Q --script ~/.emacs.d/install/setup-emacs.el
emacs -Q --script ~/.emacs.d/install/setup-emacs-work.el
echo 'emacs &' >> ~/.config/openbox/autostart
EOF

# nsis
apt install -y nsis nsis-doc

# python setup
apt install -y python3-venv python3-pip python3-debugpy python3-pylsp

# php
apt install -y php phpunit composer php-xml php-sqlite3

su - work -- <<EOF
mkdir -p ~/.local/bin && cd ~/.local/bin &&  wget https://github.com/phpactor/phpactor/releases/latest/download/phpactor.phar && chmod a+x ./phpactor.phar
mv ./phpactor.phar ./phpactor
EOF

# node.js
apt install -y nodejs npm
npm install -g typescript-language-server typescript

# podman setup
apt install -y podman podman-compose
su - work -- <<EOF
podman pull docker.io/php
podman pull docker.io/mysql
podman pull docker.io/wordpress
podman pull docker.io/python
podman pull docker.io/postgres
EOF

# ruby, ruby on rails, tools
apt install -y build-essential rustc libssl-dev libyaml-dev zlib1g-dev libgmp-dev curl

su - work -- <<EOF
cd ~/.local/bin
wget https://mise.run -O ~/.local/bin/mise
chmod +x ~/.local/bin/mise
echo 'eval "$(~/.local/bin/mise activate)"' >> ~/.bashrc
source ~/.bashrc
source ~/.bashrc
mise use -g ruby@3
gem install rails
gem install solargraph
gem install solargraph-rails
EOF

# Free Pascal Compiler
apt install -y fpc
su - work -- <<EOF
cd ~/workspace
git clone --depth 1 https://gitlab.com/freepascal.org/fpc/source freepascal
cd ./freepascal
make clean
make all
EOF
cd /home/work/workspace/freepascal && make install

#  Lazarus IDE
su - work -- <<EOF
cd ~/workspace
git clone --depth 1 https://gitlab.com/freepascal.org/lazarus/lazarus.git lazarus
cd ./lazarus
make
EOF
cd /home/work/workspace/lazarus && make install

# grafika
apt install -y gimp krita inkscape blender

# biuro
apt install -y libreoffice

# lokalne systemy AI
su - -- <<EOF
curl -fsSL https://ollama.com/install.sh | sh
EOF

su - work -- <<EOF
ollama pull deepseek-r1:7b
ollama pull qwen2.5-coder
EOF
