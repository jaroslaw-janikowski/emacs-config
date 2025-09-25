# aktualizacja
apt update -y
apt upgrade -y
apt autoremove -y

# Sway desktop
apt install -y sway make xwayland pavucontrol brightnessctl swayidle swaylock grimshot wdisplays p7zip-full clangd gnupg2 unifont silversearcher-ag gdb clzip sqlite3 sqlite3-doc

# nagrywanie ekranu w Wayland
apt install -y wf-recorder

# spelling tools
apt install -y aspell aspell-pl aspell-en

# sway setup
su - work -- <<EOF
mkdir ~/.config/sway
cp /etc/sway/config ~/.config/sway/config
cat <<EOF2>> ~/.config/sway/config

exec brightnessctl set 6%

input * {
    xkb_layout "pl"
}

xwayland enable
bindsym XF86AudioRaiseVolume exec pactl set-sink-volume @DEFAULT_SINK@ +10%
bindsym XF86AudioLowerVolume exec pactl set-sink-volume @DEFAULT_SINK@ -10%
bindsym XF86AudioMute exec pactl set-sink-mute @DEFAULT_SINK@ toggle
bindsym XF86MonBrightnessUp exec brightnessctl set 3%+
bindsym XF86MonBrightnessDown exec brightnessctl set 3%-
bindsym Print exec grimshot copy area
workspace_layout tabbed

EOF2

sed -i "s/position top/position bottom/" ~/.config/sway/config
sed -i "s/%I:%M:%S/%H:%M:%S" ~/.config/sway/config
EOF

cat <<EOF>> ~/.profile

sway &

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

su - -- <<EOF
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
sed -i "s/;; (require 'work)/(require 'work)/" ~/.emacs.d/init.el
git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git ~/workspace/emacs
cd ~/workspace/emacs && make bootstrap
EOF
cd /home/work/workspace/emacs && make install

su - work -- <<EOF
emacs -Q --script ~/.emacs.d/install/setup-emacs.el
emacs -Q --script ~/.emacs.d/install/setup-emacs-work.el
EOF

# nsis
apt install -y nsis nsis-doc

# python setup
apt install -y python3-venv python3-pip python3-debugpy python3-pylsp

# php
apt install -y php phpunit composer php-xml php-mbstring php-mysql php-redis php-sqlite3 php-xdebug
apt remove -y apache2  # czemu to jest instalowane domyślnie?

su - work -- <<EOF
mkdir -p ~/.local/bin && cd ~/.local/bin &&  wget https://github.com/phpactor/phpactor/releases/latest/download/phpactor.phar && chmod a+x ./phpactor.phar
mv ./phpactor.phar ./phpactor
EOF

# node.js
su - work -- <<EOF
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
\. "$HOME/.nvm/nvm.sh"
nvm install 22
npm install -g typescript-language-server typescript
npm install -g @angular/cli
EOF

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
su - work -- <<EOF
lazbuild --add-package /usr/local/share/lazarus/components/onlinepackagemanager/onlinepackagemanager.lpk
lazbuild --add-package /usr/local/share/lazarus/components/anchordocking/anchordocking.lpk
lazbuild --add-package /usr/local/share/lazarus/components/anchordocking/dsgn/anchordockingdsgn.lpk
lazbuild --add-package /usr/local/share/lazarus/components/dockedformeditor/dockedformeditor.lpk
lazbuild --add-package /usr/local/share/lazarus/components/weblaz/weblaz.lpk
lazbuild --build-ide
EOF

# grafika
apt install -y gimp krita inkscape blender

# biuro
apt install -y libreoffice printer-driver-cups-pdf xsane
# su - work -- <<EOF
# cd ~/Downloads
# wget ???
# gunzip ~/Downloads/linux-bprinter-driver-cups-pdf
# sh ~/Downloads/linux-bprinter-driver-cups-pdf DCP-1510E
# EOF

# lokalne systemy AI
su - -- <<EOF
curl -fsSL https://ollama.com/install.sh | sh
EOF

su - work -- <<EOF
ollama pull deepseek-r1:7b
ollama pull qwen2.5-coder
ollama pull sqlcoder:7b
EOF

# sublime
wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | tee /etc/apt/keyrings/sublimehq-pub.asc > /dev/null
echo 'Types: deb\nURIs: https://download.sublimetext.com/\nSuites: apt/stable/\nSigned-By: /etc/apt/keyrings/sublimehq-pub.asc' | tee /etc/apt/sources.list.d/sublime-text.sources
apt -y update
apt install -y sublime-text sublime-merge
