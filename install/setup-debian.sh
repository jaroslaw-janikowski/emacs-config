apt install -y sway make xwayland pavucontrol brightnessctl git-gui swayidle swaylock qemu-system-x86 podman podman-compose zeal grimshot wdisplays p7zip-full xterm fasm
su bps

# sway setup
mkdir ~/.config/sway
cp /etc/sway/config ~/.config/sway/config
cat <<EOF>> ~/.config/sway/config

exec brightnessctl set 9%

input * {
	  xkb_layout "pl"
}

output DP-2 disable
xwayland enable
bindsym XF86AudioRaiseVolume exec pactl set-sink-volume @DEFAULT_SINK@ +10%
bindsym XF86AudioLowerVolume exec pactl set-sink-volume @DEFAULT_SINK@ -10%
bindsym XF86AudioMute exec pactl set-sink-mute @DEFAULT_SINK@ toggle
bindsym XF86MonBrightnessUp exec brightnessctl set 3%+
bindsym XF86MonBrightnessDown exec brightnessctl set 3%-
bindsym Print exec grimshot copy area
workspace_layout tabbed

EOF

cat <<EOF>> ~/.profile

# set only for first logged user
if [[ "$(tty)" == "/dev/tty1" ]] ; then
	export PATH="$PATH:~/.local/bin"
	sway &
fi

EOF
exit

# ufw setup
apt install -y ufw
systemctl enable ufw
ufw deny ssh
ufw deny telnet
ufw deny vnc

# python setup
apt install -y python3-venv python3-pip

# emacs build
apt install -y build-essential findutils git ispell iamerican ipolish silversearcher-ag curl autoconf libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev libotf-dev libjpeg-dev libpng-dev libxpm-dev libsqlite3-dev libncurses-dev texinfo libjansson-dev libtree-sitter-dev libwebp-dev librsvg2-dev libgccjit-12-dev libxml2-dev libwebkit2gtk-4.0-dev ripgrep imagemagick unifont
su jj
git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git /tmp/emacs
cd /tmp/emacs
./autogen.sh
./configure --with-xwidgets --with-json --with-pgtk --with-native-compilation --with-mailutils --with-pop
make
exit
cd /tmp/emacs
make install
su bps

# emacs setup
emacs -Q --script ~/.emacs.d/install/setup-emacs.el

# podman setup
podman pull docker.io/php
podman pull docker.io/mysql
podman pull docker.io/wordpress
podman pull docker.io/python
podman pull docker.io/postgres
