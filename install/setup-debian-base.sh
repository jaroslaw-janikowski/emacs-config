# aktualizacja
apt update -y
apt upgrade -y
apt autoremove -y

# instalacja narzędzi
apt install -y sway make xwayland pavucontrol brightnessctl swayidle swaylock qemu-system-x86 grimshot wdisplays p7zip-full stterm clangd gnupg2 unifont silversearcher-ag gdb clzip sqlite3 sqlite3-doc

# suckless terminal
# apt install -y stterm
su - work -- <<EOF
git clone --depth 1 https://git.suckless.org/scroll /tmp/scroll
cd /tmp/scroll
make
EOF
cd /tmp/scroll && make install
su - work -- <<EOF
git clone --depth 1 https://git.suckless.org/st /tmp/st
cd /tmp/st
make config.h
sed -i "s/char *scroll = NULL;/char *scroll = \"scroll\";/g"
make
EOF
cd /tmp/st && make install
update-alternatives --config x-terminal-emulator

# nagrywanie ekranu w Wayland
apt install -y wf-recorder

# spelling tools
apt install -y aspell aspell-pl aspell-en

# git setup
apt install -y git git-gui

# sway setup
su - work -- <<EOF
mkdir ~/.config/sway
cp /etc/sway/config ~/.config/sway/config
cat <<EOF2>> ~/.config/sway/config

set \$term st

exec brightnessctl set 6%

input * {
    xkb_layout "pl"
}

input "2:8:AlpsPS/2_ALPS_GlidePoint" {
    dwt enabled
    tap enabled
    middle_emulation enabled
}

output eDP-1 resolution 1366x768
output DP-2 resolution 1366x768
output DP-2 disable
output DP-3 resolution 1366x768
output DP-3 disable

xwayland enable
bindsym XF86AudioRaiseVolume exec pactl set-sink-volume @DEFAULT_SINK@ +10%
bindsym XF86AudioLowerVolume exec pactl set-sink-volume @DEFAULT_SINK@ -10%
bindsym XF86AudioMute exec pactl set-sink-mute @DEFAULT_SINK@ toggle
bindsym XF86MonBrightnessUp exec brightnessctl set 3%+
bindsym XF86MonBrightnessDown exec brightnessctl set 3%-
bindsym Print exec grimshot copy area
workspace_layout tabbed

EOF2

sed -i "s/position top/position bottom" ~/.config/sway/config
sed -i "s/%I:%M:%S/%H:%M:%S" ~/.config/sway/config
EOF

cat <<EOF>> ~/.profile

# set only for first logged user
if [[ "$(tty)" == "/dev/tty1" ]] ; then
    export PATH="$PATH:~/.local/bin"
fi

sway &

EOF

# ufw setup
apt install -y ufw
systemctl enable ufw
ufw deny ssh
ufw deny telnet
ufw deny vnc

# emacs
apt build-dep -y emacs
apt install -y libtree-sitter-dev libsqlite3-dev
su - work -- <<EOF
emacs -Q --script ~/.emacs.d/install/setup-emacs.el
EOF
