apt install -y sway make xwayland pavucontrol brightnessctl swayidle swaylock qemu-system-x86 grimshot wdisplays p7zip-full xterm clangd nsis gnupg2 gnuplot gnuplot-doc graphviz graphviz-doc unifont silversearcher-ag libtree-sitter-dev gdb

# spelling tools
apt install -y ispell ipolish ibritish iamerican

# git setup
apt install -y git git-gui
su bps
git config --global pull.rebase true

# sway setup
mkdir ~/.config/sway
cp /etc/sway/config ~/.config/sway/config
cat <<EOF>> ~/.config/sway/config

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

EOF

cat <<EOF>> ~/.profile

# set only for first logged user
if [[ "$(tty)" == "/dev/tty1" ]] ; then
	export PATH="$PATH:~/.local/bin"
	sway &
fi

EOF
exit

# mpv
apt install -y mpv
su bps
cat <<EOF>> ~/.config/mpv/mpv.conf
pause
ytdl-format=18
EOF
exit

# ufw setup
apt install -y ufw
systemctl enable ufw
ufw deny ssh
ufw deny telnet
ufw deny vnc

# python setup
apt install -y python3-venv python3-pip python3-debugpy python3-pylsp

# emacs
apt build-dep emacs
su bps
emacs -Q --script ~/.emacs.d/install/setup-emacs.el
exit
