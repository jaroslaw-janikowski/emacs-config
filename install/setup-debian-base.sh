# aktualizacja
apt update -y
apt upgrade -y
apt autoremove -y

# instalacja narzędzi
apt install -y sway make xwayland pavucontrol brightnessctl swayidle swaylock qemu-system-x86 grimshot wdisplays p7zip-full stterm clangd gnupg2 unifont silversearcher-ag gdb clzip sqlite3 sqlite3-doc

# nagrywanie ekranu w Wayland
apt install -y wf-recorder

# spelling tools
apt install -y aspell aspell-pl aspell-en

# git setup
apt install -y git git-gui

# sway setup
su bps
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

# ufw setup
apt install -y ufw
systemctl enable ufw
ufw deny ssh
ufw deny telnet
ufw deny vnc

# emacs
apt build-dep emacs
apt install -y libtree-sitter-dev libsqlite3-dev
su bps
emacs -Q --script ~/.emacs.d/install/setup-emacs.el
exit
