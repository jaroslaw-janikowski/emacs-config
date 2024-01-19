apt install -y emacs build-essential findutils git ispell iamerican ipolish silversearcher-ag curl sway make xwayland pavucontrol brightnessctl git git-gui swayidle swaylock qemu-system-x86 podman podman-compose zeal grimshot wdisplay
exit

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

# emacs setup
emacs -Q --script ~/.emacs.d/install/setup-emacs.el

# podman setup
podman pull docker.io/php
podman pull docker.io/mysql
podman pull docker.io/wordpress
