# aktualizacja
apt update -y
apt upgrade -y
apt autoremove -y

# Sway desktop
apt install -y sway xwayland pavucontrol brightnessctl swayidle swaylock grimshot wdisplays p7zip-full gnupg2 unifont clzip htop

# narzędzia
apt install -y mc

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
sed -i "s/%I:%M:%S/%H:%M:%S/" ~/.config/sway/config
sed -i "s/| dmenu | /| dmenu -m 0 | /" ~/.config/sway/config
EOF

cat <<EOF>> ~/.profile

sway &

EOF

apt install -y firefox-esr
