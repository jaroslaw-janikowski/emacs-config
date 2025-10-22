# aktualizacja
apt update -y
apt upgrade -y
apt autoremove -y

apt install -y xorg openbox pavucontrol brightnessctl p7zip-full unifont clzip htop mc aspell aspell-pl aspell-en firefox-esr

cat <<EOF>> ~/.profile

startx &

EOF
