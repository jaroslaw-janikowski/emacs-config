source ./setup-debian-base.sh

# mpv
apt install -y mpv
su bps
cat <<EOF>> ~/.config/mpv/mpv.conf
pause
ytdl-format=18
EOF
exit

apt install -y fasm

# science
apt install -y gnuplot gnuplot-doc graphviz graphviz-doc

apt install -y libnewt-dev

# tex
apt -y install auctex
