source ./setup-debian-base.sh

# zeal
apt install -y zeal

# lazarus-ide
apt -y install lazarus-ide lazarus-doc

# php
apt -y install php phpunit

# podman setup
apt install -y podman podman-compose

su bps
podman pull docker.io/php
podman pull docker.io/mysql
podman pull docker.io/wordpress
podman pull docker.io/python
podman pull docker.io/postgres
exit
