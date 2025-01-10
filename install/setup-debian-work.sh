source ./setup-debian-base.sh

# zeal
apt install -y zeal

# lazarus-ide
apt install -y lazarus-ide lazarus-doc

# php
apt install -y php phpunit

# node.js
apt install -y node npm

# podman setup
apt install -y podman podman-compose

# ruby, ruby on rails, tools
apt install -y ruby ruby-dev ruby-bundler ruby-rails
gem install solargraph

su bps
podman pull docker.io/php
podman pull docker.io/mysql
podman pull docker.io/wordpress
podman pull docker.io/python
podman pull docker.io/postgres
exit
