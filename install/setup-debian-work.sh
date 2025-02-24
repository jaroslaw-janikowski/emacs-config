source ./setup-debian-base.sh

# zeal
apt install -y zeal

# lazarus-ide
apt install -y lazarus-ide lazarus-doc

# php
apt install -y php phpunit composer php-xml php-sqlite3

su work
curl -Lo phpactor.phar https://github.com/phpactor/phpactor/releases/latest/download/phpactor.phar &&
    chmod a+x phpactor.phar &&
    mv phpactor.phar ~/.local/bin/phpactor
exit

# node.js
apt install -y nodejs npm
npm install -g typescript-language-server typescript

# podman setup
apt install -y podman podman-compose

# ruby, ruby on rails, tools
apt install -y build-essential rustc libssl-dev libyaml-dev zlib1g-dev libgmp-dev curl

su work
curl https://mise.run | sh
echo 'eval "$(~/.local/bin/mise activate)"' >> ~/.bashrc
source ~/.bashrc
mise use -g ruby@3
gem install rails
gem install solargraph
gem install solargraph-rails
exit

su work
podman pull docker.io/php
podman pull docker.io/mysql
podman pull docker.io/wordpress
podman pull docker.io/python
podman pull docker.io/postgres
exit
