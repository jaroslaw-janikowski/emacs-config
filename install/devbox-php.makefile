run:
	qemu-system-x86_64 -enable-kvm -sandbox on -m 4096 -vga virtio -display gtk,show-menubar=on -name "devbox-php" -net nic -net user,hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:80 -hda ./.dev-server.qcow2 -virtfs local,path=.,mount_tag=project,security_model=mapped

create:
	qemu-img create ./.dev-server.qcow2 60G -f qcow2
	kvm -m 2048 -vga virtio -display gtk,show-menubar=on -hda ./.dev-server.qcow2 -cdrom ~/Pobrane/debian-13.0.0-amd64-netinst.iso -boot d

provision:
	apt install -y php-fpm \
		nginx \
		php-mysql mariadb-server \
		redis-server php-redis \
		rabbitmq-server
	echo "project /var/www/html/ 9p trans=virtio,version=9p2000.L 0 0" >> /etc/fstab
