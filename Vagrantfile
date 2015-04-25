# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

$script = <<EOF
set -xe
export DEBIAN_FRONTEND=noninteractive

if [ ! -x "$(which erl)" -o $(($(date +%s) - $(stat -c '%Z' /var/lib/apt/periodic/update-success-stamp))) -gt 7200 ]; then
	sudo sh -c 'echo "deb http://packages.erlang-solutions.com/debian wheezy contrib" > /etc/apt/sources.list.d/erlang.list'
	wget -c http://packages.erlang-solutions.com/debian/erlang_solutions.asc
	sudo apt-key add erlang_solutions.asc
	rm -f erlang_solutions.asc
	sudo apt-get update
	rm -f erlang_solutions.asc
fi

sudo apt-get install -y --force-yes erlang git autoconf gcc-arm-none-eabi make build-essential
sudo apt-get install -y xen-hypervisor-4.4-amd64 bridge-utils

# create a workspace for local pushes
sudo -u vagrant -i git init ling
sudo -u vagrant -i sh -c '(cd ling; git config receive.denyCurrentBranch ignore)'

# indir simplifies a lot of interactive scripting
cat > /usr/local/bin/indir << INLING
#!/usr/bin/env bash
cd \\$1; shift
exec "\\$@"
INLING
chmod +x /usr/local/bin/indir
EOF

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.box = "ubuntu/trusty64"
  config.vm.provider "virtualbox" do |vb|
    vb.memory = 2048
  end

  config.vm.hostname = "hackling"
  config.vm.provision "shell", inline: $script

  # xen kernel breaks vboxguest
  config.vm.synced_folder ".", "/vagrant", disabled: true
end
