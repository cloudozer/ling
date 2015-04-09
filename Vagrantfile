# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

$script = <<EOF
set -xe
export DEBIAN_FRONTEND=noninteractive

sudo sh -c 'echo "deb http://packages.erlang-solutions.com/debian wheezy contrib" > /etc/apt/sources.list.d/erlang.list'
wget -c http://packages.erlang-solutions.com/debian/erlang_solutions.asc
sudo apt-key add erlang_solutions.asc
sudo apt-get update
sudo apt-get install -y --force-yes erlang git autoconf gcc-arm-none-eabi make build-essential

cd /vagrant
make build-arm
EOF

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.box = "ubuntu/trusty64"
  config.vm.provider "virtualbox" do |vb|
    vb.memory = 2048
  end

  config.vm.provision "shell", inline: $script

end
