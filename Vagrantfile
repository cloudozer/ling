# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

$script = <<EOF
set -xe
export DEBIAN_FRONTEND=noninteractive
sudo apt-get update

sudo apt-get install -y --force-yes git autoconf gcc-arm-none-eabi make build-essential
sudo apt-get install -y xen-hypervisor-4.4-amd64 bridge-utils

wget http://packages.erlang-solutions.com/site/esl/esl-erlang/FLAVOUR_1_general/esl-erlang_17.5-1~ubuntu~trusty_amd64.deb
sudo dpkg -i esl-erlang_17.5-1~ubuntu~trusty_amd64.deb || true
sudo apt-get install -f -y
sudo dpkg -i esl-erlang_17.5-1~ubuntu~trusty_amd64.deb

dpkg-divert --divert /etc/grub.d/08_linux_xen --rename /etc/grub.d/20_linux_xen 
update-grub

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

# reboot for xen divert
reboot
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
