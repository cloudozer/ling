# -*- mode: ruby -*-
# vi: set ft=ruby :

# Thin Vagrant file; eventually this will be expanded to split
# ./build.sh into separate provision/build components.

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.box = "ubuntu/trusty64"
  config.vm.provider "virtualbox" do |vb|
    vb.memory = 2048
  end

  config.vm.provision "shell", path: "build.sh"

end
