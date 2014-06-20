#!/bin/sh

yum install ansible
mkdir /platform
cd /platform
git clone git@git.addsrv.net:platform/platform_mz_cluster.git
