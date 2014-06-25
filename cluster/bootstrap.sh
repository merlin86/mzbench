#!/bin/sh

yum install ansible
easy_install argparse
mkdir -p /platform
test -r /platform/platform_mz_cluster || git clone git@git.addsrv.net:platform/platform_mz_cluster.git /platform/platform_mz_cluster
