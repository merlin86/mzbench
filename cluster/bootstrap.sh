#!/bin/sh

yum install ansible
easy_install argparse
mkdir -p /platform
test -r /platform/mz-cluster || git clone git@git.addsrv.net:platform/platform_mz_cluster.git /platform/mz-cluster
git clone git@gitlab.addsrv.com:platform-software/mz-bench.git
