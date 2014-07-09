#!/bin/sh

yum install ansible
easy_install argparse
mkdir -p /platform
test -r /platform/mz-cluster || git clone git@gitlab.addsrv.com:saas-infrastructure/mz-cluster.git /platform/mz-cluster
git clone git@gitlab.addsrv.com:platform-software/mz-bench.git
