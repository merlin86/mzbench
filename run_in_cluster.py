#! /usr/bin/env python

import sys, os
import string, random

import ansible.constants
ansible.constants.HOST_KEY_CHECKING = False
import ansible.runner
import ansible.inventory

sys.path.append('/platform/mz-cluster')
from cluster_plan import allocate, deallocate
from process import run_local
from util import log, slurp
import env

def info(msg):
    print "[ INFO ] %s" % msg

USER_REPO = """[mz-unstable-{0}]
name=MachineZone unstable {0} repo
baseurl=http://rpm.addsrv.net/dav/users/{0}/repo/
enabled=1
skip_if_unavailable=0
metadata_expire=10
sslcacert=/etc/pki/tls/certs/ca-addsrv.pem
""".format(env.USER)

def sname(hostname):
    return hostname.split('.')[0]

def check_ansible_result(res):
    failures = {}
    for host, result in res['dark'].iteritems():
        failures[host] = result.get('msg')
    for host, result in res['contacted'].iteritems():
        if result.get('failed'):
            failures[host] = result.get('msg')
    if failures:
        raise Exception("Ansible failed on following hosts: %s" % failures)
    else:
        return res

def run_ansible(inv, module, args):
    args = args if isinstance(args, basestring) else ' '.join("%s=%r" % (k,v) for (k,v) in args.iteritems())
    if "DEBUG" in os.environ:
        print "run_ansible: %s %s" % (module, args)
    runner = ansible.runner.Runner(
        remote_user = 'root',
        inventory   = inv,
        module_name = module,
        module_args = args
    )
    return check_ansible_result(runner.run())

def ensure_file(inv, path, content, owner='root', group='root', mode='400'):
    return run_ansible(inv, 'copy', { 'content': content,
                                      'dest':    path,
                                      'owner':   owner,
                                      'group':   group,
                                      'mode':    mode })

def rand_str(len=20, chars=string.ascii_uppercase):
    return ''.join(random.choice(chars) for _ in range(len))

def allocate_hosts(nodes_count, purpose, user):
    name = 'bench'
    services = allocate(purpose, {'service': [
                                   {'container-template': {
                                        'name':           name,
                                        'description':    purpose,
                                        'container-type': 'erlang',
                                        'constraint':     'shared',
                                        'quantity':       nodes_count
                                   }}]})
    hosts = [name + str(n) + '.' + purpose + '.' + user + '.virt' for n in range(nodes_count)]
    info("Allocated %s hosts for '%s': %s" % (nodes_count, purpose, services[name]))
    return hosts

def setup_bench(hosts, cookie):
    inv = ansible.inventory.Inventory(hosts)
    ensure_file(inv, '/root/.erlang.cookie', cookie)

    ensure_file(inv, '/etc/yum.repos.d/mz-unstable-%s.repo' % env.USER, USER_REPO)

    run_ansible(inv, 'yum', {'name': 'mz_bench', 'state': 'present', 'disable_gpg_check': 'yes'})
    info("mz_bench rpm is present")

    ensure_file(inv, '/root/vm.args', "-sname mz_bench\n-setcookie %s" % cookie)
    run_ansible(inv, 'command', 'chdir=/root /mz/mz_bench/bin/mz_bench start')
    info("mz_bench is running")

    inv0 = ansible.inventory.Inventory(hosts[0:1])
    snames = [sname(h) for h in hosts]
    run_ansible(inv0, 'command', ' '.join(['/mz/mz_bench/bin/wait_cluster_start', cookie, '10000'] + snames))

    info("mz_bench nodes ready: %s" % hosts)

def run_bench(script, host, cookie):
    
    inv0 = ansible.inventory.Inventory([host])
    ensure_file(inv0, '/root/current.bench', slurp(script))
    cmd = ['/mz/mz_bench/bin/run', '/root/current.bench', cookie, sname(host)]
    run_ansible(inv0, 'command', ' '.join(cmd))
    info("[ OK ] Finished '%s' on %s" % (script, host))

def gettemppath():
    import tempfile
    return tempfile.gettempdir() + '/current.bench'

if __name__ == "__main__":


    import argparse
    parser = argparse.ArgumentParser(prog='bench.py')
    parser.add_argument('script', nargs=1, help='Name of bench script to run')
    parser.add_argument('--nodes', type=int, metavar='N', default=1, help='Amount of nodes to allocate, defaults to 1')
    args = vars(parser.parse_args())

    script = gettemppath()
    run_local('/mz/mzbench/bin/preprocess.escript ' + args['script'][0] + ' >' + script)

    try:
        user    = os.environ['REMOTE_USER']
        purpose = "bench-" + rand_str(len=10)
        cookie  = rand_str(len=20)

        info("user: {0}".format(user))
        info("purpose: {0}".format(purpose))
        info("cookie: {0}".format(cookie))

        info("Allocating %s host(s)" % args['nodes'])
        hosts = allocate_hosts(args['nodes'], purpose, user)
        info("Got hosts: {0}".format(hosts))

        info("Running setup")
        setup_bench(hosts, cookie)

        info("Running '%s' on %s" % (args['script'][0], hosts[0]))
        run_bench(script, hosts[0], cookie)
    except Exception as e:
        log("[ ERROR ] Failed to allocate boxes, rolling back")
        log(e)
    deallocate(purpose)
