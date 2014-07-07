#! /usr/bin/env python

import sys, os
import string, random

import ansible.constants
ansible.constants.HOST_KEY_CHECKING = False
import ansible.runner
import ansible.inventory

sys.path.append('/platform/platform_mz_cluster')
from cluster_plan import allocate, deallocate
from process import run_local
from util import log
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
    info("Allocated %s hosts for '%s': %s" % (nodes_count, purpose, hosts[name]))
    return hosts

def setup_bench(hosts, cookie):
    inv = ansible.inventory.Inventory(hosts)
    ensure_file(inv, '/root/.erlang.cookie', cookie)

    ensure_file(inv, '/etc/yum.repos.d/mz-unstable-%s.repo' % env.USER, USER_REPO)

    run_ansible(inv, 'yum', {'name': 'mzbench', 'state': 'present', 'disable_gpg_check': 'yes'})
    info("mzbench rpm is present")

    ensure_file(inv, '/root/vm.args', "-sname mzbench\n-setcookie %s" % cookie)
    run_ansible(inv, 'command', 'chdir=/root /mz/mzbench/bin/mzbench start')
    info("mzbench is running")

    # For some reason passing argumends as a sequence doesn't work.
    # wait_cluster_start receives empty argv in that case.
    run_local('/usr/bin/env epmd -daemon')
    run_local(' '.join(["../wait_cluster_start", cookie, '10000'] + hosts))
    info("mzbench nodes ready: %s" % hosts)

def run_bench(script, host, cookie):
    info("Running '%s' on %s" % (script, host))
    run_local(["../run", script, cookie, host])
    info("[ OK ] Finished '%s' on %s" % (script, host))

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(prog='bench.py')
    parser.add_argument('script', nargs=1)
    parser.add_argument('nodes_count', type=int, nargs='?', default=1)
    args = vars(parser.parse_args())

    try:
        user    = os.environ['REMOTE_USER']
        purpose = "bench-" + rand_str(len=10)
        cookie  = rand_str(len=20)

        info("user: {0}".format(user))
        info("purpose: {0}".format(purpose))
        info("cookie: {0}".format(cookie))

        info("Allocating hosts")
        hosts = allocate_hosts(args['nodes_count'], purpose, user)
        info("Got hosts: {0}".format(hosts))

        info("Running setup")
        setup_bench(hosts, cookie)

        info("Running script")
        run_bench(args['script'][0], hosts[0], cookie)
    except Exception as e:
        log("[ ERROR ] Failed to allocate boxes, rolling back")
        log(e)
    deallocate(purpose)
