#!/usr/bin/env python2

from os import listdir, symlink
import os.path as path
from sys import argv

HOME_DIR = path.expanduser('~')
SELF_PATH = path.realpath(__file__)
CUR_DIR = path.dirname(SELF_PATH)


def ln_copy(src, dst, exclude = [], overwrite = False):
    if path.samefile(src, dst):
        print 'Samefile, %s and %s' % (src, dst)
        return
    print 'Copy linking from, %s to %s' % (src, dst)
    for f in listdir(src):
        p = path.join(src, f)
        if f.startswith('.git'):
            print 'Git, %s' % p
            continue
        if p in exclude:
            print 'Excluded %s' % p
            continue
        link = path.join(dst, '%s' % f)
        if not path.exists(link):
            print 'Linking, %s' % link
            symlink(p, link)
        elif overwrite:
            print 'Overwriting, %s' % link
        elif path.isdir(link) and path.isdir(p):
            print 'Dir exists, recurring'
            ln_copy(p, link, exclude)
        else:
            print 'Existing, %s' % link

def main():
    ln_copy(CUR_DIR, HOME_DIR, [SELF_PATH])

if __name__ == '__main__':
    main()
