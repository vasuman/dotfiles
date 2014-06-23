#!/usr/bin/env python2

from os import listdir, symlink
from os.path import expanduser, abspath, samefile, join, exists
from sys import argv

HOME_DIR = expanduser('~')
CUR_DIR = abspath('.')
SELF_PATH = join(CUR_DIR, argv[0])

def main():
    for f in listdir(CUR_DIR):
        path = join(CUR_DIR, f)
        if samefile(path, SELF_PATH):
            print 'Self, %s' % path
            continue
        if f.startswith('.'):
            print 'Hidden, %s' % path
            continue
        link = join(HOME_DIR, '.%s' % f)
        if not exists(link):
            print 'Linking, %s' % link
            symlink(path, link)
        else:
            print 'Existing, %s' % link

if __name__ == '__main__':
    main()
