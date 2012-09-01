#!/usr/bin/env python
import os
import sys
import optparse

def main(argv):
  parser = optparse.OptionParser()
  parser.add_option('--curses', action="store_true", dest="curses", help="Use curses UI")
  options, argv = parser.parse_args(argv)

if __name__ == "__main__":
  sys.exit(main(sys.argv))
