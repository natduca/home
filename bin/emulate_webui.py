#!/usr/bin/env python

# This script puts a simple webserver runing on localhost:8080
# that processes file requests through Grit's flattening process
# This allows you to debug WebUI pages without having to build all
# of chrome. :)
import logging
import os
import select
import sys
import time
import urlparse
import BaseHTTPServer
import SimpleHTTPServer

gritpath = os.path.abspath("../../../tools/grit")
if not os.path.exists(gritpath):
  raise Exception("Could not find grit!")

sys.path.append(gritpath)
import grit.format.html_inline as grit_inline

PORT = 8080

jstfile1 = os.path.abspath("../../third_party/jstemplate/jstemplate.js")
jstfile2 = os.path.abspath("../../third_party/jstemplate/jsevalcontext.js")
jstfile3 = os.path.abspath("../../third_party/jstemplate/util.js")
if not os.path.exists(jstfile1) or not os.path.exists(jstfile2) or not os.path.exists(jstfile3):
  raise Exception("Could not find jstemplate!")
jsttext = """<script>
%s
%s
%s
</script>
""" % (open(jstfile1, 'r').read(),
       open(jstfile2, 'r').read(),
       open(jstfile3, 'r').read())

def sendContent(ext,req,msg):
  req.send_response(200)
  req.send_header('Last-Modified', req.date_time_string(time.time()))
  req.send_header('Content-Length', len(msg))
  if ext == ".js":
    req.send_header('Content-Type', 'application/javascript')
  elif ext == ".css":
    req.send_header('Content-Type', 'text/css')
  elif ext == ".html":
    req.send_header('Content-Type', 'text/html')
  elif ext == ".json":
    req.send_header('Content-Type', 'text/x-json')
  elif ext == ".wav":
    req.send_header('Content-Type', 'audio/x-wav')
  else:
    print "could not geuss mimetype for: %s" % ext
    req.send_header('Content-Type', 'text/plain')
  req.end_headers()
  req.wfile.write(msg)

def sendError(req, code):
  try:
    msg = "Unrecognized URL"
    req.send_response(code)
    req.send_header('Last-Modified', req.date_time_string(time.time()))
    req.send_header('Content-Length', len(msg))
    req.send_header('Content-Type', 'text/plain')
    req.end_headers()
    req.wfile.write(msg)
  except:
    pass

class RequestHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):
  def __init__(self,*args):
    SimpleHTTPServer.SimpleHTTPRequestHandler.__init__(self,*args)

  def do_GET(req):
    (_, _, path, _, _) = urlparse.urlsplit(req.path)
    if path == "":
      path = "/"
    if not path.startswith("/"):
      raise Exception("Wtf")

#    import pdb; pdb.set_trace()
    rpath = os.path.realpath(os.path.join(os.getcwd(), path[1:]))
    if not rpath.startswith(os.getcwd()) or not os.path.exists(rpath):
      sendError(req, 404)
      return
    if os.path.isdir(rpath):
      SimpleHTTPServer.SimpleHTTPRequestHandler.do_GET(req)
      return
    if not os.path.splitext(rpath)[1].lower() in ('.html', '.htm'):
      SimpleHTTPServer.SimpleHTTPRequestHandler.do_GET(req)
      return
    else:
      s = grit_inline.InlineToString(rpath, None)
      s_ = s.replace("chrome://resources/", "./shared/")
      ext = os.path.splitext(rpath)[1]
      if ext == ".html":
        s_ += jsttext
      sendContent(ext, req, s_)
      return

class Daemon(BaseHTTPServer.HTTPServer):
  def __init__(self, host, port):
    BaseHTTPServer.HTTPServer.__init__(self, (host, port), RequestHandler)
    self.port_ = port

  def on_exit(self, m, verb, data):
    logging.info("Exiting upon request.")
    self.shutdown()

  def serve_forever(self):
    logging.info('Fake DOM_UI started on port %d', self.port_)
    self.is_running_ = True
    while self.is_running_:
      delay = 0.2
      r, w, e = select.select([self], [], [], delay)
      if r:
        self.handle_request()

  def shutdown(self):
    self.is_running_ = False
    self.server_close()
    return 1

if __name__ == "__main__":
  d = Daemon("", PORT)
  try:
    d.serve_forever()
  except KeyboardInterrupt:
    pass
  d.shutdown()
