#!/usr/bin/env python

# This script puts a simple webserver runing on localhost:8080
# that processes file requests through Grit's flattening process
# This allows you to debug WebUI pages without having to build all
# of chrome. :)

import sys
import os
import SimpleHTTPServer
import SocketServer
import time

gritpath = os.path.abspath("../../../tools/grit")
if not os.path.exists(gritpath):
  raise Exception("Could not find grit!")

sys.path.append(gritpath)
import grit.format.html_inline as grit_inline

PORT = 8080

jstfile = os.path.abspath("../../third_party/jstemplate/jstemplate.js")
if not os.path.exists(jstfile):
  raise Exception("Could not find jstemplate!")
jsttext = """<script>
%s
</script>
""" % open(jstfile, 'r').read()

def sendContent(ext,req,msg):
  req.send_response(200)
  req.send_header('Last-Modified', req.date_time_string(time.time()))
  req.send_header('Content-Length', len(msg))
  if ext == ".js":
    resp.send_header('Content-Type', 'application/javascript')
  elif ext == ".css":
    resp.send_header('Content-Type', 'text/css')
  elif ext == ".html":
    resp.send_header('Content-Type', 'text/html')
  elif ext == ".wav":
    resp.send_header('Content-Type', 'audio/x-wav')
  else:
    print "could not geuss mimetype for: %s" % ext
    resp.send_header('Content-Type', 'text/plain')
  req.end_headers()
  req.wfile.write(msg)

def send404(req):
  msg = "Unrecognized URL"
  req.send_response(404)
  req.send_header('Last-Modified', req.date_time_string(time.time()))
  req.send_header('Content-Length', len(msg))
  req.send_header('Content-Type', 'text/plain')
  req.end_headers()
  req.wfile.write(msg)

class MyHTTPRequestHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):
  extensions_map = {
      ".css": "text/css",
      ".js": "application/javascript",
      ".html" : "text/html"
     }

  def __init__(self,*args):
    SimpleHTTPServer.SimpleHTTPRequestHandler.__init__(self,*args)

  def do_GET(req):
    if req.path == "" or req.path == "/":
      SimpleHTTPServer.SimpleHTTPRequestHandler.do_GET(req)
      return
    rpath = req.path[1:]
    if os.path.exists(rpath):
      s = grit_inline.InlineToString(rpath, None)
      s_ = s.replace("chrome://resources/", "./shared/")
      ext = os.path.splitext(rpath)[1]
      if ext == ".html":
        s_ += jsttext
      sendContent(ext, req, s_)
      return
    else:
      send404(req)

httpd = SocketServer.TCPServer(("", PORT), MyHTTPRequestHandler)
print "Fake DOM_UI started at http://localhost:%i/" % PORT

try:
  httpd.serve_forever()
except KeyboardInterrupt:
  pass
httpd.server_close()
