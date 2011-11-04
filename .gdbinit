def fs
  finish
  step
end


python
import gdb
import re
import subprocess
class PickPIDBase(gdb.Function):
  def __init__(self, cmdname, pick_fn):
    gdb.Function.__init__(self, cmdname)
    self.pick_fn = pick_fn

  def pick(self):
    proc = subprocess.Popen(["ps", "ax", "-ww", "-o", "pid,cmd=ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ,args"], stdout=subprocess.PIPE)
    out = proc.communicate()[0]
    cand = []
    for l in out.split("\n"):
      l = l.strip()
      if l == '':
        continue
      rec = re.split("\s+", l)
      if rec[2].startswith('-'):
        args = rec[4:]
      else:
#        print rec
        args = rec[3:]
#        print ">>>", (rec[0],rec[1],args), " from ", rec
      if self.pick_fn(rec[1],args):
        cand.append((rec[0],rec[1],args))

    if len(cand) == 0:
      print "No process found"
      return 0
    elif len(cand) > 1:
      print "Multiple processes found."
      for i in range(len(cand)):
          c = cand[i]
          if c[1][0] == '-':
            pn = c[1][1:]
          else:
            pn = c[1]
          print "[%3i] %8s %-s %s" % (i+1, c[0],pn,c[2])
      sys.stdout.write("> ")
      sys.stdout.flush()
      try:
        n = int(sys.stdin.readline()) - 1
      except:
        return 0
      if n >= 0 and n < len(cand):
        return int(cand[n][0])
      print "Invalid number."
      return 0
    else:
      return int(cand[0][0])

  def invoke(self):
    pid = self.pick()
    if pid != 0:
      f = open("/tmp/picked", "w")
      sys.stdout.write("Attaching to %s\n" % pid)
      f.write('attach %s\n' % pid)
      f.close()
    return pid != 0

def is_drt(name,args):
   return name.find("DumpRenderTree") != -1
PickPIDBase("pickdrt", is_drt)

def is_chrome(type, name, args):
   if name.find("chrome") == -1:
     return False
   # Ignore the goobuntu chrome
   if name.find("/opt/google/") != -1:
     return False
   if name.find("/usr/bin/") != -1:
     return False
   if name == '[chrome]':
     return False
   if type == "browser":
     not_type = len([a for a in args if a.startswith("--type")]) == 0
     return not_type
   else:
     for a in args:
       if a == ("--type=%s" % type):
         return True
   return False

PickPIDBase("pickchromerenderer", lambda x, y: is_chrome("renderer", x, y))
PickPIDBase("pickchromegpu", lambda x, y: is_chrome("gpu-process", x, y))
PickPIDBase("pickchromebrowser", lambda x, y: is_chrome("browser", x, y))

end

# attach to a DumpRenderTree
def adrt
  if $pickdrt()
    source /tmp/picked
  end
end

# attach to a chrome renderer
def acrr
  if $pickchromerenderer()
    source /tmp/picked
  end
end

# attach to a chrome gpu-process
def acrg
  if $pickchromegpu()
    source /tmp/picked
  end
end


# attach to a chrome browser
def acrb
  if $pickchromebrowser()
    source /tmp/picked
  end
end

