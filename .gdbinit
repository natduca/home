def fs
  finish
  step
end


python
import gdb
import re
import subprocess
class PickPID(gdb.Function):
  def __init__(self):
    gdb.Function.__init__(self, "pickpid")

  def pick(self, name):
    proc = subprocess.Popen(["ps", "ax"], stdout=subprocess.PIPE)
    out = proc.communicate()[0]
    cand = []
    for l in out.split("\n"):
      l = l.strip()
      if l == '':
        continue
      rec = re.split("\s+", l)
      if rec[4].find(name) != -1:
        if len(rec) > 5:
          cand.append((rec[0],rec[4]," ".join(rec[5:])))
        else:
          cand.append((rec[0],rec[4],""))
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

  def invoke(self, name):
    pid = self.pick(name.string())
    if pid != 0:
      f = open("/tmp/picked", "w")
      sys.stdout.write("Attaching to %s\n" % pid)
      f.write('attach %s\n' % pid)
      f.close()
    return pid != 0
PickPID()
end

def adrt
  if $pickpid("DumpRenderTree")
    source /tmp/picked
  end
end

