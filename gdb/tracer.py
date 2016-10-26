# run: gdb -ex start -ex "source ./tracer.py" -ex "tracer" --args $exe

import gdb
import signal
import sys


what_happend = None

def addr2num(addr):
    try:
        return int(addr)  # Python 3
    except:
        return long(addr) # Python 2

def examine(addr):
    try:
        cmd = "x " + addr
        return (gdb.execute(cmd, to_string=True))[:-1]
    except:
        return "<error while examine PC>"


has_exited = False
class TracerCommand (gdb.Command):
    "Tracer command for trace generation."

    def __init__ (self):
        super (TracerCommand, self).__init__("tracer", gdb.COMMAND_USER)

    def invoke (self, args, from_tty):
        gdb.execute("set pagination off", to_string=True)
        gdb.execute("set confirm off", to_string=True)
        gdb.execute("set backtrace past-main 1", to_string=True)

        # need to check if already set to reuse the command
        EndOfMainBreakpoint()
        
        arch = gdb.newest_frame().architecture()

        global has_exited
        has_exited = False
        
        cpt = 0
        f = open('tracer.dump', 'w')
        while not has_exited:
            try:
                gdb.execute("stepi", to_string=True)
                cpt += 1

                pc = gdb.parse_and_eval("$pc")                
                disa = arch.disassemble(addr2num(pc))

                f.write(examine("$pc") + ' ' + disa[0]["asm"] + '\n')

            except Exception as e:
                print("Exception: {}".format(e))
                break
            except KeyboardInterrupt:
                print("Keyboard interrupted")
        print("Done: {}".format(what_happend))
        print("{} instructions executed".format(cpt))
        gdb.execute("quit")
TracerCommand()

def exited():
    print("Exit event")
    global has_exited, what_happend
    has_exited = True
    what_happend = "exit event"
    pass

class EndOfMainBreakpoint(gdb.Breakpoint):
    def __init__(self):
        current_frame = gdb.selected_frame()
        
        current_frame.older().select()
        return_pc = gdb.parse_and_eval("$pc") 
        current_frame.select()
        
        gdb.Breakpoint.__init__(self, "*{}".format(addr2num(return_pc), internal=True))
        self.silent = True

    def stop(self):
        global has_exited, what_happend
        has_exited = True
        print("Return from main breakpoint")
        what_happend = "end of main breakpoint"
        return False # should not be useful


TracerCommand()

def signal_handler(signal, frame):
    print('You pressed Ctrl+C!')
    global has_exited, what_happend
    has_exited = True
    what_happend = "signal"
signal.signal(signal.SIGINT, signal_handler)
