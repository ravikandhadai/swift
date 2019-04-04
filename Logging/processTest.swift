import Foundation
import os

@_silgen_name("_NSGetEnviron")
func _NSGetEnviron() -> UnsafeMutablePointer<UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>>

var environ: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?> {
  return _NSGetEnviron().pointee
}

func processTest() {

  // do some logging
  let h = OSLog(subsystem: "mysubsys", category: "testcat")
  os_log(.default, log: h, "hello this is some message")
  os_log(.default, log: h, "A message with data %lld", 10)
  print("made log calls!!")

  var child: pid_t = 0

  let envp = environ
  do {
    var childActions: posix_spawn_file_actions_t?
    guard posix_spawn_file_actions_init(&childActions) == 0 else {
      fatalError("posix_spawn_file_actions_init failed")
    }
    defer { posix_spawn_file_actions_destroy(&childActions) }

    // Spawn the log show process.
    let args = ["/usr/bin/log", "show", "--predicate", "subsystem == \"mysubsys\""]
    let argv: [UnsafeMutablePointer<CChar>?] = args.map{ $0.withCString(strdup) }
    defer { for case let arg? in argv { free(arg) } }

    guard posix_spawn(&child,
                      argv[0],
                      &childActions,
                      nil,
                      argv + [nil], envp) == 0 else {
      fatalError("posix_spawn failed")
    }
  }

    var status: Int32 = 0
    let pid = wait(&status)
    if pid == -1 {
      // If the error was EINTR, just wait again.
      //if errno == EINTR { continue }
      // If we have no children to wait for, stop.
      //if errno == ECHILD { break }
      fatalError("Pid is -1")
    }

  // TODO: kill the child process.
  kill(child, 9)
}

processTest()
