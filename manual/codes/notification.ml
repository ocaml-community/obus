open Notification

lwt () =
  lwt id = Notification.notify ~summary:"Hello, world!" () in
  return ()
