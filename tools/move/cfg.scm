(All
 ((Filter (todo) (Folder hetzner/todo))
  (First
   ((Filter (spam) (Folder hetzner/spambucket))
    (Filter (trash) (Folder hetzner/Trash))
    (Filter (inbox)
     (First
      ((Filter (uibk) (Folder uibk/INBOX))
       (Filter (student) (Folder uibk-student/INBOX))
       (Folder hetzner/INBOX))))
    (Filter (sent)
     (All
      ((Filter (uibk) (Folder uibk/Sent))
       (Filter (student) (Folder uibk-student/Sent))
       (Filter (private) (Folder hetzner/Sent)))))
    (All
     ((Filter (notif) (Folder hetzner/notif))
      (Filter (order) (Folder hetzner/order))
      (Filter (invoice) (Folder hetzner/invoice))
      (Filter (masterthesis) (Folder hetzner/masterthesis))
      (Filter (booking) (Folder hetzner/booking))))
    (Folder hetzner/default)))))
