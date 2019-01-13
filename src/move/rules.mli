type tag = string
type selector = tag list

(** The rule data structure resembles a decision tree with priotized children.
 *  Folder <name> corresponds to a leave. Messages reaching this leave are
 *    placed in folder <name>.
 *  Filter <f> filters messages by properties like tags or age.
 *  All <lst> considers all children as potential storage destinations.
 *  First <lst> applies only the first children that acctually adds a folder.
 *  FolderPerTag <prefix> generates Filter (<tag>) (Folder <prefix>/<tag>) rules
 *    for all available tags
 *)
type t =
  | Folder of string
  | Filter of selector * t
  | All    of t list
  | First  of t list
  | FolderPerTag of string

val from_file : string -> t
