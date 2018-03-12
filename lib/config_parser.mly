%token <string> KEY
%token <string> VALUE
%token LEFT_BRACK
%token RIGHT_BRACK
%token EOF

%start <Config_type.t> cfg

%%

cfg:
  | EOF                    { [] }
  | l = section_list ; EOF { l }
  ;

section_list:
  | s = section ; l = section_list { s :: l }
  | s = section                    { [s] }
  ;

section:
  | h = section_head ; l = item_list { (h , l) }
  ;

section_head:
  | LEFT_BRACK; h = KEY ; RIGHT_BRACK { h }
  ;

item_list:
  | i = item ; l = item_list { i :: l }
  | i = item                 { [i] }
  ;

item:
  | key = KEY ; value = VALUE { (key, value) }
  | key = KEY                 { (key, "") }
  ;
