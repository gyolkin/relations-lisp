Start := Relations

Relations := { COMMENT "\n" } RelationDecl { "\n" (RelationDecl|COMMENT) }
RelationDecl := IDENT "=" Relation { COMMENT }

Relation := FunName
          | FunName "()"
          | FunName "(" Relation ")"
          | FunName "(" ("m"|"f") ")"                - m/f применяется по отношению к FunName
          | FunName "(" Relation "," ("m"|"f") ")"   - m/f применяется по отношению к FunName

FunName := IDENT | "spouse"| "parents" | "children"

IDENT - [a-zA-Zа-яА-Я_ё][a-zA-Z0-9а-яА-Я_ё]*
COMMENT - ;.*
