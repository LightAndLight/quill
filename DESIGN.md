# Design

## Migrations

```
$ ls

$ quill init
Generated quillfiles/migrations/initial.quillm
Generated quillfiles/schema/schema.quill

$ tree
quillfiles
|-- migrations
    |-- initial.quillm
|-- schemas
    |-- schema.quill

$ cat quillfiles/schemas/schema.quill
table MyTable {
  id : Int, PK(id), AUTO_INCREMENT(id),
  my_field1 : Int,
  my_field2 : Bool,
  my_field3 : Text
}

$ cat quillfiles/migrations/initial.quillm
initial migration "20200406-initial" {
  commands: [
    create table MyTable {
      id : Int, PK(id), AUTO_INCREMENT(id),
      my_field1 : Int,
      my_field2 : Bool,
      my_field3 : Text
    }
  ]
}

$ vi quillfiles/schemas/schema.quill

$ cat quillfiles/schemas/schema.quill
table MyTable {
  id : Int, PK(id), AUTO_INCREMENT(id),
  my_field1 : Int,
  my_field2 : Bool,
  my_field3 : Text
  my_field4 : Binary
}

$ quill genmigrations --name "20200406-add-my-field4"
Generated quillfiles/migrations/20200406-add-my-field4.quillm

$ cat quillfiles/migrations/20200406-add-my-field4.quillm
migration "20200406-add-my-field4" {
  parents: ["20200406-initial"],
  commands: [
    alter table MyTable {
      add my_field4 : Binary
    }
  ]
}

$ rm quillfiles/schemas/schema.quill

$ quill genschema --name "schema"
Generated quillfiles/schemas/schema.quill

$ cat quillfiles/schemas/schema.quill
table MyTable {
  id : Int, PK(id), AUTO_INCREMENT(id),
  my_field1 : Int,
  my_field2 : Bool,
  my_field3 : Text,
  my_field4 : Binary
}

$ cat << EOF > quillfiles/migrations/20200406-update-MyTable.quillm
migration "20200406-update-MyTable" {
  parents = ["20200406-add-my-field4"],
  commands = [
    alter table MyTable {
      drop my_field1,
      alter my_field4(arg: Binary) -> Bool {
        False
      }
    }
  ]
}
EOF

$ quill genschema --name "schema"
Generated quillfiles/schemas/schema.quill

$ cat quillfiles/schemas/schema.quill
table MyTable {
  id : Int, PK(id), AUTO_INCREMENT(id),
  my_field2 : Bool,
  my_field3 : Text,
  my_field4 : Bool
}

$ ls quillfiles/migrations
initial.quillm
20200406-add-my-field4.quillm
20200406-update-MyTable.quillm

$ export QUILL_DB_HOST=199.54.19.234:5432

$ export QUILL_DB_NAME=thingo

$ export QUILL_DB_USER=isaac

$ export QUILL_DB_PASS=hunter2

$ quill -b postgres migrate 
Migration successful

$ quill -b postgres repl --raw
You're on your own now. Good luck.

psql> SELECT * FROM quill_migrations;
          name 
------------------------
initial
20200406-add-my-field4
20200406-update-MyTable
(3 rows)

psql> \d MyTable
   Table "public.mytable"
 Column    | Type    | Modifiers 
-----------+---------+-----------------------------------------------------
 id        | integer | not null default nextval('mytable_id_seq'::regclass)
 my_field2 | boolean | not null
 my_field3 | text    | not null
 my_field4 | boolean | not null

psql> \q

$ echo "We're done for now"
We're done for now
```
