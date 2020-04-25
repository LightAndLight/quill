@0xb95ef7fb3a9fb3ab;

using Table = import "table.capnp".Table;
using Column = import "table.capnp".Column;
using Constraint = import "table.capnp".Constraint;

struct ColumnChange {
  union {
    setAutoIncrement @0 :Void;
    unsetAutoIncrement @1 :Void;
  }
}

struct AlterColumn {
  name @0 :Data;
  changes @1 :List(ColumnChange);
}

struct TableChange {
  union {
    addColumn @0 :Column;
    alterColumn @1 :AlterColumn;
    dropColumn @2 :Data;
    addConstraint @3 :Constraint;
  }
}

struct AlterTable {
  name @0 :Data;
  changes @1 :List(TableChange);
}

struct Command {
  union {
    createTable @0 :Table;
    alterTable @1 :AlterTable;
  }
}

struct Migration {
  name @0 :Data;
  parent :union {
    none @1 :Void;
    some @2 :Data;
  }
  commands @3 :List(Command);
}