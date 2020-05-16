@0xb95ef7fb3a9fb3ab;

using Table = import "table.capnp".Table;
using Column = import "table.capnp".Column;
using Constraint = import "table.capnp".Constraint;

struct TableChange {
  union {
    addColumn @0 :Column;
    dropColumn @1 :Data;
    addConstraint @2 :Constraint;
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

struct ParentInfo {
  name @0 :Data;
  hash @1 :Data;
}

struct Migration {
  name @0 :Data;
  hash @1 :Data;
  parent :union {
    none @2 :Void;
    some @3 :ParentInfo;
  }
  commands @4 :List(Command);
}