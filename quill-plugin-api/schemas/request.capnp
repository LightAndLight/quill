@0xa3a72b9143c87ce3;

struct AutoIncrement {
  arg @0 :Data;
}

struct PrimaryKey {
  arg @0 :Data;
}

struct Other {
  name @0 :Data;
  args @1 :List(Data);
}

struct Constraint {
  union {
    autoIncrement @0 :AutoIncrement;
    primaryKey @1 :PrimaryKey;
    other @2 :Other;
  }
}

struct Column {
  name @0 :Data;
  type @1 :Data;
}

struct Table {
  name @0 :Data;
  columns @1 :List(Column);
  constraints @2 :List(Constraint);
}

# Requests are sent from the driver to the backend
struct Request {
  union {
    quit @0 :Void;
    exec @1 :Data;
    echo @2 :Data;
    createTable @3 :Table;
  }
}