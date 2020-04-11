@0xa3a72b9143c87ce3;

struct Other {
  name @0 :Data;
  args @1 :List(Data);
}

struct Constraint {
  union {
    primaryKey @0 :List(Data);
    other @1 :Other;
  }
}

struct Column {
  name @0 :Data;
  type @1 :Data;
  notNull @2 :Bool;
  autoIncrement @3 :Bool;
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