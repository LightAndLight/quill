@0xf9e99c62538eec08;

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