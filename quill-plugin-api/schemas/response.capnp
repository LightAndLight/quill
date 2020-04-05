@0xa3fb1aad7b12a786;

# Responses are sent from the backend to the driver

struct Result {
  rows @0 :Int64;
  columns @1 :Int64;
  data @2 :List(List(Data));
}

struct Response {
  union {
    result @0 :Result;
    echo @1 :Data;
    error @2 :Data;
    done @3 :Void;
  }
}