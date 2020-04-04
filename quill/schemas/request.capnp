@0xa3a72b9143c87ce3;

# Requests are from the driver to the backend
struct Request {
  union {
    quit @0 :Void;
    exec @1 :Data;
  }
}