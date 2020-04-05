@0xa3a72b9143c87ce3;

# Requests are sent from the driver to the backend
struct Request {
  union {
    quit @0 :Void;
    exec @1 :Data;
    echo @2 :Data;
  }
}