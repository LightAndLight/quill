@0xa3fb1aad7b12a786;

# Responses are sent from the backend to the driver
struct Response {
  union {
    done @0 :Void;
    rows @1 :List(List(Data));
  }
}