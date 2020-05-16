# TODO

* Testing
  * When registering a migration in the migrations table, store a hash of (its Changes + the
    its parent hash). When the backend looks up a migration name to see if it should run
    the changes, also lookup the hash. If the hashes aren't equal, then the migrations have
    diverged, which is an error.
  * Queries aren't calling 'escapeIdentifier' on table names
  * Test that I can add a field and make it autoincrementable
  * Test that running a migration yields the same result as just creating the table from scratch
