{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Test.Migrate (migrateTests) where

import Capnp.Gen.Request.Pure (Request(..))
import Capnp.Gen.Response.Pure (Response(..), Result(Result))
import Control.Exception (catch)
import Data.ByteString (ByteString)
import qualified Data.Vector as Vector
import Test.Hspec

import Database (setupDb)
import Expectations (shouldBeDone, shouldBeResult)
import Quill.Backend (Backend)
import qualified Quill.Backend as Backend
import Quill.Check.Migration (MigrationError(..))
import Quill.Query (QueryException(..))
import qualified Quill.Query as Query
import Quill.Syntax (TableItem(..), Type(..))
import Quill.Syntax.Migration (Migration(..), Command(..), FieldChange(..))
import qualified Quill.Syntax.Migration as Migration

exec :: Backend -> ByteString -> IO Response
exec backend = Backend.request backend . Request'exec

setupDbMigrate ::
  (Backend -> IO ()) ->
  (Backend -> IO ()) ->
  (Backend -> IO ()) ->
  IO ()
setupDbMigrate setup teardown =
  setupDb
    setup
    (\backend -> do
        teardown backend
        shouldBeDone =<< exec backend "DROP TABLE IF EXISTS quill_migrations;"
    )

tableShouldExist ::
  Backend ->
  ByteString -> -- table name
  [ByteString] -> -- column names
  IO ()
tableShouldExist backend tableName columnNames = do
  do
    Result rows cols data_ <-
      shouldBeResult =<<
      exec backend ("SELECT COUNT(*) FROM information_schema.tables WHERE table_name = '" <> tableName <> "';")
    rows `shouldBe` 1
    cols `shouldBe` 1
    data_ `shouldBe` [ [ "1" ] ]
  do
    Result rows cols data_ <-
      shouldBeResult =<<
      exec backend ("SELECT column_name FROM information_schema.columns WHERE table_name = '" <> tableName <> "';")
    rows `shouldBe` fromIntegral (length columnNames)
    cols `shouldBe` 1
    data_ `shouldBe` Vector.fromList ((\col -> [ col ]) <$> columnNames)

migrateTests :: Spec
migrateTests = do
  around
    (setupDbMigrate (const $ pure ()) (\backend -> shouldBeDone =<< exec backend "DROP TABLE IF EXISTS \"table1\";")) .
    describe "migrate success" $ do
      it "1" $ \backend -> do
        let
          migrations =
            [ Migration
              { _mName = Migration.Name "migration1"
              , _mParent = Nothing
              , _mCommands =
                [ CreateTable
                    "table1"
                    [ Field "field1" $ TInt ()
                    ]
                ]
              , _mInfo = ()
              }
            ]
        result <- Query.migrate backend migrations
        result `shouldBe` ()

        tableShouldExist backend "table1" ["field1"]

  around
    (setupDbMigrate (const $ pure ()) (\backend -> shouldBeDone =<< exec backend "DROP TABLE IF EXISTS \"table1\";")) .
    describe "migrate failure" $ do
      it "missing parent" $ \backend -> do
        let
          migrations =
            [ Migration
              { _mName = Migration.Name "migration1"
              , _mParent = Just $ Migration.Name "doesn'texist"
              , _mCommands =
                [ CreateTable
                    "table1"
                    [ Field "field1" $ TInt ()
                    ]
                ]
              , _mInfo = ()
              }
            ]
        catch
          (Query.migrate backend migrations *> expectationFailure "expected an error to be thrown")
          (\(e :: QueryException) ->
             e `shouldBe`
             MigrationErrors [MigrationNotFound $ Migration.Name "doesn'texist"]
          )
      it "trying to rewrite history" $ \backend -> do
        let
          migrations1 =
            [ Migration
              { _mName = Migration.Name "migration1"
              , _mParent = Nothing
              , _mCommands =
                [ CreateTable
                    "table1"
                    [ Field "field1" $ TInt ()
                    ]
                ]
              , _mInfo = ()
              }
            ]
          migrations2 =
            [ Migration
              { _mName = Migration.Name "migration1"
              , _mParent = Nothing
              , _mCommands =
                [ CreateTable
                    "table1"
                    [ Field "field2" $ TInt ()
                    ]
                ]
              , _mInfo = ()
              }
            , Migration
              { _mName = Migration.Name "migration2"
              , _mParent = Just $ Migration.Name "migration1"
              , _mCommands =
                [ AlterTable
                    "table1"
                    [ DropField "field2"
                    ]
                ]
              , _mInfo = ()
              }
            ]
        (`shouldBe` ()) =<< Query.migrate backend migrations1
        catch
          (Query.migrate backend migrations2 *> expectationFailure "expected an error to be thrown")
          (\(e :: QueryException) ->
             e `shouldBe`
             UnexpectedResponse (Response'error "Migration migration1 is registered with a different hash")
          )
