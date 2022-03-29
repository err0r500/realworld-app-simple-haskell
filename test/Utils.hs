module Utils where

import qualified Data.UUID as UUID
import qualified Domain.User as D
import RIO

-- used for tests
fakeUUID1 :: UUID.UUID
fakeUUID1 = getUUID "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"

fakeUUID2 :: UUID.UUID
fakeUUID2 = getUUID "61b4ea9a-cfdb-44cc-b40b-affffeedc14e"

getUUID :: Text -> UUID.UUID
getUUID txt = do
  let Just uuid = UUID.fromText txt
  uuid

userUUID = fakeUUID1

user = D.User userUUID (D.Name "userName") (D.Email "user@email.com")

emptyPassword = D.Password ""

otherUser = D.User fakeUUID2 (D.Name "otherUserName") (D.Email "otherUser@email.com")
