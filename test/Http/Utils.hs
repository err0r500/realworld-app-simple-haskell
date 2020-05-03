module Http.Utils where

import           RIO
import qualified RIO.ByteString.Lazy           as LB
import           Network.HTTP.Types
import qualified Network.Wai.Test              as W
import           Test.Hspec.Wai

postSimpleJSON :: ByteString -> LB.ByteString -> WaiSession st W.SResponse
postSimpleJSON path = request methodPost path [(hContentType, "application/json")]
