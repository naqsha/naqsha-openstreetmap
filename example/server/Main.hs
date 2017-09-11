import Data.Default
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Naqsha.OpenStreetMap.Api

osmApi :: Proxy OSMApi
osmApi = Proxy

main = run 8000 app
app = serve osmApi $ do return (def :: Api)
