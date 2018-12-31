# Example

Consider the following common User API

```haskell

type UserAPI = "user" :> Get '[JSON] [User]
              :<|> "user" :> Capture "userId" Int :> Get '[JSON] User

data User = User
    {name    :: Text
    ,age     :: Int
    ,isAdmin :: Bool
    ,hasMI   :: Maybe Text
    } deriving (Generic, TypescriptType)

```

Given a flavor configuration you can auto generate with the fullowing function the below typescript files.

```
main :: IO ()
main = 
  apiToTSDocs asTS reqToTSFunction outputFileLocs
  where
    outputFileLocs = OutputFileNames "Server/types.tsx" "Server/api.tsx"
    asTS = servantToReqTS (Proxy :: Proxy FpTs) (Proxy :: Proxy SimpleAPI)
    reqToTSFunction = defaultReqToTSFunction (Proxy @Fetch)
```

```Typescript
// Declarations

interface User { 
  name : string
  age : number
  isAdmin : boolean
  hasMI : Option<string>
}
Array<User>
interface User { 
  name : string
  age : number
  isAdmin : boolean
  hasMI : Option<string>
}
```

```Typescript
// Function Declarations

function getUser(): Promise<Array<User>> {
  return fetch(withRemoteBaseUrl(`user`))
}
function getUserByUserId(userId : number): Promise<User> {
  return fetch(withRemoteBaseUrl(`user/${userId}`))
}
```
