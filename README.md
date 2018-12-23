https://smaccoun.github.io/servant-ts/

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

Given a flavor configuration you can auto generate the following type and function declaration files for this API.

```Typescript
// Declarations

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
