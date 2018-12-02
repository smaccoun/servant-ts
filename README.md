# servant-ts

[![CircleCI](https://circleci.com/gh/smaccoun/servant-ts.svg?style=svg)](https://circleci.com/gh/smaccoun/servant-ts)

WIP. Transitioning to this project as I finalize the API for [aeson-generic-ts](https://github.com/smaccoun/aeson-generic-ts/commit/42153bf1bfa8c0f8427dd2410b5b73a8ce597b45)

It is technically usable right now, but the API is fast moving and the configurability limited. See the tests folder for some examples and roughly what this is going to turn into

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

function getUser(): Promise<User> {
  return fetch(withBaseUrl(`user`))
}

function getUserByUserId(userId : number): Promise<Array<User>> {
  return fetch(withBaseUrl(`user/${userId}`))
}
```


