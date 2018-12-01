# servant-ts

[![CircleCI](https://circleci.com/gh/smaccoun/servant-ts.svg?style=svg)](https://circleci.com/gh/smaccoun/servant-ts)

WIP. Transitioning to this project as I finalize the API for [aeson-generic-ts](https://github.com/smaccoun/aeson-generic-ts/commit/42153bf1bfa8c0f8427dd2410b5b73a8ce597b45)

It is technically usable right now, but the API is fast moving and the configurability limited. See the tests folder for some examples and roughly what this is going to turn into

# Example

Given the following Servant API

```haskell
type SimpleAPI = "user" :> Get '[JSON] [User]
              :<|> "user" :> Capture "userId" Int :> Get '[JSON] User

```

You can specify a flavor (in this case FpTs) to generate function and type declarations

The declaration file would look like

```Typescript
// Declarations

interface User { 
  name : string
  age : number
  isAdmin : boolean
  hasMI : Option<string>
}
```

And the function file could look like

```Typescript
// Function Declarations

function getUser(): Promise<User> {
  return fetch(`${user}`)
}

function getUserByUserId(userId : number): Promise<Array<User>> {
  return fetch(`user/${userId}`)
}
```


