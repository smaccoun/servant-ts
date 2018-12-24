export const getApiLiteral = (): string => (
  `type SimpleAPI =
         "user" :> Get '[JSON] [User]
    :<|> "user" :> Capture "userId" Int :> Get '[JSON] User
  `
)


export const getTSTypes = (): string => (
  `interface User {
    name : string
    age : number
    isAdmin : boolean
    hasMI : Option<string>
}
  `
)


export const getTSFunctions = (): string => (
  `function getUser(): Promise<Array<User>> {
  return fetch(withRemoteBaseUrl(\`user\`))
}

function getUserByUserId(userId : number): Promise<User> {
  return fetch(withRemoteBaseUrl(\`user/\${userId}\`))
}
  `
)
