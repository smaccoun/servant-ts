export const getApiLiteral = (): string => (
  `data User = 
  User
    {name    :: Text
    ,age     :: Int
    ,isAdmin :: Bool
    ,hasMI   :: Maybe Text
    } deriving (Generic, TypescriptType)

type SimpleAPI =
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
