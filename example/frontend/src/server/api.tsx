export const getApiLiteral = (flavor: string): string => (
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

main :: IO ()
main = 
  apiToTSDocs asTS reqToTSFunction outputFileLocs
  where
    outputFileLocs = OutputFileNames "Server/types.tsx" "Server/api.tsx"
    asTS = servantToReqTS (Proxy :: Proxy ${flavor}) (Proxy :: Proxy SimpleAPI)
    reqToTSFunction = defaultReqToTSFunction (Proxy @Fetch)
  `
)


export function getTSTypes(flavor: string): string {
  switch(flavor){
    case 'FpTs': {
      return `interface User {
    name : string
    age : number
    isAdmin : boolean
    hasMI : Option<string>
}
  `
  }
    default: {
      return `interface User {
    name : string
    age : number
    isAdmin : boolean
    hasMI : string | null 
}
  `
    }


  }

}


export const getTSFunctions = (): string => (
  `function getUser(): Promise<Array<User>> {
  return fetch(withRemoteBaseUrl(\`user\`))
}

function getUserByUserId(userId : number): Promise<User> {
  return fetch(withRemoteBaseUrl(\`user/\${userId}\`))
}
  `
)


