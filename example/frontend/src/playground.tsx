import * as React from 'react'
import {Html} from "elm-ts/lib/React";
import {Cmd, none} from "elm-ts/lib/Cmd";
import SyntaxHighlighter from 'react-syntax-highlighter';

export type Model = number

export const init: [Model, Cmd<Msg>] = [0, none]

export type Msg = { type: 'Increment' } | { type: 'Decrement' }

export function update(msg: Msg, model: Model): [Model, Cmd<Msg>] {
  switch (msg.type) {
    case 'Increment':
      return [model + 1, none]
    case 'Decrement':
      return [model - 1, none]
  }
}

export function view(model: Model): Html<Msg> {
  return dispatch => (
    <div>
      <Header/>
      <div className="level">
        <div className="level-left"><Content /></div>
      </div>
    </div>
  )
}

const Header = () => (
    <section className="hero is-primary">
        <div className="hero-body">
            <div className="container">
                <h1 className="title">
                  Servant TS
                </h1>
            </div>
        </div>
    </section>
)

const Content = () => (
    <div className="columns is-vcentered">
      <div className="column"><APIBox/></div>
      <div className="column">
            <i className="fas fa-arrow-right fa-4x"></i>
      </div>
      <div className="column">
        <ServantTSOutputBox />
      </div>

    </div>
)

const APIBox = () => (
  <div className="box" style={{display: 'flex', flexDirection: 'column'}}>
    <h1>API</h1>
    <SyntaxHighlighter language='haskell'>
      {`
        type SimpleAPI =
                 "user" :> Get '[JSON] [User]
            :<|> "user" :> Capture "userId" Int :> Get '[JSON] User
      `}
    </SyntaxHighlighter>
  </div>
)

const ServantTSOutputBox = () => (
  <div className="box" style={{display: 'flex', flexDirection: 'column', width: '100%'}}>
    <div className="box" style={{display: 'flex', flexDirection: 'column', width: '100%', alignItems: 'center'}}>
      <h4>Server/types.tsx</h4>
      <SyntaxHighlighter language='typescript'>
        {`
          interface User {
            name : string
            age : number
            isAdmin : boolean
            hasMI : Option<string>
          }
        `}
      </SyntaxHighlighter>
    </div>
    <div className="box" style={{display: 'flex', flexDirection: 'column', width: '100%', alignItems: 'center'}}>
      <h4>Server/api.tsx</h4>
      <SyntaxHighlighter language='typescript'>
        {`
          function getUser(): Promise<Array<User>> {
            return fetch(withRemoteBaseUrl(\`user\`))
          }

          function getUserByUserId(userId : number): Promise<User> {
            return fetch(withRemoteBaseUrl(\`user/\${userId}\`))
          }
        `}
      </SyntaxHighlighter>
    </div>
    </div>
)
