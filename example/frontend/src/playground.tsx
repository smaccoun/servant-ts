import * as React from 'react'
import {Html} from "elm-ts/lib/React";
import {Cmd, none} from "elm-ts/lib/Cmd";

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
      {`
        type SimpleAPI =
                 "user" :> Get '[JSON] [User]
            :<|> "user" :> Capture "userId" Int :> Get '[JSON] User
      `}
    </div>
  )
}