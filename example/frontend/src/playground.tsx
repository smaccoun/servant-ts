import * as React from 'react'
import {Html} from "elm-ts/lib/React";
import {Cmd, none} from "elm-ts/lib/Cmd";
import { Dropdown, Button, Navbar, Box, Heading, Media, Columns, Level, Card } from "react-bulma-components/full";
import { Lens } from 'monocle-ts'
import {Content, Header} from "./views/PlaygroundViews";

export interface ViewState {
  isShowingOutput: boolean
  configuredFlavor: ConfiguredFlavor
}

const showOutputL = Lens.fromProp<ViewState>()('isShowingOutput')
const changeFlavorMenuStateL = Lens.fromProp<ViewState>()('configuredFlavor')

type ConfiguredFlavor = string

export type Model = ViewState

export const init: [Model, Cmd<Msg>] = [{isShowingOutput: false, configuredFlavor: 'FpTs'}, none]

export enum MsgTypes {
  TOGGLE_SHOW_OUTPUT = 'TOGGLE_SHOW_OUTPUT',
  CHOOSE_FLAVOR = 'CHOOSE_FLAVOR'
}

export type Msg =
    { type: MsgTypes.TOGGLE_SHOW_OUTPUT }
  | { type: MsgTypes.CHOOSE_FLAVOR, flavor: string }

export function update(msg: Msg, model: Model): [Model, Cmd<Msg>] {
  console.log("MODEL: ", model)
  console.log("MSG: ", msg)
  switch (msg.type) {
    case MsgTypes.TOGGLE_SHOW_OUTPUT:
      return [showOutputL.set(true)(model), none]
    case MsgTypes.CHOOSE_FLAVOR:
      return [changeFlavorMenuStateL.set(msg.flavor)(model), none]
  }
}

export function view(model: Model): Html<Msg> {
  return dispatch => (
    <div>
      <Header/>
      <Level style={{margin: "12px"}}>
        <Level.Side align="left">
          {Content(model)(dispatch)}
        </Level.Side>
      </Level>
    </div>
  )
}


