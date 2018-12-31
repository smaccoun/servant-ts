import * as React from 'react'
import {Html} from "elm-ts/lib/React";
import {Cmd, none} from "elm-ts/lib/Cmd";
import { Dropdown, Button, Navbar, Box, Heading, Media, Columns, Level, Card } from "react-bulma-components/full";
import { Lens } from 'monocle-ts'
import {Content, Header} from "./views/PlaygroundViews";
import {perform, Task} from "elm-ts/lib/Task";

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
  CHOOSE_FLAVOR = 'CHOOSE_FLAVOR',
  ANIMATE_OUTPUT = 'ANIMATE_OUTPUT'
}

export type Msg =
    { type: MsgTypes.TOGGLE_SHOW_OUTPUT, shouldShow: boolean }
  | { type: MsgTypes.CHOOSE_FLAVOR, flavor: string }

export type AnimateOutputMsg = { type: MsgTypes.ANIMATE_OUTPUT }

const taskNone: Task<void> = new Task(() => Promise.resolve())

function showOutput(shouldShow: boolean): Msg {
  return {type: MsgTypes.TOGGLE_SHOW_OUTPUT, shouldShow}
}

function delay<A>(n: number, task: Task<A>): Task<A> {
  return new Task<A>(
    () =>
      new Promise(resolve => {
        setTimeout(() => {
          console.log("YO WE GONNA RESOLVE")
          task.run().then(resolve), n
        })
      })
  )
}

const t: (() => Task<boolean>) = () => delay(1000, new Task(() => Promise.resolve(true)))
const triggerRefreshShowOutput: (() => Cmd<Msg>) = () => perform(t(), t => {console.log("T! ", t); return showOutput(t)})


export function update(msg: Msg, model: Model): [Model, Cmd<Msg>] {
  console.log("MODEL: ", model)
  console.log("MSG: ", msg)
  switch (msg.type) {
    case MsgTypes.TOGGLE_SHOW_OUTPUT:
      const nextCmd = msg.shouldShow ? none : perform(t(), t => showOutput(t))
      return [showOutputL.set(msg.shouldShow)(model), nextCmd]
    case MsgTypes.CHOOSE_FLAVOR:
      const withFlavorChange = changeFlavorMenuStateL.set(msg.flavor)(model)
      return [showOutputL.set(false)(withFlavorChange), triggerRefreshShowOutput()]
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


