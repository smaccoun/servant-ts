import * as React from 'react'
import {Html} from "elm-ts/lib/React";
import {Cmd, none} from "elm-ts/lib/Cmd";
import { Dropdown, Button, Navbar, Box, Heading, Media, Columns, Level, Card } from "react-bulma-components/full";
import {getApiLiteral, getTSFunctions, getTSTypes} from "./server/api";
import {displayCodeFiled} from "./views/RenderedFiles";
import { Lens, Optional } from 'monocle-ts'

interface ViewState {
  isShowingOutput: boolean
  configuredFlavor: ConfiguredFlavor
}

const showOutputL = Lens.fromProp<ViewState>()('isShowingOutput')
const changeFlavorMenuStateL = Lens.fromProp<ViewState>()('configuredFlavor')

type ConfiguredFlavor = string

export type Model = ViewState

export const init: [Model, Cmd<Msg>] = [{isShowingOutput: false, configuredFlavor: 'FpTs'}, none]

enum MsgTypes {
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

const Header = () => (
  <Navbar color="info">
      <Navbar.Brand>
        <Navbar.Item>
          <h1 className="title">
            Servant TS
          </h1>
        </Navbar.Item>
      </Navbar.Brand>
      <Navbar.Menu>
        <Navbar.Container position="end">
            <Navbar.Item>
              <a className="fab fa-github fa-2x" href="https://github.com/smaccoun/servant-ts">
                <i ></i>
              </a>
            </Navbar.Item>
        </Navbar.Container>
      </Navbar.Menu>
  </Navbar>
)

function Content(viewState: ViewState): Html<Msg> {
  return dispatch => (
    <Columns className="is-vcentered">
      <Columns.Column>
        {InputColumn(viewState)(dispatch)}
      </Columns.Column>
      {viewState.isShowingOutput ?
        (<Columns className="is-vcentered">
          <Columns.Column>
            <i className="fas fa-arrow-right fa-4x"></i>
          </Columns.Column>
          <Columns.Column>
            {viewServantTSOutputBox(viewState.configuredFlavor)}
          </Columns.Column>
        </Columns>
        )
          :
          <div></div>
        }


    </Columns>
  )
}

function InputColumn(model: Model): Html<Msg> {
  return dispatch => (
    <Box>
      <Heading>Input</Heading>
      {APIBox(model)(dispatch)}
    </Box>
    )
}

function APIBox(model: Model): Html<Msg> {
  return dispatch => (
    <div>
      <Level>
        <Level.Item>{displayCodeFiled("API.hs", getApiLiteral(), "haskell") }</Level.Item>
      </Level>
      <Level>
        <Level.Side align="left"></Level.Side>
        <Level.Side align="right">
          <Level.Item>
            {viewFlavorMenu(model.configuredFlavor)(dispatch)}
          </Level.Item>
          <Level.Item>
            <Button
              color="danger"
              onClick={() => dispatch({type: MsgTypes.TOGGLE_SHOW_OUTPUT})}>
              Run
            </Button>
          </Level.Item>
        </Level.Side>
      </Level>
    </div>
  )
}

function viewFlavorMenu(flavor: string): Html<Msg> {
  return dispatch => (
    <Dropdown value={flavor} onChange={(flavor) => dispatch({type: MsgTypes.CHOOSE_FLAVOR, flavor })}>
      <Dropdown.Item value="FpTs">
        FpTs
      </Dropdown.Item>
      <Dropdown.Item value="Vanilla">
        Vanilla
      </Dropdown.Item>
    </Dropdown>
  )
}

const viewServantTSOutputBox = (flavor: string) => (
  <div className="box">
    <Level>
      <Heading>Output</Heading>
    </Level>
    <Level style={{marginTop: "10px"}}>
    {displayCodeFiled(
      "Server/types.tsx",
      getTSTypes(flavor),
      "typescript"
    )}
    </Level>
    <Level>
      {displayCodeFiled(
        "Server/api.tsx",
        getTSFunctions(),
        "typescript"
        )}
    </Level>
    </div>
)



