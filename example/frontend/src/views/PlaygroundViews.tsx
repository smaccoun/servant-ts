import {Html} from "elm-ts/lib/React";
import {displayCodeFiled} from "./RenderedFiles";
import {getApiLiteral, getTSFunctions, getTSTypes} from "../server/api";
import { Dropdown, Button, Navbar, Box, Heading, Media, Columns, Level, Card } from "react-bulma-components/full";
import * as React from "react";
import {Model, Msg, MsgTypes, ViewState} from "../playground";

export const Header = () => (
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
        <Navbar.Item href="https://github.com/smaccoun/servant-ts">
          <i className="fab fa-github fa-2x" ></i>
        </Navbar.Item>
      </Navbar.Container>
    </Navbar.Menu>
  </Navbar>
)

export function Content(viewState: ViewState): Html<Msg> {
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

export function InputColumn(model: Model): Html<Msg> {
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

export function viewFlavorMenu(flavor: string): Html<Msg> {
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

export const viewServantTSOutputBox = (flavor: string) => (
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


