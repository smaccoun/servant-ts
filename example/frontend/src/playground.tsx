import * as React from 'react'
import {Html} from "elm-ts/lib/React";
import {Cmd, none} from "elm-ts/lib/Cmd";
import { Button, Navbar, Box, Heading, Media, Columns, Level, Card } from "react-bulma-components/full";
import {getApiLiteral, getTSFunctions, getTSTypes} from "./server/api";
import {displayCodeFiled} from "./views/RenderedFiles";

interface ViewState {
  isShowingOutput: boolean
}

export type Model = ViewState

export const init: [Model, Cmd<Msg>] = [{isShowingOutput: false}, none]

export type Msg = { type: 'toggleShowOutput' }

export function update(msg: Msg, model: Model): [Model, Cmd<Msg>] {
  switch (msg.type) {
    case 'toggleShowOutput':
      return [{isShowingOutput: !model.isShowingOutput}, none]
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
              <a href="https://github.com/smaccoun/servant-ts">
                <i className="fab fa-github fa-2x"></i>
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
        {InputColumn()(dispatch)}
      </Columns.Column>
      {viewState.isShowingOutput ?
        (<Columns className="is-vcentered">
          <Columns.Column>
            <i className="fas fa-arrow-right fa-4x"></i>
          </Columns.Column>
          <Columns.Column>
            <ServantTSOutputBox/>
          </Columns.Column>
        </Columns>
        )
          :
          <div></div>
        }


    </Columns>
  )
}

function InputColumn(): Html<Msg> {
  return dispatch => (
    <Box>
      <Heading>Input</Heading>
      {APIBox()(dispatch)}
    </Box>
    )
}

function APIBox(): Html<Msg> {
  return dispatch => (
    <div>
      <Level>
        <Level.Item>{displayCodeFiled("API.hs", getApiLiteral(), "typescript") }</Level.Item>
      </Level>
      <Level>
        <Level.Side align="left"></Level.Side>
        <Level.Side align="right">
          <Level.Item>
            <Button
              color="danger"
              onClick={() => dispatch({type: 'toggleShowOutput'})}>
              Run
            </Button>
          </Level.Item>
        </Level.Side>
      </Level>
    </div>
  )
}

const ServantTSOutputBox = () => (
  <div className="box">
    <Level>
      <Heading>Output</Heading>
    </Level>
    <Level style={{marginTop: "10px"}}>
    {displayCodeFiled(
      "Server/types.tsx",
      getTSTypes(),
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



