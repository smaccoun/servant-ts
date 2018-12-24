import * as React from 'react'
import {Html} from "elm-ts/lib/React";
import {Cmd, none} from "elm-ts/lib/Cmd";
import SyntaxHighlighter from 'react-syntax-highlighter';
import { Navbar, Box, Heading, Media, Columns, Level, Card } from "react-bulma-components/full";
import {getApiLiteral, getTSFunctions, getTSTypes} from "./server/api";


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
      <Level style={{margin: "12px"}}>
        <Level.Side align="left">
          <Content />
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
            <Navbar.Item><i className="fab fa-github"></i></Navbar.Item>
        </Navbar.Container>
      </Navbar.Menu>
  </Navbar>
)

const Content = () => (
    <Columns className="is-vcentered">
      <Columns.Column>
        <InputColumn/>
      </Columns.Column>
      <Columns.Column>
            <i className="fas fa-arrow-right fa-4x"></i>
      </Columns.Column>
      <Columns.Column>
        <ServantTSOutputBox />
      </Columns.Column>

    </Columns>
)

const InputColumn = () => (
  <Box>
    <Heading>Input</Heading>
    {APIBox()}
  </Box>
)

const APIBox = () => (
  displayCodeFiled("API.hs", getApiLiteral(), "typescript")
)

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


const displayCodeFiled = (filename: string, codeContent: string, language: string) => (
  displayFile(
    filename,
    (<SyntaxHighlighter language={language}>{codeContent}</SyntaxHighlighter>)
  )
)


function displayFile(filename: string, content: JSX.Element): JSX.Element {
  return (
    <Card>
      <Card.Header>
        <Card.Header.Title>
          <Media>
            <div className="media-left"><i className="fas fa-file"></i></div>
            <Media.Item><h3>{filename}</h3></Media.Item>
          </Media>
        </Card.Header.Title>
      </Card.Header>
      {content}
    </Card>
  )

}
