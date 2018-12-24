import SyntaxHighlighter from "react-syntax-highlighter";
import * as React from "react";
import { Navbar, Box, Heading, Media, Columns, Level, Card } from "react-bulma-components/full";

export const displayCodeFiled = (filename: string, codeContent: string, language: string) => (
  displayFile(
    filename,
    (<SyntaxHighlighter language={language}>{codeContent}</SyntaxHighlighter>)
  )
)


export function displayFile(filename: string, content: JSX.Element): JSX.Element {
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