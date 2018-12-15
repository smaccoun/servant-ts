import * as React from 'elm-ts/lib/React'
import { render } from 'react-dom'
import * as component from './playground'

console.log("HI THERE!")

const main = React.program(component.init, component.update, component.view)
React.run(main, dom => render(dom, document.getElementById('app')!))