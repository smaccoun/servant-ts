import {render} from "react-dom";
import * as React from 'elm-ts/lib/React'
import * as component from './playground'

const main = React.program(component.init, component.update, component.view)
React.run(main, dom => render(dom, document.getElementById('app')!))