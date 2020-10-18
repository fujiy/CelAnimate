import 'electron'

import './three'
import './components/treeview'
import './components/contextmenu'
import './style/main.sass'
const { Elm } = require('./elm/Main.elm')

const mountNode = document.createElement('div');
document.body.appendChild(mountNode)

const app = Elm.Main.init({ node: mountNode });
