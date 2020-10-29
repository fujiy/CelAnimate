// import 'electron'

require('@fortawesome/fontawesome')
require('@fortawesome/fontawesome-free-solid')
require('@fortawesome/fontawesome-free-regular')
require('@fortawesome/fontawesome-free-brands')
// require("font-awesome-webpack")

import './three'
import './components/treeview'
import './components/contextmenu'
import './components/slidertrack'
import './style/main.sass'


const { Elm } = require('./elm/Main.elm')

const mountNode = document.createElement('div');
document.body.appendChild(mountNode)
const app = Elm.Main.init.call(window, { node: mountNode });
