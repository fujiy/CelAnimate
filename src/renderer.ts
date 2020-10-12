import  'electron'

import './three.ts'
const { Elm } = require('../elm/Main.elm')
import '../style/main.sass'

const mountNode = document.createElement('div');
document.body.appendChild(mountNode)

const app = Elm.Main.init({node: mountNode});
