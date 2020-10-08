import  'electron'

require('./three.ts')
const { Elm } = require('../elm/Main.elm')

const mountNode = document.createElement('div');
document.body.appendChild(mountNode)

const app = Elm.Main.init({node: mountNode});
