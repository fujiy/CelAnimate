'use strict';

require('./index.html');

import { Elm } from '../elm/Main.elm';
const mountNode = document.getElementById('main');

console.log(Elm);
const app = Elm.Main.init({node: mountNode});
