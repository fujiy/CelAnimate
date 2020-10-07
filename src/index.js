'use strict';

require('./index.html');
require('./three.js');

import { Elm } from '../elm/Main.elm';
const mountNode = document.getElementById('main');

const app = Elm.Main.init({node: mountNode});
