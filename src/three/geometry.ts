
import * as THREE from 'three';

import {ThreeElement} from './element'

export class ThreeGeometry extends ThreeElement {
    geometry: THREE.Geometry
    constructor() {
        super();
        this.geometry = null;
    }
}

export class BoxGeometry extends ThreeGeometry {
    constructor() {
        super();
        this.geometry = new THREE.BoxGeometry(1, 1, 1);
    }
}

customElements.define('geometry-box', BoxGeometry);
