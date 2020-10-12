
import * as THREE from 'three';

import {ThreeObject} from './element'

export class ThreeCamera extends ThreeObject {
    object3d: THREE.Camera
    constructor() {
        super()
    }
    static get observedAttributes(): string[] {
        return [];
    }

}

export class PerspectiveCamera extends ThreeCamera {
    object3d: THREE.PerspectiveCamera
    constructor() {
        super();
        this.object3d = new THREE.PerspectiveCamera(
            75, 1,
            0.1, 1000);
        this.object3d.position.z = 2;
    }
    static get observedAttributes(): string[] {
        return super.observedAttributes.concat(['aspect', 'fov'])
    }
    attrChanged() {
        this.object3d.aspect = this.numAttr('aspect')
        this.object3d.fov = this.numAttr('fov')
        this.object3d.updateProjectionMatrix();
    }
}

customElements.define('camera-perspective', PerspectiveCamera);
