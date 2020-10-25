
import * as THREE from 'three';

import { ThreeObject } from './element'

export class ThreeCamera extends ThreeObject {
    object3d: THREE.Camera
    autoAspect: boolean = false

    constructor() {
        super()
    }
    static get observedAttributes(): string[] {
        return [];
    }

    set aspect(value: number) {

    }

}

export class PerspectiveCamera extends ThreeCamera {
    object3d: THREE.PerspectiveCamera
    constructor() {
        super();
        this.object3d = new THREE.PerspectiveCamera(
            75, 1,
            0.1, 1000);
    }
    static get observedAttributes(): string[] {
        return super.observedAttributes.concat(['aspect', 'fov', 'auto-aspect'])
    }
    attrChanged() {
        this.autoAspect = !!this.attr('auto-aspect')
        if (!this.autoAspect)
            this.object3d.aspect = this.numAttr('aspect') || 1
        this.object3d.fov = this.numAttr('fov')
        this.object3d.updateProjectionMatrix();
    }

    set aspect(value: number) {
        this.object3d.aspect = value
        this.object3d.updateProjectionMatrix();
    }
}

customElements.define('camera-perspective', PerspectiveCamera);
