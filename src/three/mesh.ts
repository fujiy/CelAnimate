
import * as THREE from 'three';

import {ThreeObject} from './element'
import {ThreeGeometry} from './geometry'
import {ThreeMaterial} from './material'


export class ThreeMesh extends ThreeObject {
    constructor() {
        super();
    }
    didConnect() {
        // const geometry =
        // Array.from(this.children).filter(
        //     e => { return e instanceof ThreeGeometry }
        // )[0] as ThreeGeometry
        const geometry: ThreeGeometry = this.childIs(ThreeGeometry)
        const material: ThreeMaterial = this.childIs(ThreeMaterial)
        this.object3d = this.object3d ||
            new THREE.Mesh(geometry.geometry, material.material);

        const animate = () => {
            requestAnimationFrame(animate);
        }
        animate();

        super.didConnect()
    }
}


customElements.define('three-mesh',   ThreeMesh);
