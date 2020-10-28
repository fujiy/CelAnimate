
import * as THREE from 'three';

import { ThreeObject } from './element'
import { ThreeGeometry } from './geometry'
import { ThreeMaterial } from './material'


export class ThreeMesh extends ThreeObject {
    object3d: THREE.Mesh
    didConnect() {
        const geometry: ThreeGeometry = this.childIs(ThreeGeometry)
        const material: ThreeMaterial = this.childIs(ThreeMaterial)
        if (!geometry.geometry) return
        this.object3d = this.object3d ||
            new THREE.Mesh(geometry.geometry,
                material && material.material)

        super.didConnect()
    }
    updateGeometry(geometry: ThreeGeometry) {
        this.object3d.geometry = geometry.geometry
    }
}

export class ThreeLineSegments extends ThreeObject {
    object3d: THREE.Line
    didConnect() {
        const geometry: ThreeGeometry = this.childIs(ThreeGeometry)
        const material: ThreeMaterial = this.childIs(ThreeMaterial)
        if (!geometry.geometry) return
        this.object3d = this.object3d ||
            new THREE.LineSegments(geometry.geometry,
                material && material.material)

        super.didConnect()
    }
    updateGeometry() {
        // console.log(this.childIs(ThreeGeometry), this)
        this.object3d.geometry =
            (this.childIs(ThreeGeometry) as ThreeGeometry).geometry
    }
}

export class AxesHelper extends ThreeObject {
    willConnect() {
        this.object3d = new THREE.AxesHelper(this.numAttr("size"))
    }
}

customElements.define('three-mesh', ThreeMesh);
customElements.define('three-line-segments', ThreeLineSegments);
customElements.define('axes-helper', AxesHelper);
