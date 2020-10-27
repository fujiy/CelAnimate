
import * as THREE from 'three';
import { CircleBufferGeometry } from 'three';

import { ThreeElement } from './element'

export class ThreeGeometry extends ThreeElement {
    geometry: THREE.BufferGeometry = null
    constructor() {
        super()
    }
}

export class BufferGeometry extends ThreeGeometry {
    _vertices: number[][] = []
    _faces: number[][] = []
    _uvs: number[][] = []

    maxVertices: number = 500
    maxFaces: number = 500

    positions: THREE.BufferAttribute
    indices: THREE.BufferAttribute
    mappings: THREE.BufferAttribute

    set vertices(points: number[][]) {
        this._vertices = points
        if (this.geometry) {
            points.forEach((point, i) => {
                this.positions.setXYZ(i, point[0], point[1], point[2])
            })
            this.positions.needsUpdate = true
        }
    }
    set faces(indices: number[][]) {
        this._faces = indices
        if (this.geometry) {
            indices.forEach((index, i) => {
                this.indices.setXYZ(i * 3, index[0], index[1], index[2])
            })
            this.indices.needsUpdate = true
            this.geometry.setDrawRange(0, indices.length * 3)
            this.geometry.computeBoundingSphere()
        }

    }
    set uvs(mappings: number [][]) { 
        this._uvs = mappings
        if (this.geometry) {
            mappings.forEach((mapping, i) => {
                this.mappings.setXY(i, mapping[0], mapping[1])
            })
            this.mappings.needsUpdate = true
        }
    }

    willConnect() {
        this.init()

        this.vertices = this._vertices
        this.faces = this._faces
        this.uvs = this._uvs
    }

    init() {
        this.geometry = new THREE.BufferGeometry()

        this.positions =
            new THREE.BufferAttribute(new Float32Array(this.maxVertices * 3), 3)
        this.geometry.setAttribute('position', this.positions)

        this.indices =
            new THREE.BufferAttribute(new Uint16Array(this.maxFaces * 3), 1)
        this.geometry.setIndex(this.indices)

        this.mappings =
            new THREE.BufferAttribute(new Float32Array(this.maxVertices * 2), 2)
        this.geometry.setAttribute('uv', this.mappings)

        this.geometry.setDrawRange(0, this._faces.length * 3)
    }
}

export class BoxGeometry extends ThreeGeometry {
    constructor() {
        super()
    }
    willConnect() {
        this.geometry = new THREE.BoxBufferGeometry(
            this.numAttr("width"),
            this.numAttr("float"),
            this.numAttr("depth"),
            this.numAttr("widthSegments"),
            this.numAttr("heightSegments"),
            this.numAttr("depthSegments"))
    }
}

export class PlaneGeometry extends ThreeGeometry {
    willConnect() {
        this.geometry = new THREE.PlaneBufferGeometry(
            this.numAttr("width"),
            this.numAttr("height"),
            this.numAttr("widthSegments"),
            this.numAttr("heightSegments"))
    }
}

export class CircleGeometry extends ThreeGeometry {
    willConnect() {
        this.geometry = new THREE.CircleBufferGeometry(
            this.numAttr("radius"),
            this.numAttr("segments"),
            this.numAttr("thetaStart"),
            this.numAttr("thetaLength"))
    }
}

export class EdgesGeometry extends ThreeGeometry {
    constructor() {
        super()
    }
    didConnect() {
        const geometry: ThreeGeometry = this.childIs(ThreeGeometry)
        this.geometry = new THREE.EdgesGeometry(geometry.geometry)
    }
}

customElements.define('geometry-buffer', BufferGeometry)
customElements.define('geometry-box', BoxGeometry)
customElements.define('geometry-plane', PlaneGeometry)
customElements.define('geometry-circle', CircleGeometry)
customElements.define('geometry-edges', EdgesGeometry)
