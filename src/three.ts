import * as THREE from 'three';

import {ThreeElement, ThreeObject} from './three/element'
import {ThreeCamera} from './three/camera'
import './three/element'
import './three/camera'
import './three/mesh'
import './three/geometry'
import './three/material'


class ThreeCanvas extends ThreeElement {
    renderer: THREE.WebGLRenderer

    constructor() {
        super()
        this.renderer = new THREE.WebGLRenderer()
    }
    static get observedAttributes(): string[] {
        return ['width', 'height']
    }
    attributeChangedCallback() {
        this.renderer.setSize(+this.getAttribute('width'),
                              +this.getAttribute('height'))
    }
    get scene(): ThreeScene {
        return document.getElementById(this.getAttribute('scene-id')) as ThreeScene
    }
    add(_: ThreeObject) {
        // nothing to do
    }

    didConnect() {
        this.appendChild(this.renderer.domElement)

        const animate = () => {
            const scene  = this.scene
            const camera = scene.camera

            this.renderer.render(scene.object3d, camera.object3d)

            requestAnimationFrame(animate)
        };

        animate();

    }
}

class ThreeScene extends ThreeObject {
    constructor() {
        super();
        this.object3d = new THREE.Scene();
    }
    static get observedAttributes(): string[] {
        return super.observedAttributes.concat(['camera-id'])
    }


    get camera(): ThreeCamera {
        return document.getElementById(
            this.getAttribute('camera-id')) as ThreeCamera
    }
        // [...this.children].map(e => { this.object.add(e.object); });
}

customElements.define('three-canvas', ThreeCanvas);
customElements.define('three-scene',  ThreeScene);
