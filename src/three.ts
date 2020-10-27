import * as THREE from 'three'
import Stats from 'stats-js'

import { ThreeElement, ThreeObject } from './three/element'
import { ThreeCamera } from './three/camera'
import './three/element'
import './three/camera'
import './three/mesh'
import './three/geometry'
import './three/material'


class ThreeCanvas extends ThreeElement {
    renderer: THREE.WebGLRenderer

    width: number
    height: number

    constructor() {
        super()
        this.renderer = new THREE.WebGLRenderer()
    }
    static get observedAttributes(): string[] {
        return ['width', 'height', 'auto-size']
    }
    attrChanged() {
    }

    get scenes(): ThreeScene[] {
        const ids = this.getAttribute('scene-id').split(' ')
        return ids.map(id => document.getElementById(id) as ThreeScene)
    }

    get camera(): ThreeCamera {
        const id = this.getAttribute('camera-id')
        if (id) {
            return document.getElementById(id) as ThreeCamera
        }
        else null
    }

    add(_: ThreeObject) {
        // nothing to do
    }

    resize(width: number, height: number) {
        this.width = width
        this.height = height
        this.renderer.setSize(width, height)
    }

    didConnect() {
        this.appendChild(this.renderer.domElement)
        this.renderer.domElement.style.position = "fixed"

        const observer = new window["ResizeObserver"](entries => {
            const rect = entries[0].contentRect
            this.resize(rect.width, rect.height)

            const event = new Event("resize")
            this.dispatchEvent(event)
        })
        observer.observe(this)

        if (this.attr("auto-size")) {
            const rect = this.getBoundingClientRect()
            this.resize(rect.width, rect.height)

            const event = new Event("resize")
            this.dispatchEvent(event)
        }

        var stats = new Stats();
        stats.showPanel(0);
        // document.body.appendChild(stats.dom);

        const animate = () => {
            // stats.begin();

            this.renderer.setClearColor(0x000000)
            this.renderer.autoClear = false

            for (const scene of this.scenes) {
                const camera = scene.camera || this.camera
                if (camera.autoAspect) {
                    camera.aspect = this.width / this.height
                }
                this.renderer.clearDepth()
                this.renderer.render(scene.object3d, camera.object3d)
            }

            // stats.end();

            requestAnimationFrame(animate)
        };

        animate();

    }
}

class ThreeScene extends ThreeObject {
    object3d: THREE.Scene

    constructor() {
        super();
        this.object3d = new THREE.Scene();
    }
    static get observedAttributes(): string[] {
        return super.observedAttributes.concat(['camera-id', 'background'])
    }

    attrChanged(name: string, value: any) {
        switch (name) {
            case "background":
                this.object3d.background = new THREE.Color(value);
        }
    }

    get camera(): ThreeCamera {
        const id = this.getAttribute('camera-id')
        if (id) {
            return document.getElementById(id) as ThreeCamera
        }
        else null
    }
    // [...this.children].map(e => { this.object.add(e.object); });
}

customElements.define('three-canvas', ThreeCanvas);
customElements.define('three-scene', ThreeScene);
