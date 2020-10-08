import * as THREE from 'three';

class ThreeCanvas extends HTMLElement {
    renderer: THREE.WebGLRenderer

    constructor() {
        super()
        this.renderer = new THREE.WebGLRenderer()
    }
    static get observedAttributes(): string[] {
        return ['width', 'height', 'scene-id']
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

    connectedCallback() {
        this.appendChild(this.renderer.domElement)

        const self = this

        const animate = function() {
            const scene  = self.scene
            const camera = scene.camera

            self.renderer.render(scene.object3d, camera.object3d)

            requestAnimationFrame(animate);
        };

        animate();

    }
}

class ThreeObject extends HTMLElement {
    object3d: THREE.Object3D

    constructor() {
        super();
        this.object3d = null;
    }
    connectedCallback() {
        (this.parentElement as ThreeObject).add(this);
    }
    add(object: ThreeObject) {
        this.object3d.add(object.object3d);
    }
}

class ThreeScene extends ThreeObject {
    constructor() {
        super();
        this.object3d = new THREE.Scene();
    }
    static get observedAttributes() {
        return ['camera-id'];
    }


    get camera(): ThreeCamera {
        return document.getElementById(
            this.getAttribute('camera-id')) as ThreeCamera
    }
        // [...this.children].map(e => { this.object.add(e.object); });
}

class ThreeCamera extends ThreeObject {
    object3d: THREE.Camera
    constructor() {
        super()
    }
}

class PerspectiveCamera extends ThreeCamera {
    object3d: THREE.PerspectiveCamera
    constructor() {
        super();
        this.object3d = new THREE.PerspectiveCamera(
            75, 1,
            0.1, 1000);
        this.object3d.position.z = 5;
    }
    attributeChangedCallback() {
        this.object3d.aspect =
            +this.getAttribute('width') / +this.getAttribute('height');
        this.object3d.updateProjectionMatrix();
    }
    connectedCallback() {
        this.attributeChangedCallback();
    }
}


class ThreeMesh extends ThreeObject {
    constructor() {
        super();
    }
    connectedCallback() {
        const geometry =
            Array.from(this.children).filter(
                e => { return e instanceof ThreeGeometry }
            )[0] as ThreeGeometry
        const material =
            Array.from(this.children).filter(
                e => { return e instanceof ThreeMaterial }
            )[0] as ThreeMaterial
        this.object3d = this.object3d ||
            new THREE.Mesh(geometry.geometry, material.material);

        super.connectedCallback();

        const animate = () => {

            this.object3d.rotation.x += 0.01;
            this.object3d.rotation.y += 0.01;
            requestAnimationFrame(animate);
        }
        animate();
    }
}

class ThreeGeometry extends HTMLElement {
    geometry: THREE.Geometry
    constructor() {
        super();
        this.geometry = null;
    }
}

class BoxGeometry extends ThreeGeometry {
    constructor() {
        super();
        this.geometry = new THREE.BoxGeometry(1, 1, 1);
    }
}

class ThreeMaterial extends HTMLElement {
    material: THREE.Material
    constructor() {
        super();
    }
    static get observedAttributes(): string[] {
        return [];
    }
}

class MeshBasicMaterial extends ThreeMaterial {
    material: THREE.MeshBasicMaterial
    constructor() {
        super();
        this.material = new THREE.MeshBasicMaterial();
    }
    static get observedAttributes(): string[] {
        return ThreeMaterial.observedAttributes.concat(['color'])
    }
    attributeChangedCallback() {
        // this.material.color = this.getAttribute('color');
        this.material.color = new THREE.Color(this.getAttribute('color'));
    }
}

customElements.define('three-canvas', ThreeCanvas);
customElements.define('three-scene',  ThreeScene);
customElements.define('three-mesh',   ThreeMesh);
customElements.define('camera-perspective', PerspectiveCamera);
customElements.define('geometry-box', BoxGeometry);
customElements.define('material-mesh-basic',  MeshBasicMaterial);
