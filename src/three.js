import * as THREE from 'three';

class ThreeCanvas extends HTMLElement {
    constructor() {
        super();
        this.renderer = new THREE.WebGLRenderer();
    }
    static get observedAttributes() {
        return ['width', 'height', 'scene-id'];
    }
    attributeChangedCallback(name, oldValue, newValue) {
        this.renderer.setSize(this.getAttribute('width'),
                              this.getAttribute('height'));
    }
    get scene() {
        return document.getElementById(this.getAttribute('scene-id'));
    }
    add(object) {
        // nothing to do
    }

    connectedCallback() {
        this.appendChild(this.renderer.domElement);

        const self = this;

        const animate = function() {
            const scene  = self.scene;
            const camera = scene.camera;

            self.renderer.render(scene.object, camera.object);

            requestAnimationFrame(animate);
        };

        animate();

    }
}

class ThreeObject extends HTMLElement {
    constructor() {
        super();
        this.object = null;
    }
    connectedCallback() {
        this.parentElement.add(this.object);
    }
    add(object) {
        this.object.add(object);
    }
}

class ThreeScene extends ThreeObject {
    constructor() {
        super();
        this.object = new THREE.Scene();
    }
    static get observedAttributes() {
        return ['camera-id'];
    }
    attributeChangedCallback(name, oldValue, newValue) {

    }


    get camera() {
        return document.getElementById(this.getAttribute('camera-id'));
    }
        // [...this.children].map(e => { this.object.add(e.object); });
}


class PerspectiveCamera extends ThreeObject {
    constructor() {
        super();
        this.object = new THREE.PerspectiveCamera(
            75, 1,
            0.1, 1000);
        this.object.position.z = 5;
    }
    attributeChangedCallback(name, oldValue, newValue) {
        this.object.aspect =
            this.getAttribute('width') / this.getAttribute('height');
        this.object.updateProjectionMatrix();
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
              [...this.children].filter(
                  e => { return e instanceof ThreeGeometry; })[0];
        const material =
              [...this.children].filter(
                  e => { return e instanceof ThreeMaterial; })[0];
        this.object = this.object ||
            new THREE.Mesh(geometry.geometry, material.material);

        console.log(this.object);

        super.connectedCallback();

        const animate = () => {

            this.object.rotation.x += 0.01;
            this.object.rotation.y += 0.01;
            requestAnimationFrame(animate);
        }
        animate();
    }
}

class ThreeGeometry extends HTMLElement {
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
    constructor() {
        super();
    }
    static get observedAttributes() {
        return [];
    }
}

class MeshBasicMaterial extends ThreeMaterial {
    constructor() {
        super();
        this.material = new THREE.MeshBasicMaterial();
    }
    static get observedAttributes() {
        return [super.observedAttributes, ['color']].concat();
    }
    attributeChangedCallback(name, oldValue, newValue) {
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
