import * as THREE from 'three';

class ThreeElement extends HTMLElement {
    childrenCount: number = 0

    constructor() {
        super()
    }
    childIs<T extends ThreeElement>(cls: any): T | null {
        return Array.from(this.children).filter(
            e => { return e instanceof cls }
        )[0] as T || null

    }

    connectedCallback() {
        this.willConnect()

        setTimeout(() => {
            this.childrenCount =
                Array.from(this.children).filter(
                    e => { return e instanceof ThreeElement }).length

            if (this.childrenCount == 0) {
                this.connectedBubble()
            }
        })
    }
    connectedBubble() {
        if (this.childrenCount <= 1) {
            this.didConnect()
            if (this.parentElement instanceof ThreeElement) {
                this.parentElement.connectedBubble()
            }
        }
        else this.childrenCount--
    }

    willConnect() {}
    didConnect() {}
}

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

    willConnect() {
        this.appendChild(this.renderer.domElement)

        const self = this

        const animate = function() {
            const scene  = self.scene
            const camera = scene.camera

            self.renderer.render(scene.object3d, camera.object3d)

            requestAnimationFrame(animate)
        };

        animate();

    }
}

class ThreeObject extends ThreeElement {
    object3d: THREE.Object3D

    constructor() {
        super();
        this.object3d = null;
    }
    static get observedAttributes(): string[] {
        return [];
    }
    didConnect() {
        (this.parentElement as ThreeObject).add(this)
    }
    add(object: ThreeObject) {
        this.object3d.add(object.object3d)
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

class ThreeCamera extends ThreeObject {
    object3d: THREE.Camera
    constructor() {
        super()
    }
    static get observedAttributes(): string[] {
        return [];
    }

}

class PerspectiveCamera extends ThreeCamera {
    object3d: THREE.PerspectiveCamera
    constructor() {
        super();
        this.object3d = new THREE.PerspectiveCamera(
            75, 1,
            0.1, 1000);
        this.object3d.position.z = 2;
    }
    static get observedAttributes(): string[] {
        return super.observedAttributes.concat(['width', 'height'])
    }
    attributeChangedCallback() {
        this.object3d.aspect =
            +this.getAttribute('width') / +this.getAttribute('height')
        this.object3d.updateProjectionMatrix();
    }
    willConnect() {
        this.attributeChangedCallback();
    }
}


class ThreeMesh extends ThreeObject {
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

class ThreeGeometry extends ThreeElement {
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

class ThreeMaterial extends ThreeElement {
    material: THREE.Material
    constructor() {
        super()
    }
    static get observedAttributes(): string[] {
        return []
    }
}

class MeshBasicMaterial extends ThreeMaterial {
    material: THREE.MeshBasicMaterial
    constructor() {
        super();
        this.material = new THREE.MeshBasicMaterial();
    }
    static get observedAttributes(): string[] {
        return super.observedAttributes.concat(['color'])
    }
    attributeChangedCallback() {
        // this.material.color = this.getAttribute('color');
        if (this.getAttribute('color')) {
            this.material.color = new THREE.Color(this.getAttribute('color'));
        }
    }
    didConnect() {
        const texture: ThreeTexture = this.childIs(ThreeTexture)
        if (texture) this.material.map = texture.texture
    }
}

class ThreeTexture extends ThreeElement {
    texture: THREE.Texture

    constructor() {
        super();
    }
    willConnect() {
        const src    = this.getAttribute('src')
        const loader = new THREE.TextureLoader()
        this.texture = loader.load(src)
    }
}

customElements.define('three-canvas', ThreeCanvas);
customElements.define('three-scene',  ThreeScene);
customElements.define('three-mesh',   ThreeMesh);
customElements.define('camera-perspective', PerspectiveCamera);
customElements.define('geometry-box', BoxGeometry);
customElements.define('material-mesh-basic',  MeshBasicMaterial);
customElements.define('three-texture',  ThreeTexture);
