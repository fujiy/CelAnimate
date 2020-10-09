
import * as THREE from 'three';

import {ThreeElement} from './element'


export class ThreeMaterial extends ThreeElement {
    material: THREE.Material
    constructor() {
        super()
    }
    static get observedAttributes(): string[] {
        return []
    }
}

export class MeshBasicMaterial extends ThreeMaterial {
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

export class ThreeTexture extends ThreeElement {
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


customElements.define('material-mesh-basic',  MeshBasicMaterial);
customElements.define('three-texture',  ThreeTexture);
