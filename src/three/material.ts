
import * as THREE from 'three';

import {ThreeElement} from './element'


export class ThreeMaterial extends ThreeElement {
    material: THREE.Material

    static get observedAttributes(): string[] {
        return [] 
    }
}

export class LineBasicMaterial extends ThreeMaterial {
    material: THREE.LineBasicMaterial
    constructor() {
        super()
        this.material = new THREE.LineBasicMaterial()
    }
    static get observedAttributes(): string[] {
        return super.observedAttributes.concat(['color', 'linewidth'])
    }
    attrChanged(name: string, value: any) {
        console.log(this, this.material, name, value)
        switch (name) {
            case 'color':
                this.material.color = value && new THREE.Color(value);
                break
            case 'linewidth':
                this.material.linewidth = value;
                break
        }
    }
}

export class MeshBasicMaterial extends ThreeMaterial {
    material: THREE.MeshBasicMaterial
    constructor() {
        super();
        this.material = new THREE.MeshBasicMaterial();
    }
    static get observedAttributes(): string[] {
        return super.observedAttributes.concat(['color', 'wireframe'])
    }
    attrChanged(name: string, value: any) {
        switch (name) {
            case 'color':
                this.material.color = value && new THREE.Color(value);
                break
            case 'wireframe':
                this.material.wireframe = !!value
                break
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
        const src    = this.attr('src')
        const loader = new THREE.TextureLoader()
        this.texture = loader.load(src)
    }
}


customElements.define('material-mesh-basic',  MeshBasicMaterial);
customElements.define('material-line-basic',  LineBasicMaterial);
customElements.define('three-texture',  ThreeTexture);
