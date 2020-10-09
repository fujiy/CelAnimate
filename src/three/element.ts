
import * as THREE from 'three';

export class ThreeElement extends HTMLElement {
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


export class ThreeObject extends ThreeElement {
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

