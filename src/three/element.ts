
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

    attr(name: string): string {
        return this.getAttribute(name)
    }
    numAttr(name: string): number {
        const num = this.getAttribute(name)
        return num ? +num : undefined
    }

    attributeChangedCallback(name: string, _oldValue: any, newValue: any) {
        this.attrChanged(name, newValue)
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

    attrChanged(name: string, value: any) {}
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

    set position(v) {
        if (this.object3d) {
            this.object3d.position.x = v[0]
            this.object3d.position.y = v[1]
            this.object3d.position.z = v[2]
        }
    }
}

