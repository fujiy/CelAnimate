
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

    attrChanged(name: string, value: any) { }
    willConnect() { }
    didConnect() { }
}


export class ThreeObject extends ThreeElement {
    object3d: THREE.Object3D
    parent: ThreeObject
    position_: number[] = [0, 0, 0]
    rotation_: number[] = [0, 0, 0]
    lookAt_: number[]

    constructor() {
        super();
        this.object3d = null;
    }
    static get observedAttributes(): string[] {
        return [];
    }
    disconnectedCallback() {
        if (!this.parentElement) {
            this.parent.object3d.remove(this.object3d)
        }
    }
    adoptedCallback() {
        console.log("adopted", this)
    }

    didConnect() {
        if (this.parentElement instanceof ThreeElement) {
            this.parent = this.parentElement as ThreeObject
            this.parent.add(this)
        }
        else {
            console.log("Parent element is not a three object",
                this.parentElement, this)
        }
        this.position = this.position_
        this.rotation = this.rotation_
        this.lookAt = this.lookAt_
    }
    add(object: ThreeObject) {
        this.object3d.add(object.object3d)
    }

    set position(v: number[]) {
        this.position_ = v || this.position_
        if (this.object3d) {
            this.object3d.position.fromArray(this.position_)
        }
    }
    set rotation(v: number[]) {
        this.rotation_ = v || this.rotation_
        if (this.object3d) {
            this.object3d.rotation.fromArray(this.rotation_)
        }
    }

    set lookAt(v: number[]) {
        this.lookAt_ = v
        if (this.object3d && v) {
            this.object3d.lookAt(v[0], v[1], v[2])
        }
    }
}

export class ThreeGroup extends ThreeObject {

    willConnect() {
        this.object3d = new THREE.Group()
    }
}

customElements.define('three-group', ThreeGroup);
