

export class SliderTrack extends HTMLElement {

    value: number = 0
    min: number = 0
    max: number = 100
    step: number = 10

    _down: boolean = false

    moveEvent: any
    upEvent: any

    static get observedAttributes(): string[] {
        return ['value', 'min', 'max', 'step'];
    }
    attributeChangedCallback(name: string, _oldValue: any, value: any) {
        switch (name) {
            case 'value':
                this.value = +value
                break;
            case 'min':
                this.min = +value
                break;
            case 'max':
                this.max = +value
                break;
            case 'step':
                this.step = +value
        }
    }
    connectedCallback() {
        this.addEventListener('pointerdown', e => {
            this.value = this.calcValue(e)
            this._down = true
            const event = new Event("change")
            this.dispatchEvent(event)
        })
        this.moveEvent = e => {
            if (this._down) {
                this.value = this.calcValue(e)
                const event = new Event("change")
                this.dispatchEvent(event)
            }
        }
        this.upEvent = e => {
            this._down = false
        }
        document.addEventListener('pointermove',   this.moveEvent)
        document.addEventListener('pointerup',     this.upEvent)
        document.addEventListener('pointercancel', this.upEvent)
    }
    disconnectedCallback() {
        document.removeEventListener('pointermove',   this.moveEvent)
        document.removeEventListener('pointerup',     this.upEvent)
        document.removeEventListener('pointercancel', this.upEvent)
    }

    calcValue(e: MouseEvent): number {
        const rect = this.getBoundingClientRect()
        const x = e.clientX - rect.left
        var value =
            x / rect.width * (this.max - this.min) + this.min
        if (value < this.min) value = this.min
        if (value > this.max) value = this.max
        return value
    }

    calcLeft(value: number): number {
        return (value - this.min) / (this.max - this.min) * 100
    }
}

export class SliderObject extends HTMLElement {
    value: number = 0
    movable: boolean = true

    _down: boolean = false
    parent: SliderTrack

    moveEvent: any
    upEvent: any


    static get observedAttributes(): string[] {
        return ['value', 'movable'];
    }
    attributeChangedCallback(name: string, _oldValue: any, value: any) {
        switch (name) {
            case 'value':
                this.value = +value
                break;
            case 'movable':
                this.movable = !!value
                break;
        }
        this.update()
    }
    connectedCallback() {
        this.parent = this.parentElement as SliderTrack
        this.addEventListener('pointerdown', e => {
            this._down = true
            e.stopPropagation()
            const event = new Event("down")
            this.dispatchEvent(event)
        })
        this.moveEvent = e => {
            if (this._down && this.movable) {

                this.value = this.parent.calcValue(e)
                const event = new Event("change")
                this.dispatchEvent(event)
            }
        }
        this.upEvent = e => {
            this._down = false
        }

        document.addEventListener('pointermove',   this.moveEvent)
        document.addEventListener('pointerup',     this.upEvent)
        document.addEventListener('pointercancel', this.upEvent)

        this.update()
    }
    disconnectedCallback() {
        document.removeEventListener('pointermove',   this.moveEvent)
        document.removeEventListener('pointerup',     this.upEvent)
        document.removeEventListener('pointercancel', this.upEvent)
    }
    update() {
        if (!this.parent) return
        this.style.left = this.parent.calcLeft(this.value) + "%"
    }
}

customElements.define('slider-track', SliderTrack)
customElements.define('slider-object', SliderObject)
