
export class TreeGroup extends HTMLElement {

    selected: boolean

    constructor() {
        super()
    }
    static get observedAttributes(): string[] {
        return [];
    }
    attributeChangedCallback(name: string, _oldValue: any, value: any) {
        switch (name) {
            case 'selected':
                this.selected = value
        }
    }
    connectedCallback() {
        this.style.display = "block"
    }
}

export class TreeItem extends HTMLElement {
    selected: boolean

    constructor() {
        super()
    }
    static get observedAttributes(): string[] {
        return ['selected'];
    }
    attributeChangedCallback(name: string, _oldValue: any, value: any) {
        switch (name) {
            case 'selected':
                this.selected = value
        }
    }
    connectedCallback() {
        this.style.display = "block"

        this.addEventListener('click', _ => {
            if (!this.selected) {
                this.selected = true
                const event = new Event("change")
                this.dispatchEvent(event)
            }
        })
    }
}


customElements.define('tree-group', TreeGroup)
customElements.define('tree-item', TreeItem)
