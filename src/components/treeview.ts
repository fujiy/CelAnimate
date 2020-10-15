
export class TreeGroup extends HTMLElement {
    textElement: HTMLElement
    constructor() {
        super()
        this.textElement = document.createElement("div")
    }
    static get observedAttributes(): string[] {
        return ['text'];
    }
    attributeChangedCallback(name: string, _oldValue: any, value: any) {
        switch (name) {
            case "text":
                this.textElement.innerText = value
        }
    }
    connectedCallback() {
        this.appendChild(this.textElement)
        console.log(this)
    }

}

export class TreeItem extends HTMLElement {
    constructor() {
        super()
    }
    static get observedAttributes(): string[] {
        return ['text'];
    }
    attributeChangedCallback(name: string, _oldValue: any, value: any) {
        switch (name) {
            case "text":
                this.innerText = value
        }
    }
}


customElements.define('tree-group', TreeGroup)
customElements.define('tree-item', TreeItem)
