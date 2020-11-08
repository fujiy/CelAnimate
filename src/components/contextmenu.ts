

export class ContextMenu extends HTMLElement {
    clickEvent: any

    connectedCallback() {
        this.style.display = "none"
        this.style.position = "absolute"
        this.parentElement.addEventListener("contextmenu", e => {
            this.style.display = "block"
            const rect = this.parentElement.getBoundingClientRect()
            this.style.left = e.pageX + "px"
            this.style.top = e.pageY + "px"
            e.preventDefault()
            e.stopPropagation()
        })
        this.clickEvent = e => {
            this.style.display = "none"
            e.stopPropagation()
        }
        document.addEventListener("click", this.clickEvent)
    }
    disconnectedCallback() {
        document.removeEventListener("click", this.clickEvent)
    }
}


customElements.define('context-menu', ContextMenu)
