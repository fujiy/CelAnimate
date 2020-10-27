

export class ContextMenu extends HTMLElement {

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
        document.addEventListener("click", e => {
            this.style.display = "none"
            e.stopPropagation()
        })
    }
}


customElements.define('context-menu', ContextMenu)
