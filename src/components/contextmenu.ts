

export class ContextMenu extends HTMLElement {

    connectedCallback() {
        this.style.display = "none"
        this.style.position = "absolute"
        this.parentElement.addEventListener("contextmenu", e => {
            this.style.display = "block"
            const rect = this.parentElement.getBoundingClientRect()
            this.style.left = e.pageX - rect.left + "px"
            this.style.top = e.pageY - rect.top + "px"
            e.preventDefault()
            e.stopPropagation()
        })
        document.addEventListener("click", _ => {
            this.style.display = "none"
        })
    }
}


customElements.define('context-menu', ContextMenu)
