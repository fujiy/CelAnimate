
import * as fs from 'fs'
import * as unzipper from 'unzipper'
import * as JSZip from 'node-zip'

const { remote } = require('electron')


export class FileAccesor extends HTMLElement {

    path: string = ""
    data: any

    connectedCallback() {
        Array.from(this.children).forEach(x => {
            switch (x.getAttribute('type')) {
                case "save":
                    x.addEventListener('click', e => {this.save()})
                    break;
                case "open":
                    x.addEventListener('click', e => {this.open()})
                    break;
            }
        })
    }

    async save() {
        if (!this.path) { 
            const r = await  remote.dialog.showSaveDialog(null, {
                defaultPath: '.',
                filters: [
                    {name: 'zip file', extensions: ['zip']}
                ]
            })
            if (r && r.filePath) {
                this.path = r.filePath
            }
        }
        if (!this.path) return

        const zip = new JSZip()
        for (const data of this.data) {
            switch (data.type) {
                case "json":
                    zip.file(data.path, JSON.stringify(data.data))
                    break
                case "png":
                case "jpeg":
                    zip.file(data.path, data.data.split(',')[1],
                             {base64: true})
                    break
            }
        }
        const data = zip.generate({base64:false, compression:'DEFLATE'})
        fs.writeFileSync(this.path, data, 'binary')
    }
    async open() {
        const r = await remote.dialog.showOpenDialogSync(null, {
            properties: ['openFile'],
            defaultPath: '.',
            filters: [
                {name: 'zip file', extensions: ['zip']}
            ]
        })
        if (r && r[0]) {
            this.path = r[0]
            fs.readFile(this.path, 'binary', async (err, data) => {
                if (err) throw err
                const zip = new JSZip().load(data)

                this.data = []

                for (const path in zip.files) {
                    if (path.endsWith('json')) {
                        this.data.push(
                            {path: path,
                             data: JSON.parse(zip.file(path).asText()),
                             type: 'json'
                            })
                    }
                    else if (path.endsWith('png')) {
                        const bytes = zip.file(path).asUint8Array()
                        let binary = '';
                        for (let i = 0; i < bytes.byteLength; i++) {
                            binary += String.fromCharCode(bytes[i]);
                        }
                        this.data[path]
                        this.data.push(
                            {path: path,
                             data: `data:image/png;base64,${btoa(binary)}`,
                             type: 'png'
                            }
                        )
                    }
                }

                const event = new Event("fileload")
                this.dispatchEvent(event)
            })

        }
    }

}


customElements.define('file-accesor', FileAccesor)
