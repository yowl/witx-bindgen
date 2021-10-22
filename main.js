var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
import { Demo, Lang, Config } from './demo.js';
import * as browser from './browser.js';
class Editor {
    constructor() {
        this.input = document.getElementById('input-raw');
        this.language = document.getElementById('language-select');
        this.mode = document.getElementById('mode-select');
        this.files = document.getElementById('file-select');
        this.rustUnchecked = document.getElementById('rust-unchecked');
        this.wasmtimeTracing = document.getElementById('wasmtime-tracing');
        this.wasmtimeAsync = document.getElementById('wasmtime-async');
        this.wasmtimeCustomError = document.getElementById('wasmtime-custom-error');
        this.outputHtml = document.getElementById('html-output');
        this.inputEditor = ace.edit("input");
        this.outputEditor = ace.edit("output");
        this.inputEditor.setValue(this.input.value);
        this.inputEditor.clearSelection();
        this.outputEditor.setReadOnly(true);
        this.inputEditor.setOption("useWorker", false);
        this.outputEditor.setOption("useWorker", false);
        this.generatedFiles = {};
        this.demo = new Demo();
        this.config = null;
        this.rerender = null;
    }
    instantiate() {
        return __awaiter(this, void 0, void 0, function* () {
            const imports = {};
            const obj = {
                log: console.log,
                error: console.error,
            };
            browser.addBrowserToImports(imports, obj, name => this.demo.instance.exports[name]);
            yield this.demo.instantiate(fetch('./demo.wasm'), imports);
            this.config = Config.new(this.demo);
            this.installListeners();
            this.render();
        });
    }
    installListeners() {
        this.inputEditor.on('change', () => {
            this.input.value = this.inputEditor.getValue();
            if (this.rerender !== null)
                clearTimeout(this.rerender);
            this.rerender = setTimeout(() => this.render(), 500);
        });
        this.language.addEventListener('change', () => this.render());
        this.mode.addEventListener('change', () => this.render());
        this.rustUnchecked.addEventListener('change', () => {
            this.config.setRustUnchecked(this.rustUnchecked.checked);
            this.render();
        });
        this.wasmtimeTracing.addEventListener('change', () => {
            this.config.setWasmtimeTracing(this.wasmtimeTracing.checked);
            this.render();
        });
        this.wasmtimeAsync.addEventListener('change', () => {
            let async_;
            if (this.wasmtimeAsync.checked)
                async_ = { tag: 'all' };
            else
                async_ = { tag: 'none' };
            this.config.setWasmtimeAsync(async_);
            this.render();
        });
        this.wasmtimeCustomError.addEventListener('change', () => {
            this.config.setWasmtimeCustomError(this.wasmtimeCustomError.checked);
            this.render();
        });
        this.files.addEventListener('change', () => this.updateSelectedFile());
    }
    render() {
        for (let div of document.querySelectorAll('.lang-configure')) {
            div.style.display = 'none';
        }
        const config = document.getElementById(`configure-${this.language.value}`);
        config.style.display = 'inline-block';
        const witx = this.inputEditor.getValue();
        const is_import = this.mode.value === 'import';
        let lang;
        switch (this.language.value) {
            case "js":
                lang = Lang.Js;
                break;
            case "rust":
                lang = Lang.Rust;
                break;
            case "wasmtime":
                lang = Lang.Wasmtime;
                break;
            case "wasmtime-py":
                lang = Lang.WasmtimePy;
                break;
            case "c":
                lang = Lang.C;
                break;
            case "markdown":
                lang = Lang.Markdown;
                break;
            case "spidermonkey":
                lang = Lang.Spidermonkey;
                break;
            default: return;
        }
        const result = this.config.render(lang, witx, is_import);
        if (result.tag === 'err') {
            this.outputEditor.setValue(result.val);
            this.outputEditor.clearSelection();
            this.showOutputEditor();
            return;
        }
        this.generatedFiles = {};
        const selectedFile = this.files.value;
        this.files.options.length = 0;
        for (let i = 0; i < result.val.length; i++) {
            const name = result.val[i][0];
            const contents = result.val[i][1];
            this.files.options[i] = new Option(name, name);
            this.generatedFiles[name] = contents;
        }
        if (selectedFile in this.generatedFiles)
            this.files.value = selectedFile;
        this.updateSelectedFile();
    }
    showOutputEditor() {
        this.outputHtml.style.display = 'none';
        document.getElementById('output').style.display = 'block';
    }
    showOutputHtml() {
        this.outputHtml.style.display = 'block';
        document.getElementById('output').style.display = 'none';
    }
    updateSelectedFile() {
        if (this.files.value.endsWith('.html')) {
            const html = this.generatedFiles[this.files.value];
            this.outputHtml.innerHTML = html;
            this.showOutputHtml();
            return;
        }
        this.showOutputEditor();
        this.outputEditor.setValue(this.generatedFiles[this.files.value]);
        this.outputEditor.clearSelection();
        if (this.files.value.endsWith('.d.ts'))
            this.outputEditor.session.setMode("ace/mode/typescript");
        else if (this.files.value.endsWith('.js'))
            this.outputEditor.session.setMode("ace/mode/javascript");
        else if (this.files.value.endsWith('.rs'))
            this.outputEditor.session.setMode("ace/mode/rust");
        else if (this.files.value.endsWith('.c'))
            this.outputEditor.session.setMode("ace/mode/c_cpp");
        else if (this.files.value.endsWith('.h'))
            this.outputEditor.session.setMode("ace/mode/c_cpp");
        else if (this.files.value.endsWith('.md'))
            this.outputEditor.session.setMode("ace/mode/markdown");
        else if (this.files.value.endsWith('.py'))
            this.outputEditor.session.setMode("ace/mode/python");
        else
            this.outputEditor.session.setMode(null);
    }
}
(new Editor()).instantiate();
