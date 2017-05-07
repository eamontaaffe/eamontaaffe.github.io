import React from 'react';
import highlightjs from 'highlight.js';

import 'highlight.js/styles/atom-one-light.css'
import marked, { Renderer } from 'marked';

// Create your custom renderer.
const renderer = new Renderer();
renderer.code = (code, language) => {
    // Check whether the given language is valid for highlight.js.
    const validLang = !!(language && highlightjs.getLanguage(language));
    // Highlight only if the language is valid.
    const highlighted = validLang ? highlightjs.highlight(language, code).value : code;
    // Render the highlighted code with `hljs` class.
    return `<pre><code class="hljs ${language}">${highlighted}</code></pre>`;
};

const Markdown = ({content}) => {
    // Synchronous highlighting with highlight.js
    marked.setOptions({ renderer });
    
    const inner = {__html: marked(content)} 
    return (
            <div className="content" dangerouslySetInnerHTML={inner}/>
    )
}

export default Markdown;
