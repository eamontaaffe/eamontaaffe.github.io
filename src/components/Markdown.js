import React from 'react';
import marked from 'marked';

const Markdown = ({content}) => {
    // Synchronous highlighting with highlight.js
    marked.setOptions({
        highlight: function (code) {
            return require('highlight.js').highlightAuto(code).value;
        }
    });
    
    const inner = {__html: marked(content)} 
    return (
            <div className="content" dangerouslySetInnerHTML={inner}/>
    )
}

export default Markdown;
