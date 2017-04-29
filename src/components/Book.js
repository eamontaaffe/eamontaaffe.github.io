import React from 'react';
import marked from 'marked';

function contentRow(text, width) {
    const remainingSpace = (width - 4) - text.length
    const out = "|&nbsp;" + text + "&nbsp;".repeat(1+remainingSpace) + "|"
    return out
}

function asciiBook(title, author, link, height=16, width=14) {
    const boarderRow = "+" + "-".repeat(width-2) + "+";
    const emptyRow =  "|" + "&nbsp;".repeat(width-2) + "|";
    const maxTextWidth = width - 4;
    const maxTextLines = 2;

    const chunksRegex = new RegExp(`.{1,${maxTextWidth}}`,'g')
    const titleChunks = title.match(chunksRegex)
    const authorChunks = author.match(chunksRegex)
    
    var output = []
    output.push(boarderRow)
    output.push(emptyRow)
    output = titleChunks.reduce((acc, text) =>
	acc.concat(contentRow(text, width))
    ,output)
    output.push(emptyRow)
    output = authorChunks.reduce((acc, text) =>
	acc.concat(contentRow(text, width))
    ,output)
    for (var i = 0; i < height - output.length - 2; i++) {
	output.push(emptyRow)
    }
    output.push(boarderRow)

    const out = output.reduce((acc, row) => acc + row + "<br/>", "")
    console.log(output)
    return out
}

const Book = ({title, author}) => {
    return (
	    <div
	className="Book"
	dangerouslySetInnerHTML = {{
	    __html: asciiBook(title, author)
	}} />
)}

export default Book;
