import React from 'react';

function contentRow(text, width, link) {
    const remainingSpace = (width - 4) - text.length
    const out = `|&nbsp;<a href="${link}" target="_blank">` + text + "</a>&nbsp;".repeat(1+remainingSpace) + "|"
    return out
}

function asciiBook(title, author, link="#", height=16, width=14) {
    const boarderRow = "+" + "-".repeat(width-2) + "+";
    const emptyRow =  "|" + "&nbsp;".repeat(width-2) + "|";
    const maxTextWidth = width - 4;

    const chunksRegex = new RegExp(`.{1,${maxTextWidth}}`,'g')
    const titleChunks = title.match(chunksRegex)
    const authorChunks = author.match(chunksRegex)
    
    var output = []
    output.push(boarderRow)
    output.push(emptyRow)
    output = titleChunks.reduce((acc, text) =>
	acc.concat(contentRow(text, width,link))
    ,output)
    output.push(emptyRow)
    output = authorChunks.reduce((acc, text) =>
	acc.concat(contentRow(text, width, link))
    ,output)
    for (var i = 0; i < height - output.length - 2; i++) {
	output.push(emptyRow)
    }
    output.push(boarderRow)

    const out = output.reduce((acc, row) => acc + row + "<br/>", "")
    return out
}

const Book = ({title, author, link}) => {
    return (
	    <div
	className="Book"
	dangerouslySetInnerHTML={{
	    __html: asciiBook(title, author, link)
	}} />
)}

export default Book;
