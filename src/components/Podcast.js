import React from 'react';

function strip(html)
{
   var tmp = document.createElement("div");
   tmp.innerHTML = html;
   return tmp.textContent || tmp.innerText || "";
}


function podcastInnerHTML({name, producer, length, link="#"}){
    const middle = `| <a href=${link} target="_blank">▷</a> | ${name} | ${producer} | ${length} |</br>`
    const top = "┌" + "─".repeat(strip(middle).length-1) + "┐</br>"
    const bottom = "└" + "─".repeat(strip(middle).length-1) + "┘"

    return top + middle + bottom;    
}

const Podcast = ({podcast}) => (
	<div
    className="Podcast"
    dangerouslySetInnerHTML={{
	__html: podcastInnerHTML(podcast)
    }}
    />
)

export default Podcast;
