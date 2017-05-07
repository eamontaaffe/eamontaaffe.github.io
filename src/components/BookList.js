import React from 'react';

import Book from './Book';
import Fetching from './Fetching';

function renderFetching() {
    return (<Fetching />)
}

function renderBooks(content) {
    return (
        content.map((book, i) => 
		    <Book key={i}
		    {...book} />
	           )
    )
}

const BookList = ({ content, isFetching }) => {
    return (
	<div className="BookList">
	    {isFetching ? renderFetching() : renderBooks(content)}
        </div>
    )
}

export default BookList;
