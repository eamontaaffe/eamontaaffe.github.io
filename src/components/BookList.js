import React from 'react';

import Book from './Book';

const BookList = ({ content }) => {
    return (
	    <div className="BookList">
	    {content.map((book, i) => 
			 <Book key={i}
			 {...book} />
	                )}
        </div>
    )
}

export default BookList;
