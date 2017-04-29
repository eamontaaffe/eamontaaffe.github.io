import React from 'react';

import Book from './Book';

const BookList = (props) => (
	<div className="BookList">
	{props.books.map((book, i) => 
	    <Book key={i} title={book.title} author={book.author} />
	)}
    </div>
)

export default BookList;
