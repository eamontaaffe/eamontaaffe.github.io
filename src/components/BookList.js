import React from 'react';

import Book from './Book';

const BookList = (props) => (
	<div className="BookList">
	{props.books.map((book, i) => 
			 <Book key={i}
			 {...book} />
	)}
    </div>
)

export default BookList;
