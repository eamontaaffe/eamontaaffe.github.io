import React from 'react';
import { connect } from 'react-redux';

import BookList from '../components/BookList';

const Books = (props) => (
	<div className="Books">
	<BookList books={props.books} />
    </div>
)

const mapStateToProps = (state) => {
    const { books } = state;
    return {
	books,
    }
}

export default connect(mapStateToProps)(Books)
