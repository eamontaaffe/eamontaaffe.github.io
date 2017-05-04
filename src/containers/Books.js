import React, { Component } from 'react';
import { connect } from 'react-redux';

import { fetchBooks } from '../actions';
import BookList from '../components/BookList';

class Books extends Component {

    componentDidMount() {
        const { dispatch } = this.props
        dispatch(fetchBooks())
    }

    render() {
        const { books } = this.props
        return (
	        <div className="Books">
	        <BookList {...books} />
                </div>
        )
    }

}

const mapStateToProps = (state) => {
    const { books } = state;
    return { books }
}

export default connect(mapStateToProps)(Books)
