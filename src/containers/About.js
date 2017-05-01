import React, { Component } from 'react';
import marked from 'marked';
import { connect } from 'react-redux';

import { fetchAbout } from '../actions/'

class About extends  Component {
    componentDidMount() {
	const { dispatch } = this.props
	dispatch(fetchAbout())
    }

    render() {
	return (
		<div
	    className="About"
	    dangerouslySetInnerHTML={{__html: marked(this.props.content)}}
		/>
	)
    }
}

const mapStateToProps = (state) => {
    const { about } = state
    return {
	...about,
    }
}

export default connect(mapStateToProps)(About);
