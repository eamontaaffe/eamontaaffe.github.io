import React, { Component } from 'react';
import { connect } from 'react-redux';

import About from '../components/About';
import { fetchAbout } from '../actions/'

class AboutContainer extends  Component {
    componentDidMount() {
	const { dispatch } = this.props
	dispatch(fetchAbout())
    }

    render() {
	return (
		<About {...this.props.about} />
	)
    }
}

const mapStateToProps = (state) => {
    const { about } = state
    return {
	about,
    }
}

export default connect(mapStateToProps)(AboutContainer);
