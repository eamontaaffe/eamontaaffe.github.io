import React from 'react';
import marked from 'marked';
import { connect } from 'react-redux';

const About = (props) => (
	<div
    className="About"
    dangerouslySetInnerHTML={{__html: marked(props.about)}}
	/>
)

const mapStateToProps = (state) => {
    const { about } = state
    return {
	about
    }
}

export default connect(mapStateToProps)(About);
