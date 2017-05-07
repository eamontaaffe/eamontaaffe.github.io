import React from 'react';
import moment from 'moment';

import Markdown from './Markdown';

const Post = (props) => {
    const date = moment(props.date)
    const dateString = date.format('MMMM Do YYYY')
    

    return (
	    <div className="Post">
	    ---<br/>
	    <div className="title">
	    <span className="key">Title:</span> {props.title}
	</div>
	    <div className="date">
	    <span className="key">Date:</span> {dateString}</div>
	    ---<br/>
	    <Markdown content={props.content} />
	    </div>
    )
}

export default Post;
