import React from 'react';
import moment from 'moment';
import marked from 'marked';

const Post = (props) => {
    const date = moment(props.date)
    const dateString = date.format('MMMM Do YYYY')
    const content = {__html: marked(props.content)}

    return (
	    <div className="Post">
	    ---<br/>
	    <div className="title">
	    <span className="key">Title:</span> {props.title}
	</div>
	    <div className="date">
	    <span className="key">Date:</span> {dateString}</div>
	    ---<br/>
	    <div className="content" dangerouslySetInnerHTML={content}/>
	    </div>
    )
}

export default Post;
